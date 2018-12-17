# Comm by SPECIE
#create_mc_specie <- function(data, index = "KG_KM"){
  data %>%
  group_by(SPECIE) %>% nest() %$% data %>%
  map(., `[[`, index) %>%
  map(., matrix, nrow = length(unique(data$HAUL_NUMBER)), byrow = FALSE) %>%
  map(., set_rownames, unique(data$HAUL_NUMBER)) %>%
  set_names(unique(data$SPECIE)) %>% 
  map2(., by(medits_ALL$YEAR, medits_ALL$SPECIE, unique, simplify = TRUE) %>% map(as.character), set_colnames)
}

# Centered matrix
# centering <- function(data){
#   scale(data, scale = FALSE)
# }

# Scalar product matrix
scal_prod_mat <- function(data){
  data%*%t(data)
}

# RV coefficient
RV_coef <- function(data1, data2){
  num <- t(data1)%*%data2
  tr_num <- psych::tr(num)
  den1 <- psych::tr(t(data1)%*%data1)
  den2 <- psych::tr(t(data2)%*%data2)
  RV <- tr_num/(sqrt(den1*den2))
  return(RV)
}
RV_coef <- Vectorize(RV_coef, vectorize.args = "data2")

#mat_entropart_new <- create_mc_specie(data = medits_ALL)
mat_entropart %<>% map(t)
#mat_entropart_centered <- map(mat_entropart , centering)
scalar_product_matrix_list <- map(mat_entropart, scal_prod_mat)

RV_mat <- matrix(NA, nrow = length(scalar_product_matrix_list), 
                 ncol = length(scalar_product_matrix_list))
RV_mat[nrow(RV_mat), ncol(RV_mat)] <- 1
for(i in 1:(length(scalar_product_matrix_list)-1)){
  print(i)
  RV_mat[i,i] <- 1 
  RV_mat[i, (i+1):ncol(RV_mat)] <- RV_coef(data1 = scalar_product_matrix_list[[i]],
                         data2 = scalar_product_matrix_list[-(1:i)])
}
RV_mat[lower.tri(RV_mat)] <- RV_mat[upper.tri(RV_mat)]
colnames(RV_mat) <- rownames(RV_mat) <- 1:24 %>% as.character()
corrplot::corrplot(RV_mat, method = "square", tl.col = "black", tl.srt = 45,
                   tl.cex = .8, col = RColorBrewer::brewer.pal(8, "YlOrRd"), order = "hclust", 
                   addCoef.col = "black", type = "upper", number.cex = .5)

# PCA of the cosine matrix
Cosine_mat <- eigen(RV_mat, symmetric = TRUE)
G <- Cosine_mat$vectors%*%diag(sqrt(Cosine_mat$values))
plot(G[,1:2], type = "n")
text(G[,1:2], labels = 1994:2017 %>% as.character())

## Goodness of the compromise
Cosine_mat$values[1]/sum(Cosine_mat$values)

# Compromise
# Weights are calculated by normalizing the values of the first eigenvector 
compromise <- function(PCA_out, S){
  weights_vec <- PCA_out$vectors[, 1]/sum(PCA_out$vectors[, 1])
  compromise_mat <- matrix(0, ncol = ncol(S[[1]]), nrow = nrow(S[[1]]))
  for(i in 1:length(S)){
    compromise_mat <- compromise_mat + weights_vec[i]*S[[i]]
  }
  return(list(w = weights_vec, S_plus = compromise_mat))
}

comp_out <- compromise(Cosine_mat, scalar_product_matrix_list)

plot(comp_out$w, Cosine_mat$values, type = "n")
points(comp_out$w, Cosine_mat$values, pch = 20)
text(comp_out$w, Cosine_mat$values, labels = 1994:2017 %>% as.character)

#comp_out$S_plus

# PCA of the compromise
pca_comp <- eigen(comp_out$S_plus)
pca_comp$values[1]/sum(pca_comp$values)
Fmat <- pca_comp$vectors%*%diag(sqrt(pca_comp$values))
plot(Fmat[,1:2], pch = 20, type = "n")
text(Fmat[,1:2], labels = rownames(mat_entropart$`1994`), cex = .9, 
     col = c("firebrick", "darkgreen","grey")[fac_haul])

p <- ggplot(data = bind_cols(Fmat[,1:2] %>% as_tibble(),
                        medits_ALL %>% distinct(HAUL_NUMBER, DEPTH_CLASS) %>% na.omit()), 
       aes(x = V1, y = V2, colour = DEPTH_CLASS)) +
  #scale_x_continuous(limits = c(-2, 7)) + scale_y_continuous(limits = c(-10,10)) +
  geom_text(aes(label = HAUL_NUMBER), size = 2) + theme_bw() 
plotly::ggplotly(p)

# projections
proj_mat <- pca_comp$vectors%*%diag((pca_comp$values)^(-0.5))
proj_mat_all <- comp_out$S_plus%*%proj_mat
# plot(proj_mat_all[,1:2], pch = 20, type = "n")
# text(proj_mat_all[,1:2], labels = rownames(mat_entropart_centered$`1994`), cex = .9,
#      col = c("firebrick", "darkgreen","grey")[fac_haul])

proj_mat_list <- list()
for(i in 1:length(scalar_product_matrix_list)){
  proj_mat_list[[i]] <- scalar_product_matrix_list[[i]]%*%proj_mat
}

plot(x = Fmat[,1], y = Fmat[,2], pch = 20, type = "n", xlim = c(-20, 20),ylim = c(-20, 20))
text(x = Fmat[,1], y = Fmat[,2], labels = rownames(mat_entropart$`1994`), cex = .9, 
     col = c("firebrick", "darkgreen","grey")[fac_haul])
names <- 1994:2017 %>% as.character()
for(i in 1:length(proj_mat_list)){
  text(proj_mat_list[[i]][,1],proj_mat_list[[i]][,2], labels = names[i]) 
}
