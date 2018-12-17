# Creazione dataset nested con le metacomunità per ogni anno. Si ammette anche il filtro per Specie Faunistica

create_metacommunity <- function(data, faunistic = F, fau_label = NULL, index = "KG_KM"){
  
  # faunistic è True se si vuole creare la mc per una sola specie faunistica. Se False, la mc è completa per tutte le specie.
  # index indica la variabile rispetto alla quale creare la mc. KG_KM o N_KM
  if(faunistic){
    nrow <- length(unique(data[data$FAUNISTIC_CATEGORY == fau_label,]$SPECIE))
    names <- unique(data[data$FAUNISTIC_CATEGORY == fau_label,]$SPECIE)
    mat_entropart <- data %>%
      filter(FAUNISTIC_CATEGORY %in% fau_label) %>%
      group_by(YEAR) %>% nest() %$% data %>%
      map(., `[[`, index) %>%
      map(., matrix, nrow = nrow, byrow = FALSE) %>%
      map(., set_rownames, names) %>%
      set_names(seq(range(data$YEAR)[1], range(data$YEAR)[2]))
    
    # idx_list <- map(mat_entropart, .f = function(x) return(x == 0))
    # mat_entropart <- map2(mat_entropart, idx_list, function(x,y){
    #   x[y] <- 0.00001
    #   return(x)
    # })
    
  } else {
    mat_entropart <- data %>%
      group_by(YEAR) %>% nest() %$% data %>%
      map(., `[[`, index) %>%
      map(., matrix, nrow = length(unique(data$SPECIE)), byrow = FALSE) %>%
      map(., set_rownames, unique(data$SPECIE)) %>%
      set_names(seq(range(data$YEAR)[1], range(data$YEAR)[2])) %>% 
      map2(., by(medits_ALL$HAUL_NUMBER, medits_ALL$YEAR, unique, simplify = TRUE) %>% map(as.character), set_colnames)
      
  }
  
  return(mat_entropart)
}

# Andamento dell'entropia/diversità nel tempo e al variare di q. Si ammette anche il filtro per Specie Faunistica

Tsallis_years <- function(MC, type, plot = T){
  FUN <- match.fun(type) 
  Richness <- map(.x = map(MC, `[[`, "Ps"), .f = FUN, q = 0) %>% map(., as.numeric) %>% unlist() %>% log()
  Shannon <-  map(.x = map(MC, `[[`, "Ps"), .f =FUN, q = 1) %>% map(., as.numeric) %>% unlist() %>% log()
  Simpson <- map(.x = map(MC, `[[`, "Ps"), .f = FUN, q = 2) %>%  map(., as.numeric) %>% unlist() %>% log()
  Generalized <- map(.x = map(MC, `[[`, "Ps"), .f = FUN, q = 3) %>% map(., as.numeric) %>% unlist() %>% log()
  
  T_mat <- bind_cols(`Richness (q = 0)` = Richness, `Shannon (q = 1)` = Shannon, `Simpson (q = 2)` = Simpson, 
                     `Generalized (q = 3)` = Generalized) %>% 
      mutate(Year = as.numeric(names(Richness))) %>%
      gather(Entropy, Value, `Richness (q = 0)`:`Generalized (q = 3)`) %>%
      mutate(Entropy = factor(Entropy, levels = c("Richness (q = 0)", "Shannon (q = 1)", "Simpson (q = 2)", 
                                                  "Generalized (q = 3)")))
  
  if(plot){
    p <- T_mat %>%
      ggplot + aes(x = Year, y = Value) + geom_line() + theme_light() + labs(y = "Entropy") +
      facet_wrap(~Entropy, scales = "free") + scale_x_continuous(limits = range(names(MC) %>% as.numeric),
                                                                 breaks = seq(1994, 2017)) +
      theme(axis.text.x = element_text(angle = 90))
    return(p)
  } else {
    return(T_mat)
  }
  
}


# Own Metacommunity and Preprocess.MC

Preprocess_MC_mine <- function(Nsi, Wi){
  Ni <- colSums(Nsi)
  #Psi <- Nsi %*% diag(1/Ni)
  # Se in una comunità non è stato pescato nessun pesce allora la probabilità di pescare la specie s-ma in quella comunità è 0(?)
  #Psi[is.nan(Psi)] <- 0
  N <- sum(as.numeric(Ni))
  Psi <- Nsi/N
  dimnames(Psi) <- dimnames(Nsi)
  Ps <- rowSums(Nsi/N)
  Ns <- rowSums(Nsi)
  MC <- list(Nsi = Nsi, Ns = Ns, Ni = Ni, N = N, Psi = Psi, 
             Wi = Wi, Ps = Ps, Nspecies = dim(Nsi)[1], Ncommunities = dim(Nsi)[2], 
             SampleCoverage = Coverage(Ns), 
             SampleCoverage.communities = apply(Nsi, 2, Coverage, CheckArguments = FALSE))
  class(MC) <- "MetaCommunity"
  return(MC)
}

MetaCommunity_mine <- function(Abundances, Weights = rep(1, ncol(Abundances))){
  # E' necessario che la matrice delle abbondanze sia di dimensioni SxI e che abbia i nomi di riga uguali ai nomi delle specie
  # Non sono necessari i nomi delle colonne
  # Nspecie <- numero di specie diverse overall
  Nspecies <- nrow(Abundances)
  # Ncommunities <- numero di comunità
  Ncommunities <- ncol(Abundances)
  # SpeciesNames <- I nomi delle specie considerate
  SpeciesNames <- as.factor(rownames(Abundances))
      
  if (is.null(colnames(Abundances))) {
    colnames(Abundances) <- paste("P", 1:(ncol(Abundances)), 
                                  sep = "")
  }
  
  Nsi <- as.matrix(Abundances)
  dimnames(Nsi)[[1]] <- SpeciesNames
  if (is.vector(Weights)) {
    Wi <- Weights/sum(Weights)
  } else {
    Wi <- Weights$Weights/sum(Weights$Weights)
  }
  names(Wi) <- colnames(Nsi)
  Preprocess_MC_mine(Nsi, Wi)
}

# Summary statistics
do_table_stats <- function(data){
  data %>% summarise(Min = min(Value),
                     Q1 = quantile(Value, 0.25),
                     Median = median(Value),
                     Mean = mean(Value),
                     Q3 = quantile(Value, 0.75),
                     IQR = Q3-Q1,
                     Std.dev = sd(Value),
                     CV = Std.dev/abs(Mean),
                     Max = max(Value),
                     Kurtosis = kurt(Value),
                     Skewness = skew(Value))
}

# Profiling

profile_def <- function(MC){
  
  profile_list <- list()
  for(i in names(MC)){
    profile_list[[i]] <- DivProfile(q.seq = seq(0,3,0.1), MC = MC[[i]],
                                    Biased = TRUE, Correction = "Best")
  }
  
  Total_div <- bind_rows(Reduce(f = "cbind", x = profile_list %>% map(`[[` ,"TotalAlphaDiversity")) %>% as_tibble() %>% 
                           set_colnames(1994:2014) %>% gather(., "Year", "Diversity") %>% mutate(Type = "Alpha"),
                         Reduce(f = "cbind", x = profile_list %>% map(`[[` ,"TotalBetaDiversity")) %>% as_tibble() %>% 
                           set_colnames(1994:2014) %>% gather(., "Year", "Diversity") %>% mutate(Type = "Beta"),
                         Reduce(f = "cbind", x = profile_list %>% map(`[[` ,"GammaDiversity")) %>% as_tibble() %>% 
                           set_colnames(1994:2014) %>% gather(., "Year", "Diversity") %>% mutate(Type = "Gamma")) %>%
    mutate(q = rep(seq(0,3,0.1), 21*3))
  img <- image_graph(350, 340)
  datalist <- split(Total_div, Total_div$Year)
  out <- lapply(datalist, function(data){
    p <- data %>% ggplot + geom_line(aes(x = q, y = Diversity)) + 
      facet_wrap(~Type, nrow = 3, scales = "free_y") + ggtitle(data$Year) + theme_bw()
    print(p)
  })
  invisible(dev.off())
  return(img)
}


# Funzione calcolo dissimilarità normalizzate
Dissimilarity <- function(data,i,j){
  
  a <- data[,i]
  b <- data[,j]
  
  tot_a <- sum(a > 0)
  tot_b <- sum(b > 0)
  tot_comm <- sum((a>0) & (b>0))
  diss_idx <- 1 - tot_comm/(tot_a+tot_b-tot_comm)
  return(diss_idx)
}
Dissimilarity <- Vectorize(Dissimilarity, vectorize.args=list("i","j"))
Dissimilarity_list <- function(data){
  N <- ncol(data)
  mat_out <- outer(X = 1:N,Y = 1:N, FUN = Dissimilarity, data=data)
  dimnames(mat_out) <- list(1:N, 1:N)
  return(mat_out)
}


# Trova i centroidi di un set di coordinate
find_centroid <- function(data){
  subset_coord <- data %>% distinct(X,Y) %>% sp::SpatialPoints()
  cc <- rgeos::gCentroid(subset_coord) %>% sp::coordinates()
  return(cc)
}


# Assumere che tutte le coordinate siano lunghe 6 (tipo stringa)
# La prima coppia sono i gradi e me li tengo, poi i minuti e i secondi (seconda e terza coppia) vanno divisi (insieme, un numero unico) per 60. In seguito sommare i gradi a quanto ottenuto
coord_dec_degree_string <- function(coord){
  coord_string <- as.character(coord) %>% str_split("[[:punct:]]", simplify = TRUE) #%>% Reduce("rbind", .)
  ncharact <- apply(coord_string,2,nchar) %>% c()
  if(ncharact[1] < 4 & !is.na(ncharact[1])){coord_string[1] <- paste(rep("0", 4-ncharact[1]), coord_string[1], sep = "")}
  if(ncharact[2] < 2 & !is.na(ncharact[2])){coord_string[2] <- paste(coord_string[2], rep("0", 2-ncharact[2]), sep = "")}
  return(paste0(c(coord_string), collapse = ""))
}

coord_dec_degree <- function(coord){
  coord %<>% as.list()
  coord %<>% map(coord_dec_degree_string)
  grades <- map_chr(coord, str_sub, start = 1, end = 2) %>% as.numeric()
  minutes <- map(coord, str_sub, start = 3, end = 4)
  seconds <- map(coord, str_sub, start = 5, end = 6)
  new_coord <- grades + map2_chr(minutes, seconds, paste, sep = ".") %>% as.numeric() %>% divide_by(60)
  return(new_coord)
}
