# Load packages
require(ThreeWay)
require(tidyverse)
# Load data
load("Dati Medits/mat_entropart.RData")

# Transpose abundance matrices
mat_entropart %<>% map(t)
# Matricization
X_A <- Reduce(x = mat_entropart, f = "cbind")
colnames(X_A) <- paste(colnames(X_A), rep(names(mat_entropart), 
                                          each = ncol(mat_entropart[[1]])), sep = ".")

# Parafac
laba <- rownames(X_A)
labb <- colnames(mat_entropart$`1994`)
labc <- names(mat_entropart) %>% as.character()

X_A_CP <- CP(data = X_A, laba = laba, labb = labb, labc = labc)
# X_A_CP$B %>% as.data.frame %>% rownames_to_column("Specie") %>% as_tibble %>% arrange(desc(Comp.1), Comp.2, Comp.3, Comp.4)
# X_A_CP$A %>% as.data.frame %>% rownames_to_column("Sito") %>% as_tibble %>% arrange(desc(Comp.1), Comp.2, Comp.3, Comp.4)
# X_A_CP$C %>% as.data.frame %>% rownames_to_column("Anno") %>% as_tibble %>% arrange(desc(Comp.1), Comp.2, Comp.3, Comp.4)
# Non centrare i dati perché qui lo 0 non è il punto neutrale (bensì l'assenza del pesce)
# Se non si normalizzasse rispetto alle specie/ allora si vedrebbe l'evoluzione dell'unità rispetto al suo valor medio
# Particolare risultato per ENGR-ENC e DASI-PAS (si mangiano tutta l'informazione)

# Tucker3
XA_T3 <- T3(data = X_A, laba = laba, labb = labb, labc = labc)
# Vedere articolo per matrici sparse con Tucker2 e articolo di Klein su come normalizzare

# Impilo le matrici di abbondanza
# mat_impilata <- Reduce("rbind", mat_entropart)
# rownames(mat_impilata) <- paste(laba, rep(labc, each = 40), sep = ".")
# 
# require(ade4)
# mat_impilata2 <- mat_impilata[, apply(mat_impilata, 2, var) < 0.0001]
# det(cor(mat_impilata))
# cor(mat_impilata) %>% corrplot::corrplot()
# pca_imp <- dudi.pca(scale(mat_impilata), scannf = F, nf = 10)
# scatter(pca_imp, clab.row = 0.4, clab.col = 0.2)
# 
# apply(mat_impilata, 2, var)
# 
# 
# datapca_solounanno <- mat_entropart$`2017` %>% t
# apply(datapca_solounanno, 2, var) %>% round(5)
# 
# datapca_solounanno <- scale(datapca_solounanno)
# 
# pca2017 <- dudi.pca(datapca_solounanno, scannf = FALSE, nf = 10)
# 
# scatter(pca2017)
# summary(pca2017)
