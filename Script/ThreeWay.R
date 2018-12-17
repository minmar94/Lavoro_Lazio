require(ThreeWay)
require(tidyverse)

load("Dati Medits/Medits_Join_1994to2017.RData")

mat_entropart %<>% map(t)
X_A <- Reduce(x = mat_entropart, f = "cbind")
colnames(X_A) <- paste(colnames(X_A), rep(1994:2017, each = 323), sep = ".")

# Parafac
laba <- rownames(X_A)
labb <- colnames(mat_entropart$`1994`)
labc <- 1994:2017 %>% as.character()

X_A_CP <- CP(data = X_A, laba = laba, labb = labb, labc = labc)

# Non centrare i dati perché qui lo 0 non è il punto neutrale (bensì l'assenza del pesce)
# Se non si normalizzasse rispetto alle specie/ allora si vedrebbe l'evoluzione dell'unità rispetto al suo valor medio
# Particolare risultato per ENGR-ENC e DASI-PAS (si mangiano tutta l'informazione)
XA_T3 <- T3(data = X_A, laba = laba, labb = labb, labc = labc)
jointplotgen(XA_T3$H, XA_T3$A, XA_T3$B, XA_T3$C, 2, 2, laba, labb, labc)

# Impilo le matrici di abbondanza
mat_impilata <- Reduce("rbind", mat_entropart)
rownames(mat_impilata) <- paste(laba, rep(labc, each = 40), sep = ".")

require(ade4)
mat_impilata2 <- mat_impilata[, apply(mat_impilata, 2, var) < 0.0001]
det(cor(mat_impilata))
cor(mat_impilata) %>% corrplot::corrplot()
pca_imp <- dudi.pca(scale(mat_impilata), scannf = F, nf = 10)
scatter(pca_imp, clab.row = 0.4, clab.col = 0.2)

apply(mat_impilata, 2, var)


datapca_solounanno <- mat_entropart$`2017` %>% t
apply(datapca_solounanno, 2, var) %>% round(5)

datapca_solounanno <- scale(datapca_solounanno)

pca2017 <- dudi.pca(datapca_solounanno, scannf = FALSE, nf = 10)

scatter(pca2017)
summary(pca2017)
