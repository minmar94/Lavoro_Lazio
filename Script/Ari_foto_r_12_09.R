getwd()
setwd("D:/ECOBIO/TESI/Foto")

###carico la tabella su rstudio###
tab_esca_R <- read.csv("D:/ECOBIO/TESI/Foto/tab_esca_R.csv", sep=";")
View(tab_esca_R)

###chiamo la tabella "ari_ph" per lavorarci su r###
ari_ph=read.csv("D:/ECOBIO/TESI/Foto/tab_esca_R.csv", sep=";")
View(ari_ph)


#categorie, vedo se ho scritto tutto bene
levels(ari_ph$gruppi)


##barplot con ggplot2 ma prima mii ricavo media, sd e se

tab_esca_R2 <- read.csv("D:/ECOBIO/TESI/Foto/tab_esca_R2.csv", sep=";")
View(tab_esca_R2)

###chiamo la tabella "ari_ph2" per lavorarci su r###
ari_ph2=read.csv("D:/ECOBIO/TESI/Foto/tab_esca_R2.csv", sep=";")
View(ari_ph2)
summary(ari_ph2)
head(ari_ph2)

###barplot per sito###
library(ggplot2)

ggplot(data = ari_ph2, aes(x = interaction(ari_ph2$sito,ari_ph2$gruppi), y = ari_ph2$cop.media, fill =ari_ph2$gruppi)) + 
  facet_grid(~ari_ph2$sito, scales = "free_x", space = "free_x", switch = "x") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank()) +
  xlab("Sito") + 
  ylab("Copertura media") +
  labs(fill = "Gruppi") +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=ari_ph2$cop.media+ari_ph2$se, ymax=ari_ph2$cop.media+ari_ph2$se), width=.6, position=position_dodge(.9)) +
  geom_linerange(aes(ymin = ari_ph2$cop.media, ymax = ari_ph2$cop.media+ari_ph2$se),  position=position_dodge(.9)) +
  scale_y_sqrt(breaks = c(2.5, 10, 20, 30, 40, 50, 60, 70))

###barplot per gruppi
library(ggplot2)
ggplot(data = ari_ph2, aes(x = ari_ph2$gruppi, y = ari_ph2$cop.media, fill = ari_ph2$sito)) +
  xlab("Gruppi") + 
  ylab("Copertura media") +
  labs(fill = "Sito") +
  geom_bar(stat ="identity", width= 0.75, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin=ari_ph2$cop.media+ari_ph2$se, ymax=ari_ph2$cop.media+ari_ph2$se), width=.6, position=position_dodge(.9)) +
  geom_linerange(aes(ymin = ari_ph2$cop.media, ymax = ari_ph2$cop.media+ari_ph2$se),  position=position_dodge(.9)) +
  scale_y_sqrt(breaks = c(2.5, 10, 20, 30, 40, 50, 60, 70))



###SIMPER: Similarity Percentage analysis. Discriminating species between two groups using 
#Bray-Curtis dissimilarities##


##non riesco con reshape, lo faccio manualmente su excel

library(vegan)
##richiamo manualmente da excel
ari_phcat=read.csv("D:/ECOBIO/TESI/Foto/Tab_esca_wide.csv", sep=";")
View(ari_phcat)
summary(ari_phcat)

ari_phenv=read.csv("D:/ECOBIO/TESI/Foto/Tab_esca_env.csv", sep=";")
View(ari_phenv)
summary(ari_phenv)

require(vegan)
data(ari_phcat)
data(ari_phenv)
sim1 <- (sim <- with (ari_phenv, simper (ari_phcat, Sito)))
summary(sim1)
sim1
lapply(sim1,FUN=function(x){x$overall})



###PERMANOVA: non-parametric multivariate statistical test.Permutational Multivariate Analysis of Variance
#il PERMANOVA è usato per confrontare gruppi 
#di oggetti e testare l'ipotesi nulla che i centroidi e la dispersione dei gruppi come definiti dallo spazio 
#di misura siano equivalenti per tutti i gruppi. Un rifiuto dell'ipotesi nulla significa che il centroide 
#e / o la diffusione degli oggetti è diverso tra i gruppi.

library(vegan)

list.files()
cat_tot=read.table("D:/ECOBIO/TESI/Foto/Tab_esca_wide.csv",header=T,sep=";")
env_tot=read.table("D:/ECOBIO/TESI/Foto/Tab_esca_env.csv",header=T,sep=";")
adonis(cat_tot ~ Sito,data=env_tot,permutations=999)
View(adonis)
