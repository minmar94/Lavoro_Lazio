getwd()
setwd("D:/ECOBIO/TESI/Stat")

###carico la tabella su rstudio###
lista_specie_R <- read.csv("D:/ECOBIO/TESI/Stat/lista_specie_R.csv", sep=";")
View(lista_specie_R)

###chiamo la tabella "ari" per lavorarci su r###
ari=read.csv("D:/ECOBIO/TESI/Stat/lista_specie_R.csv", sep=";")
View(ari)
summary(ari)

###la funz. levels provides access to the levels attribute of a variable. 
#Voglio vedere le categorie di taxa, quindi tutti i taxa che ho in tabella.
#(vedo pure se li ho scritti tutti bene)###
levels(ari$taxa)


###decido di raggruppare i livelli di taxa in macrocategorie,
#più o meno in phyla. 
#Per creare una nuova variabile carico il pacchetto "car"###
library(car)

ari <- within(ari, {
  gruppi <- Recode(taxa, 
   'c("Amphipoda","Cumacea","Decapoda","Isopoda","Mysidacea","Tanaidacea")="Crustacea"; c("Bivalvia","Gastropoda","Scaphopoda","Polyplacophora")="Mollusca"; c("Crinoidea","Echinoidea","Ophiuroidea")="Echinodermata"',
   as.factor=TRUE)
})
View(ari)

levels(ari$gruppi)

###Ggplot2 produce grafici a partire da un set di dati e prevede componenti separati che possono 
#essere combinati in modi diversi (Grammar of Graphics), consentendo la creazione di grafici personalizzati.
#I grafici sono per il resto personalizzabili in modo estremamente flessibile, dal sistema di coordinate, 
#fino a forme specifiche per rappresentare i dati. Posso però essere complicati da realizzare, rispetto 
#a quelle incluse nella distribuzione standard di R, nei pacchetti graphics e lattice. ggplot2 ha una sintassi sua.
#la costruzione dei grafici è più flessibile e potente in ggplot2, e i grafici sono più gradevoli esteticamente.

require("ggplot2")

###boxplot per SITO con DENSITà

bp_sito<- ggplot(data = ari, aes(x = interaction(ari$sito,ari$gruppi), y = log1p(ari$dens),
                                 fill =ari$gruppi)) + 
  facet_grid(~ari$sito, scales = "free_x", space = "free_x", switch = "x") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank()) +
  labs(fill = "Gruppi",x= "Sito", y= "Log(Dens)") +
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.5) + 
  geom_boxplot(position = position_dodge(width = 0.9)) 

bp_sito
summary(bp_sito)

#https://stackoverflow.com/questions/38101512/the-same-width-of-the-bars-in-geom-barposition-dodge#

###boxplot PER GRUPPI con densità

ggplot(data = ari, aes(x = ari$gruppi, y = log1p(ari$dens), fill =ari$sito)) + 
  xlab("Gruppi") + 
  ylab("Log(Dens)") +
  labs(fill = "Sito") +
  stat_boxplot(geom = "errorbar", position = position_dodge2(width = 1.2, preserve = "single")) + 
  geom_boxplot(position = position_dodge2(width = 1.2, preserve = "single"))



###SIMPER CON DENSITA': Similarity Percentage analysis. Discriminating species between two groups using 
#Bray-Curtis dissimilarities##

library(vegan)

ari_wide1=read.csv("D:/ECOBIO/TESI/Stat/lista_specie_wide1.csv", sep=";")
View(ari_wide1)

ari_siti=read.csv("D:/ECOBIO/TESI/Stat/lista_specie_siti.csv", sep=";")
View(ari_siti)
summary(ari_siti)

sim1<-(sim <- with(ari_siti,simper(ari_wide1,sito)))
summary(sim1)
sim1
lapply(sim1,FUN=function(x){x$overall})


###ora devo fare una serie di cose per cui è necessario trasformare la mia tabella da long to wide. 
#Uso il pacchetto "reshape2":Flexibly restructure and aggregate data 
#using just two functions: melt and 'dcast' (or 'acast'). 

library(reshape2)

ari2<- dcast(ari, sito ~ specie, value.var="dens", fun=sum)
View(ari2)


#per prima cosa sposto la prima colonna (Siti) come nome delle righe (le funzioni del pacchetto 
#vegan vogliono la matrice di comunità fatta cosi)
ari3 <- data.frame(ari2[,-1], row.names=ari2[,1])
View(ari3)


###CLUSTER###
#insieme di tecniche di analisi multivariata dei dati x selezionare e raggruppare elementi omogenei 
#in un insieme di dati. Le tecniche di clustering si basano su misure relative alla somiglianza 
#tra gli elementi. In molti approcci questa similarità, o meglio, dissimilarità, è concepita in termini 
#di distanza in uno spazio multidimensionale. 
#una partizione ottenuta mediante un algoritmo di clustering è a tutti gli effetti un descrittore
#aggiuntivo (e sintetico) dell'insieme di oggetti in esame. L'appartenenza ad un cluster, infatti, 
#se codificata in maniera appropriata, può essere utilizzata come una variabile di sintesi 
#per ulteriori elaborazioni dell'informazione disponibile.

#Molti metodi multivariati sono sensibili all'abbondanza totale in un campione, quindi dovremmo 
#probabilmente convertire queste stime di abbondanza assoluta in una stima relativa dell'abbondanza

apply(ari3, 1, sum)

# Trasforma le abbondanze in abbondaze realtive  dividendo ciascun valore per l' abbondanza TOTALE
#DECOSTAND: The function provides some popular (and effective) standardization methods for 
#community ecologists. total: divide by margin total.

require(vegan)

comm <- decostand (ari3, method = "total")

apply(comm, 1, sum)
comm[1:5, 1:5]

#calculate BRAY-CURTIS distance among samples.è una statistica utilizzata per quantificare 
#la dissimilarita compositiva tra due siti diversi, basata sui conteggi in ciascun sito.
#La dissimilarita di Bray-Curtis è limitata tra 0 e 1, dove 0 significa che i due siti hanno la stessa 
#composizione (cioè condividono tutte le specie), e 1 significa che i due siti non condividono alcuna specie.
comm.bc.dist <- vegdist(comm, method = "bray")

# cluster communities using AVERAGE-LINKAGE algorithm.-> cluster gerarchico
#1. Collegamento singolo :in ciascuna fase del processo combiniamo i due cluster con la più piccola 
#distanza di collegamento singolo.
#2. Link completo :in ciascuna fase del processo combiniamo i due cluster che hanno la più piccola 
#distanza completa di collegamento.
#3. Collegamento medio : nel AVERAGE LINKAGE , definiamo la distanza tra due cluster come la distanza
#media tra i punti dati nel primo cluster e i punti dati nel secondo cluster.in ciascuna fase del 
#processo combiniamo i due cluster che hanno la distanza di collegamento media più piccola.
#4. Metodo Centroide : nel metodo centroide , la distanza tra due cluster è la distanza tra i due 
#vettori medi dei cluster. In ogni fase del processo combiniamo i due cluster che hanno la distanza 
#del centroide più piccola. 


comm.bc.clust <- hclust(comm.bc.dist, method = "average")
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity")


##con l'argomento "h" gli dici dove tagliare i cluster secondo i valori di dissimilarità, 
#in quesrto caso con 0.8 ottieni i 3 cluster che identificano poi i 5 siti
rect.hclust(comm.bc.clust, h=0.8, border = c("red","green","blue"))


###DCA: Detrended Correspondence Analysis###Analisi della corrispondenza indotta###
#tecnica statistica multivariata usata dagli ecologisti per trovare i principali fattori o gradienti in 
#matrici di dati grandi, ricche di specie ma solitamente sparse che tipizzano i dati della comunità 
#ecologica.Utilizzato per sopprimere gli artefatti inerenti alla >p. delle altre analisi multivariate 
#quando vengono applicati ai dati del gradiente.
#lo faccio con "decorana": Esegue analisi di corrispondenza detratte e analisi della corrispondenza 
#reciproca di base o di corrispondenza ortogonale.
#https://www.davidzeleny.net/anadat-r/doku.php/en:ca_dca#


DCA <- decorana(ari3)
summary(DCA)

#cepnames abbrevia i nomi delle specie.

abb.names <- make.cepnames(names(ari3))
stems <- colSums(ari3)

plot(DCA, dis="sp", type="n")
#qui ho messo al posto di mod dca, ma non so se è giusto
sel <- orditorp(DCA, dis="sp", lab=abb.names, priority=stems, pcol = "red",pch="+")
##evidenzio i siti con ORDISPIDER:Functions to add convex hulls, "spider" graphs, ellipses 
#or cluster dendrogram to ordination diagrams. 
#Function ordispider draws a `spider' diagram where each point is connected to the group centroid 
#with segments. Weighted centroids are used in the correspondence analysis methods cca and 
#decorana or if the user gives the weights in the call. If ordispider is called with cca or rda result 
#without groups argument, the function connects each `WA' scores to the correspoding `LC' score.
for (i in seq (1, 5)) ordispider (DCA, groups = as.numeric(ari2$sito), show.groups = i, col = i, label = T)
summary(DCA)
DCA


DCA <- decorana(ari5)
summary(DCA)
DCA
plot(DCA)
rbind(
  SD=sqrt(eigs),
  Proportion=eigs/sum(eigs),
  Cumulative=cumsum(eigs/sum(eigs)))



plot(DCA, dis="sp", type="n")
#qui ho messo al posto di mod dca, ma non so se è giusto
sel <- orditorp(DCA, dis="sp", lab=abb.names, priority=stems, pcol = "red",pch="+")
##evidenzio i siti con ORDISPIDER:Functions to add convex hulls, "spider" graphs, ellipses 
#or cluster dendrogram to ordination diagrams. 
#Function ordispider draws a `spider' diagram where each point is connected to the group centroid 
#with segments. Weighted centroids are used in the correspondence analysis methods cca and 
#decorana or if the user gives the weights in the call. If ordispider is called with cca or rda result 
#without groups argument, the function connects each `WA' scores to the correspoding `LC' score.
for (i in seq (1, 15)) ordispider (DCA, groups = as.numeric(ari5_1$sito), show.groups = i, col = i, label = T)



###INDICI BIOTICI SINTETICI

#Shannon per replica
library(vegan)
Hs_rep <- diversity(ari_pca1)
View(Hs_rep)

#Shannon per sito
Hs_sito <- diversity(ari3)
View(Hs_sito)

#Evenness Pielou per replica
E_rep <- Hs_rep/log(specnumber(ari_pca1))
View(E_rep)

#Evennes Pielou per Sito
E_sito <- Hs_sito/log(specnumber(ari3))
View(E_sito)

#Simpson per Sito
simp_sito <- diversity(ari3, "simpson")
View(simp_sito)




