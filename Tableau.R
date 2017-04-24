library(tidyverse)
library(ade4)
library(readr)
library(stringr)

##### ce 23/4 pour tableau de Guillaume
#### library(readr)
### Chargement du fichier de donn√©es

Data_GH <- read_csv("~/PGC_PERSONNEL/LaboratoiR/UMR Guillaume/Data_GH.csv",
col_types = cols(BalnÈaire = col_integer(),
Classe = col_factor(levels = c("1",
"2", "3", "4", "5")), SÈparation = col_integer()),
locale = locale(decimal_mark = ","))
View(Data_GH)

for(i in (1:nrow(Data_GH))){
  Data_GH$abb[i] <- str_split(Data_GH$Localisation,",")[[i]][1]
}
Data_GH$abb <- str_replace(Data_GH$abb,"Saint","St")
row.names(Data_GH) <- Data_GH$abb
Data_GH$mat <- as.factor(3*Data_GH$Pierre+2*Data_GH$Mixte+1*Data_GH$`MatÈriaux pÈrissables`)
levels(Data_GH$mat) <- c("Mat.pÈrissables","Mixte","Pierre")

# dÈtermination des NA" commme non rÈponse
Data_GH[which(is.na(Data_GH$BalnÈaire)),"BalnÈaire"] <- 0
Data_GH[which(is.na(Data_GH$SÈparation)),"SÈparation"] <- 0

# quantification des infrastructures et des outils
Data_GH$nbInfra <- apply(Data_GH[,10:18],1,function(x) sum(x))
Data_GH$nbOutil <- apply(Data_GH[,19:38],1,function(x) sum(x))

#### Jeux stats : dimension des Ètablissements et Èquipements
### coefficient de correlation

CorInfra <- cor(Data_GH$`Superficie Ha` , Data_GH$nbInfra)
CorOutil <- cor(Data_GH$`Superficie Ha` , Data_GH$nbOutil)
CorInfraOutil <- cor(Data_GH$nbInfra,Data_GH$nbOutil)

# mÍme ‡ la limite du seuil, il exisite une correlation positive surface ha / nb de dispositfs

#### ReprÈsentation graphique de la faible corrÈlation
gCorOutil <- ggplot(data=Data_GH,aes(x = `Superficie Ha`,y=nbOutil))
gCorOutil <- gCorOutil + geom_point(aes(color=as.character(Classe)))+
# gCorOutil <- gCorOutil + geom_smooth(method=lm,se=T, linetype="dashed",color="darkred") 
  labs(title= "Relation entre superficie et nombre d'outils distincts")+
  labs(colour="Classes de taille",x="Superficie en ha",y="nombre d'outils distincts",caption="Data G.H")
gCorOutil


gCorInfra <- ggplot(data=Data_GH,aes(x = `Superficie Ha`,y=nbInfra))
gCorInfra <- gCorInfra + geom_point(aes(color=Classe))+
  labs(title= "Relation entre superficie et nombre d'infrastructures distinctes")+
  labs(colour="Classes de taille",x="Superficie en ha",y="nombre d'installations distinctes",caption="Data G.H")
gCorInfra

gBxMat <- ggplot(data=Data_GH,aes(y = `Superficie Ha`,x=mat))
gBxMat <- gBxMat+geom_boxplot()+
labs(title= "Dimensions par nature de matÈriaux")+
  labs(x="Type de matÈriaux",y="Surface en Ha",caption="Data G.H")
gBxMat


Data_GHRed <- Data_GH[,10:38]
Data_GHRedI <- Data_GH[,10:18]
Data_GHRedO <- Data_GH[,19:38]

library(ade4)

AFCRed <- dudi.coa(Data_GHRed,scannf = F,nf=7)
AFCRedI <- dudi.coa(Data_GHRedI,scannf = F,nf=7)
AFCRedO <- dudi.coa(Data_GHRedO,scannf = F,nf=7)

dli <- dist.dudi(AFCRed)
dco <- dist.dudi(AFCRed, amongrow = FALSE)

summary(Data_GH$mat)
table(Data_GH$Classe,Data_GH$mat)
### Chi2 , test de variances, wilcox
chisq.test(Data_GH$`Superficie Ha`,Data_GH$nb)
var.test(Data_GH$`Superficie Ha`,Data_GH$nb)
wilcox.test(Data_GH$`Superficie Ha`,Data_GH$nb)
kruskal.test(`Superficie Ha` ~ nb, data = Data_GH)

#####
plot(hclust(dco,method = "ward.D2"),labels=names(Data_GHRed),frame.plot = T,main = "Dendogramme - variables",ylab=NULL, hang=-1,cex=0.75)
plot(hclust(dli,method = "ward.D2"),labels=Data_GH$abb,frame.plot = T,main = "Dendogramme - lignes",ylab=NULL, hang=-1,cex=0.75)

p.AFCRed.li <- ggplot(AFCRed$li,aes(Axis1,Axis2))
p.AFCRed.li <- p.AFCRed.li+geom_point()
p.AFCRed.li

p.AFCRed.co <- ggplot(AFCRed$co,aes(Comp1,Comp2))
p.AFCRed.co <- p.AFCRed.co+geom_point()
p.AFCRed.co

library(adegraphics)







g1 <- s.label(AFCRed$co, ppoints=list(cex = 1.5, col= "red"), plabels = list(box = list(draw = FALSE),optim = FALSE,cex=0.75,col="black"),plot = FALSE)
g2 <- s.label(AFCRed$li, ppoints=list(cex = 1.5, col= "dark green"), plabels = list(box = list(draw = FALSE),optim = FALSE,cex=0.75,col="black"),plot = FALSE)
cbindADEg(g1, g2, plot = TRUE)
