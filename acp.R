library("FactoMineR")
file_path <- "C:/TP/IDD Med.csv"

data <- read.table(file_path, header=TRUE, row.names=3, sep=";", dec=",", check.names=FALSE, fileEncoding="latin1")
data
summary(data)

# Changement du type de la variable "année" en catégorielle
data$année <- as.factor(data$année)
data
summary(data)

pcaresults <- PCA(data[, 3:23], scale.unit=TRUE, ncp=5)
pcaresults

pca70 <- PCA(data[data$année == 70, 3:23], scale.unit=TRUE, ncp=5)
pca2000 <- PCA(data[data$année == 2000, 3:23], scale.unit=TRUE, ncp=5)

par(mfrow=c(1,2))
plot(pca70, graph.type="classic", cex=0.5, cex.lab=0.5, cex.main=0.5)
plot(pca2000, graph.type="classic", cex=0.5, cex.lab=0.5, cex.main=0.5)
par(mfrow=c(1,2))
plot(pca2000, choix="ind", graph.type="classic", cex=0.5, cex.lab=0.5, cex.main=0.5)
plot(pca2000, choix="var", graph.type="classic", cex=0.5, cex.lab=0.5, cex.main=0.5)

summary(pcaresults)
summary(pcaresults, ncp=5, nbelements=Inf)
write.csv(summary(pcaresults, nbelements=Inf), file="resultats_acp.csv")

pcaresults$eig
pcaresults$ind
pcaresults$ind.sup
pcaresults$var

dimdesc(pcaresults, proba=0.01)
plot(pcaresults, axes=2:3)
plot(pcaresults, axes=2:3, choix="var")
pcaresults2 <- PCA(data[, 3:23], ind.sup=17:112)
plot(pcaresults2, axes=2:3, cex=0.5, title="Dev durable des pays")
plot(pcaresults2, axes=2:3, cex=2, title="Dev durable des pays", select="Maroc00")
plot(pcaresults2, axes=2:3, cex=2, title="Dev durable des pays", select=c('Maroc00', 'Maroc95', 'Maroc90'))
plot(pcaresults2, axes=2:3, cex=2, title="Dev durable des pays", select="cos2 0.7")
plot(pcaresults2, axes=2:3, cex=2, title="Dev durable des pays", select="cos2 8")
plot(pcaresults2, axes=2:3, cex=2, title="Dev durable des pays", select="contrib 5")
pcaresults3 <- PCA(data[, 3:23], ind.sup=17:112, quanti.sup=12)
plot(pcaresults3, habillage="co2")
plot(pcaresults3, cex=2, choix="var", select="contrib 8")
pcaresults4 <- PCA(data[1:16, 3:23], quanti.sup=1:5)
pcaresults5 <- PCA(data[1:16, 3:23], quali.sup=1:5)

install.packages("factoextra")

library(factoextra)
fviz_eig(pcaresults)
fviz_pca_ind(pcaresults, col.ind="cos2", gradient.cols=c("#EDE342", "#F69A97", "#714674"))
fviz_pca_var(pcaresults, col.var="contrib", gradient.cols=c("#EDE342", "#F69A97", "#714674"))
fviz_pva_biplot(pcaresults, col.var="#2E9FDF", col.ind="#696969")

library(Factoshiny)
Factoshiny(data[1:16, 3:23])

