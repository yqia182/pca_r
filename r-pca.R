library(vegan)
library(MASS)
par(mar=c(1,1,1,1))
# word directory
setwd("D:\\Desktop\\")
getwd()
all <- read.csv("mspc.csv",stringsAsFactors = TRUE)

factors <- all[,c(2:5)]
chems<- all[,c(6:44)]
is.factor(factors$Finished)
is.factor(factors$Juice)
is.factor(factors$Yeast)
# 3.	Scale the data scale is generic function whose default method centers and/or scales the columns of a numeric matrix.
chems.17 <- scale(chems,center=FALSE)

#Jaccard is the method used to compute distance matrices from the data frame "chemicals"
#Adonis() performs multivariate ANOVA using these distance matrices. This partitions distance matrices among sources of variation and fits a linea model to the distance matrices. This method uses permutation tests to inspect the significance of these partitions and produces pseudo-F ratios.
multiANOVA <- adonis(chems.17 ~ Yeast * Juice * Finished, method = "euclid", factors, perm = 1000)
multiANOVA

#Perform constrained correspondance anlaysis using rda().
#CCA does not try to display all variation in the data, but only the part that can be explained by the used constraints. CCA is a good choice if the user has clear and strong a prior hypotheses on constraints and is not interested in the major structure in the data set.
#cca(data matrix ~ constraining variables. data frame containing the constraining variables)
CCA <- rda(chems.17)

summary(CCA)

screeplot(CCA, type="lines", main="Proportion of explained variance")
CCAsummary <- summary(rda(chems.17))

library(car)
Juice.col <- c("blue", "orange", "purple", "brown")
Yeast.pch <- c(1:4)

### Temp in colours
plot(CCAsummary$sites[,1:2],xlab="PC1 ( %)", ylab="PC2 ( %)", type="n")
abline(v=0,h=-0,col="grey",lwd=2,lty=2)
points(yt$sites[,1:2],pch=yeast.pch[unclass(factors$Yeast)],col=temp.col[unclass(factors$Temp)],cex=1.5)


