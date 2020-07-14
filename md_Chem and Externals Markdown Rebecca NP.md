---
title: "PERMANOVA of Chemicals vs. Externals (2017)"
author: "Rebecca"
date: "Thursday, February the 16th, 2017"
output: word_document
---
```{r echo=FALSE}
# Katie Uni
setwd("C:/Users/kpar121/Dropbox/2014 Katie")
# Katie laptop
#setwd("C:/Users/Katie/Dropbox/2014 Katie")
#setwd("~/Dropbox/01active/03work/10 Students/04 PhD/2014 Katie/")
#getwd()
```

1. Read in necessary libraries
```{r Read_in_libraries}
library(vegan)
library(MASS)
```

2. Read in data files: Matrix of chemical data (all in ug/L) and wine factors which includes the sample (1-59) vineyard, fraction, fining agent and fermentation replicate number
```{r}
all <- read.csv("RebeccaData.csv")

factors <- all[,c(2:6)]
chems<- all[,c(7:34)]

is.factor(factors$Temp)
factors$Temp <- factor(factors$Temp, levels = c("12.5", "25"))
```

3. Scale the data
```{r}
chems.17 <- scale(chems,center=FALSE)
```

4. Create and view model with all variables

Jaccard is the method used to compute distance matrices from the data frame "chemicals"

Adonis() performs multivariate ANOVA using these distance matrices. This partitions distance matrices among sources of variation and fits a linea model to the distance matrices. This method uses permutation tests to inspect the significance of these partitions and produces pseudo-F ratios.

```{r}
yeasttemp <- adonis(chems.17 ~ Yeast * Juice * Temp *  Finished, method = "euclid", factors, perm = 1000)
yeasttemp
```


5. In order to better visualize how are Fining Agents (6.1% of the variation) are influencing our model we can perform constrained correspondance analysis (a.k.a. canonical correpondence analysis). 

Perform constrained correspondance anlaysis using rda().

CCA does not try to display all variation in the data, but only the part that can be explained by the used constraints. CCA is a good choice if the user has clear and strong a prior hypotheses on constraints and is not interested in the major structure in the data set.

cca(data matrix ~ constraining variables. data frame containing the constraining variables)
 
```{r}
yeasttemp.rda <- rda(chems.17)

summary(yeasttemp.rda)

screeplot((rda(chems.17)), type="lines", main="Proportion of explained variance")


```


6. Save results in summary
You can only call 'species' and 'sites' if you use the summary of the rda. Otherwise you must use $CA$u and $CA$v.

```{r}
yeasttemp.rda

yt <- summary(rda(chems.17))
```


7. Build a PCA for PC1 vs PC2

```{r}
library(car)
temp.col <- c("blue", "red")
juice.col <- c("green", "orange")
yeast.pch <- c(1:4)

### Temp in colours
plot(yt$sites[,1:2],xlab="PC1 (40.9 %)", ylab="PC2 (32.2 %)", type="n")
abline(v=0,h=-0,col="grey",lwd=2,lty=2)
points(yt$sites[,1:2],pch=yeast.pch[unclass(factors$Yeast)],col=temp.col[unclass(factors$Temp)],cex=1.5)

### Juice in colours
plot(yt$sites[,1:2],xlab="PC1 (40.9 %)", ylab="PC2 (32.2 %)", type="n")
abline(v=0,h=-0,col="grey",lwd=2,lty=2)
points(yt$sites[,1:2],pch=yeast.pch[unclass(factors$Yeast)],col=juice.col[unclass(factors$Juice)],cex=1.5)


legend("topright",c("EC1118","L1528","M2", "X5"),pch=1:4,bty="n")


```

8. Mardia's criterion
```{r}
pc1.mardia.thresh <- .5*max(abs(yt$species[,1]))
pc2.mardia.thresh <- .5*max(abs(yt$species[,2]))

mardia.choice1 <- which(abs(yt$species[,1])>pc1.mardia.thresh)
mardia.choice2 <- which(abs(yt$species[,2])>pc2.mardia.thresh)

mardia.choice1
mardia.choice2
```


9. Temp biplot 
```{r}
pdf("awesome temp plot blank.pdf", height = 7, width = 10)

plot(yt$sites[,(1:2)],xlab="PC1 (40.9 %)", ylab="PC2 (32.2 %)", type = "n")

abline(v=0,h=0,col="grey",lwd=2,lty=2)

points(yt$species[mardia.choice1,1:2], col="black", pch = 16, cex = 1.5)

points(yt$species[mardia.choice2,1:2], col="black",pch=16,cex=1.5)

points(yt$sites[,1:2],pch=yeast.pch[unclass(factors$Yeast)],col=temp.col[unclass(factors$Temp)],cex=1.5)

#text(yt$species[mardia.choice1,1:2],labels=names(mardia.choice1),pos=1)

#text(yt$species[mardia.choice2,1:2],labels=names(mardia.choice2),pos=1)

dev.off()
```

9. Juice biplot 
```{r}
pdf("awesome juice plot blank.pdf", height = 7, width = 10)

plot(yt$sites[,(1:2)],xlab="PC1 (40.9 %)", ylab="PC2 (32.2 %)", type = "n")

abline(v=0,h=0,col="grey",lwd=2,lty=2)

points(yt$species[mardia.choice1,1:2], col="black", pch = 16, cex = 1.5)

points(yt$species[mardia.choice2,1:2], col="black",pch=16,cex=1.5)

points(yt$sites[,1:2],pch=yeast.pch[unclass(factors$Yeast)],col=juice.col[unclass(factors$Juice)],cex=1.5)

#text(yt$species[mardia.choice1,1:2],labels=names(mardia.choice1),pos=1)

#text(yt$species[mardia.choice2,1:2],labels=names(mardia.choice2),pos=1)

dev.off()
```

10. Load in library
```{r}
library(car)
```

11. Add dataellipse to identify juices
```{r}
pdf("temp with juice ellipses blank.pdf", height = 7, width = 10)

plot(yt$sites[,(1:2)],xlab="PC1 (40.9 %)", ylab="PC2 (32.2 %)", type = "n", xlim = c(-1.8,1.3), ylim = c(-2,1.9))

abline(v=0,h=0,col="grey",lwd=2,lty=2)

points(yt$species[mardia.choice1,1:2], col="black", pch = 16, cex = 1.5)

points(yt$species[mardia.choice2,1:2], col="black",pch=16,cex=1.5)

points(yt$sites[,1:2],pch=yeast.pch[unclass(factors$Yeast)],col=temp.col[unclass(factors$Temp)],cex=1.5)

dataEllipse(yt$sites[,1],yt$sites[,2],factors$Juice, plot.points=F, center.pch = FALSE, col = juice.col, levels = 0.95, group.labels = c("",""))

dev.off()
```

