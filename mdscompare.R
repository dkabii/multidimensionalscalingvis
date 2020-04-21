#install.packages("magrittr")
#install.packages("dplyr")
#install.packages("ggpubr")

library(magrittr)
library(dplyr)
library(ggpubr)
library(MASS)

setwd("F:/projects/Rrena/rassignment");
getwd()
plantsData = read.csv("Hyptis-2.csv")
plantsDataNumeric = subset(plantsData, select=c(1,2,3,4,5,6,7))
head(plantsData)
head(plantsDataNumeric)
plantsDataNumericMatrix = as.matrix(plantsDataNumeric)
summary(plantsDataNumeric)
print(plantsDataNumericMatrix)
summary(plantsDataNumericMatrix)

#chart
res.cor <- cor(plantsDataNumeric, method = "spearman")
mds.cor <- (1 - res.cor) %>%
  cmdscale() %>%
  as_tibble()
colnames(mds.cor) <- c("Dim.1", "Dim.2")
ggscatter(mds.cor, x = "Dim.1", y = "Dim.2", 
          size = 1,title="Classical Scaling",
          label = colnames(res.cor),
          repel = TRUE)

#chart
mds <- plantsDataNumeric %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
print(mds)
colnames(mds) <- c("Dim.1", "Dim.2")
mds['Location']=as.character(plantsData[,8])
head(mds)
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = plantsData[,8],
          size = 1, title="Classical Scaling",
	    color="Location",
          repel = TRUE)




#chart
mds2 <- plantsDataNumeric %>%
  dist() %>%          
  sammon() %>%
  .$points %>%
  as_tibble()
colnames(mds2) <- c("Dim.1", "Dim.2")
mds2['Location']=plantsData[,8]
head(mds2)
ggscatter(mds2, x = "Dim.1", y = "Dim.2", 
          label = plantsData[,8],
          size = 1,title="Sammon Scaling",
	    color="Location",
          repel = TRUE)

#chart
mds3 <- plantsDataNumeric %>%
  dist() %>%          
  isoMDS() %>%
  .$points %>%
  as_tibble()
colnames(mds3) <- c("Dim.1", "Dim.2")
mds3['Location']=as.character(plantsData[,8])
head(mds3)
ggscatter(mds3, x = "Dim.1", y = "Dim.2", 
          label = plantsData[,8],
          size = 1,title="Kruskal's Scaling",
	    color="Location",
          repel = TRUE)

mds['scaling']='Classical'
head(mds)
mds2['scaling']='Sammon'
head(mds2)
mds3['scaling']='Kruskal'
head(mds3)
mdsFinal = rbind(mds, mds2)
mdsFinal = rbind(mdsFinal, mds3)
head(mdsFinal)
tail(mdsFinal)
print(mdsFinal$Location)

#chart
ggscatter(mdsFinal, x = "Dim.1", y = "Dim.2", 
          size = 1,title="Scaling Comparision",
	    label = mdsFinal$Location, color="scaling",
          repel = TRUE)

