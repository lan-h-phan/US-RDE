library(haven)
library(tidyverse)
library(dplyr)
dat <- read_sav("~/Desktop/Academics/PhD Research/U.S. RDE/4th Run-Locomotion/U.S.+RDE+Just+Locomotion_August+24,+2022_08.46.sav")
dat<- subset(dat, !is.na(IVAssessDich))
colnames(dat)

#########################
#Data Clustering
#########################
datc <- select(dat, IVLocoDich, IVAssessDich)
datc <- na.omit(datc)

#what is the best number of cluster
#using the elbow method
#The location of a knee in the plot is usually considered as an indicator of the appropriate number of clusters 
#because it means that adding another cluster does not improve much better the partition. 


install.packages("factoextra")
install.packages("NbClust")
library(factoextra)
library(NbClust)
fviz_nbclust(datc, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

#3 or 4 clusters

#3 clusters seem to be it
model3 <- kmeans(datc, centers = 3)
#calculate quality of partition
(BSS3 <- model3$betweenss) #between sum squares
(TSS3 <- model3$totss) #total sum of squares
BSS3/TSS3*100 #66.31% explained percentage


#Visualization
library(cluster)
set.seed(1234)
km_res <- kmeans(datc, centers = 3, nstart = 20)

fviz_cluster(km_res, datc, ellipse.type = "norm")

#3 groups: 1 = high in both, 2 = Assessment, 3 = locomotion
# Save the different groups to the data set
dat <- data.frame(dat, cluster = as.factor(model3$cluster))
head(dat)
summary(dat$cluster)
#group 1 has 83, group 2 = 140, gr 3 = 77


###Cluster 1 both
datc1 <-data.frame(filter(dat, cluster == 1))
cor.test(datc1$IVLocomotion, datc1$DVLocomotion) # significant r = .24, p = .02
cor.test(datc1$IVAssessment, datc1$DVAssessment) #n.s.
cor.test(datc1$IVLocomotion, datc1$DVAssessment) #n.s.
cor.test(datc1$IVAssessment, datc1$DVLocomotion) #n.s.




###Cluster 2 Assessment
datc2 <-data.frame(filter(dat, cluster == 2))
cor.test(datc2$IVLocomotion, datc2$DVLocomotion) # n.s.
cor.test(datc2$IVAssessment, datc2$DVAssessment) #n.s.
cor.test(datc2$IVLocomotion, datc2$DVAssessment) #n.s.
cor.test(datc2$IVAssessment, datc2$DVLocomotion) #n.s.

#####One sample T-tests
datc2t <- select(datc2, DVAssessment, DVLocomotion)
t.test(datc2t) #significant at t(279) = 62.67, p <.001
summary(datc2t$DVAssessment) #2.32
summary(datc2t$DVLocomotion) #3.87
#lol this is not to be expected

###Cluster 3 Locomotion
datc3 <-data.frame(filter(dat, cluster == 3))
cor.test(datc3$IVLocomotion, datc3$DVLocomotion) # r = .38, p <.001
cor.test(datc3$IVAssessment, datc3$DVAssessment) #n.s.
cor.test(datc3$IVLocomotion, datc3$DVAssessment) #n.s.
cor.test(datc3$IVAssessment, datc3$DVLocomotion) #n.s.

#####One sample T-tests
datc3t <- select(datc3, DVAssessment, DVLocomotion)
t.test(datc3t) #significant at t(153) = 37.62, p <.001
summary(datc3t$DVAssessment) #2.94
summary(datc3t$DVLocomotion) #3.63
