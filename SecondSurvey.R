library(haven)
library(tidyverse)
library(dplyr)
dat <- read_sav("~/Desktop/Academics/PhD Research/U.S. RDE/2nd Run/AllData.sav")
colnames(dat)
dat$Condition <- factor(dat$Condition)

################
#Variables
################
#Make binary outcomes
summary(dat$DVDiaEduCont) #3.447
sd(dat$DVDiaEduCont, na.rm=TRUE)
dat$DVDiaEduDich <- cut(dat$DVDiaEduCont,
                        breaks = c(-Inf, 3.45, Inf),
                        levels = c(1,2),
                        labels = c("L", "H"))


summary(dat$DVSocCont) #3.83
sd(dat$DVSocCont, na.rm=TRUE)
dat$DVSocDich <- cut(dat$DVSocCont,
                     breaks = c(-Inf, 3.83, Inf),
                     levels = c(1,2),
                     labels = c("L", "H"))

summary(dat$DVTotalAct) #3.56
sd(dat$DVTotalAct, na.rm=TRUE)
dat$DVTotalDich <- cut(dat$DVTotalAct,
                       breaks = c(-Inf, 3.56, Inf),
                       levels = c(1,2),
                       labels = c("L", "H"))


prodat <- data.frame(filter(dat, Condition == 1)) #Promotion Experiment
predat <- data.frame(filter(dat, Condition == 2)) #Prevention Experiment

########################
#Heatmaps
########################

library(ComplexHeatmap)
library(circlize)
#Do not include outcomes in the variables; 
# Outcomes should be binary


###All Data

dathm1 <- select(dat, Q5, Q6, Q8, ProSco3, ProSco3Dich, Q11, Q12, Q13, PreSco3, PreSco3Dich)

#rescale data to z-scores
dathm2 <- scale(dathm1)


#Adding Binary Outcomes

ht_diaedu<-Heatmap(dat$DVDiaEduDich, name = "Dialogue & Education",
                   col = colorRamp2(c(0,1),c("white", "black")),
                   heatmap_legend_param = list(at = c(0,1),
                                               labels = c("Low", "High")),
                   width = unit(0.5,"cm"))

ht_soc<-Heatmap(dat$DVSocDich, name = "Social Activities",
                col = colorRamp2(c(0,1),c("white", "black")),
                heatmap_legend_param = list(at = c(0,1),
                                            labels = c("Low", "High")),
                width = unit(0.5,"cm"))
ht_total<-Heatmap(dat$DVTotalDich, name = "Total Activities",
                  col = colorRamp2(c(0,1),c("white", "black")),
                  heatmap_legend_param = list(at = c(0,1),
                                              labels = c("Low", "High")),
                  width = unit(0.5,"cm"))

ht_condition<-Heatmap(dat$Condition, name = "Experimental Condition",
                  col = colorRamp2(c(1,2),c("white", "black")),
                  heatmap_legend_param = list(at = c(1,2),
                                              labels = c("Promotion", "Prevention")),
                  width = unit(0.5,"cm"))

#no clustering
Heatmap(dathm2, name = "U.S. RDE Heatmaps",
        cluster_rows = FALSE, cluster_columns = FALSE)

#cluster by rows
Heatmap(dathm2, name = "U.S. RDE Heatmaps",
        cluster_columns = FALSE)

#cluster by both rows and columns
ht_all <- Heatmap(dathm2, name = "U.S. RDE Heatmaps") 
ht_all
  
draw(ht_all+ht_diaedu+ht_soc+ht_total, auto_adjust = FALSE)
#the binary is not super helpful lol

draw(ht_all+ht_condition, auto_adjust = FALSE)

#also not helpful


##Promotion Experimental Condition
prodathm1 <- select(prodat, Q5, Q6, Q8, ProSco3, ProSco3Dich, Q11, Q12, Q13, PreSco3, PreSco3Dich)

#rescale data to z-scores
prodathm2 <- scale(prodathm1)

#Heatmap clustering
Heatmap(prodathm2, name = "Promotion Framing")

##Prevention Experimental Condition
predathm1 <- select(predat, Q5, Q6, Q8, ProSco3, ProSco3Dich, Q11, Q12, Q13, PreSco3, PreSco3Dich)

#rescale data to z-scores
predathm2 <- scale(predathm1)


#Heatmap clustering
Heatmap(predathm2, name = "Prevention Framing")

#########################
#Data Clustering
#########################
datc <- select(dat, ProScoDich, PreScoDich)


#what is the best number of cluster
#using the elbow method
#The location of a knee in the plot is usually considered as an indicator of the appropriate number of clusters 
#because it means that adding another cluster does not improve much better the partition. 

library(factoextra)
library(NbClust)
fviz_nbclust(datc, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

#3 or 4 clusters

#3 clusters
model3 <- kmeans(datc, centers = 3)
#calculate quality of partition
(BSS3 <- model3$betweenss) #between sum squares
(TSS3 <- model3$totss) #total sum of squares
BSS3/TSS3*100 #67.21% explained percentage



#4 clusters seem to be it
model4 <- kmeans(datc, centers = 4)
#calculate quality of partition
(BSS4 <- model4$betweenss) #between sum squares
(TSS4 <- model4$totss) #total sum of squares
BSS4/TSS4*100 #73.33% explained percentage

#Visualization
library(cluster)
set.seed(1234)
km_res <- kmeans(datc, centers = 4, nstart = 20)

fviz_cluster(km_res, datc, ellipse.type = "norm")

#4 groups: low in both, high in both, and leaning for prevention or leaning for promotion
# Save the different groups to the data set
dat <- data.frame(dat, cluster = as.factor(model4$cluster))
head(dat)
summary(dat$cluster)
#cluster 1 is both (n = 114); cluster 2 is Promotion (n = 92); cluster 3 is neither (n = 49), and cluster 4 is prevention (n = 45)

##########################
#Manual clustering
##########################

#convert to Z score
dat <- dat %>%
  mutate(ProScoZ = (ProScoDich - mean(ProScoDich))/ sd(ProScoDich),
         PreScoZ = (PreScoDich - mean(PreScoDich))/ sd(PreScoDich))

dat[, 'group'] <- NA

dat <- dat %>%
  mutate(group = replace(group, ProScoZ > 0 && PreScoZ > 0, 1),
         group = replace(group, ProScoZ > 0 && PreScoZ <= 0, 2),
         group = replace(group, ProScoZ <= 0 && PreScoZ <= 0, 3),
         group = replace(group, ProScoZ <= 0 && PreScoZ > 0, 2))


#######################################
#Separate into Experimental Conditions
#######################################
prodat <- data.frame(filter(dat, Condition == 1)) #Promotion Experiment
predat <- data.frame(filter(dat, Condition == 2)) #Prevention Experiment


####Correlations 

#Cluster 1 Both
prodatc1 <-data.frame(filter(dat, Condition == 1, cluster == 1))
cor.test(prodatc1$ProScoCont, prodatc1$DVDiaEduCont) # significant r = .41, p = .001
cor.test(prodatc1$ProScoCont, prodatc1$DVSocCont) #not significant
cor.test(prodatc1$ProScoCont, prodatc1$DVTotalAct) # significant r = .33, p =.01

predatc1 <-data.frame(filter(dat, Condition == 2, cluster == 1))
cor.test(predatc1$PreScoCont, predatc1$DVDiaEduCont) # significant, r = .30, p =.02
cor.test(predatc1$PreScoCont, predatc1$DVSocCont) #n.s.
cor.test(predatc1$PreScoCont, predatc1$DVTotalAct) # r = .27, p = .04


#cluster 2 Promotion
prodatc2 <-data.frame(filter(dat, Condition == 1, cluster == 2))
cor.test(prodatc2$ProScoCont, prodatc2$DVDiaEduCont) # significant r = .37, p = .01
cor.test(prodatc2$ProScoCont, prodatc2$DVSocCont) #n.s.
cor.test(prodatc2$ProScoCont, prodatc2$DVTotalAct) # match is significant r = .33, p = .02

predatc2 <-data.frame(filter(dat, Condition == 2, cluster == 2))
cor.test(predatc2$ProScoCont, predatc2$DVDiaEduCont) # significant r = 39, p= .007
cor.test(predatc2$ProScoCont, predatc2$DVSocCont) #n.s.
cor.test(predatc2$ProScoCont, predatc2$DVTotalAct) #r = .30, p =.04


#cluster 3 Neither
prodatc3 <-data.frame(filter(dat, Condition == 1, cluster == 3))
cor.test(prodatc3$ProScoCont, prodatc3$DVDiaEduCont) # significant r = .41, p = .04
cor.test(prodatc3$ProScoCont, prodatc3$DVSocCont) #n.s.
cor.test(prodatc3$ProScoCont, prodatc3$DVTotalAct) # r = .40, p = .05

predatc3 <-data.frame(filter(dat, Condition == 2, cluster == 3))
cor.test(predatc3$PreScoCont, predatc3$DVDiaEduCont) # r = .51, p =.01
cor.test(predatc3$PreScoCont, predatc3$DVSocCont) # r =.44, p = .03
cor.test(predatc3$PreScoCont, predatc3$DVTotalAct) # r .56, p =.005

#Cluster 4 Prevention
prodatc4 <-data.frame(filter(dat, Condition == 1, cluster == 4))
cor.test(prodatc4$PreScoCont, prodatc4$DVDiaEduCont) # n.s.
cor.test(prodatc4$PreScoCont, prodatc4$DVSocCont) #r = .48, r =.02
cor.test(prodatc4$PreScoCont, prodatc4$DVTotalAct) # n.s.

predatc4 <-data.frame(filter(dat, Condition == 2, cluster == 4))
cor.test(predatc4$PreScoCont, predatc4$DVDiaEduCont) # n.s.
cor.test(predatc4$PreScoCont, predatc4$DVSocCont) # n.s.
cor.test(predatc4$PreScoCont, predatc4$DVTotalAct) # n.s.


#####T-tests
#Cluster 2 promotion with promotion vs. prevention framing
datc2 <- data.frame(filter(dat, cluster == 2))
datc2 <- select(datc2, DVDiaEduCont, DVSocCont, DVTotalAct, Condition)

for (i in 1:3) { #variables to compare are 1 to 3
  boxplot(datc2[,i]~datc2$Condition, #draw boxplot by group
          ylab = names(datc2[i]), #rename y-axis with variable's name
          xlab = "Condition"
  )
  print(t.test(datc2[,i] ~datc2$Condition)) #print results
}
# the t-test results are not significant yikes

#Cluster 4 prevention with promotion vs. prevention framing
datc4 <- data.frame(filter(dat, cluster == 4))
datc4 <- select(datc4, DVDiaEduCont, DVSocCont, DVTotalAct, Condition)

for (i in 1:3) { #variables to compare are 1 to 3
  boxplot(datc4[,i]~datc4$Condition, #draw boxplot by group
          ylab = names(datc4[i]), #rename y-axis with variable's name
          xlab = "Condition"
  )
  print(t.test(datc4[,i] ~datc4$Condition)) #print results
}
#not significant either sed


###############################
#Examining Fit with Polynomial
###############################


#visualize the data for all
library(ggplot2)
ggplot(dat, aes(x=ProScoCont, y=PreScoCont)) +
  geom_point()
#also looks linear

#####Promotion Condition#######
#visualize the data
ggplot(prodat, aes(x=ProScoCont, y=PreScoCont)) +
  geom_point()
#looks kind of linear

## Linear Regression for Total Activities
ModelPro1 <- lm(DVTotalAct ~ ProScoCont + PreScoCont, data = prodat)
summary(ModelPro1)
#Significant, moving onto polynomial

ModelPro12 <- lm(DVTotalAct ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = prodat)
summary(ModelPro12)
#Significant

ModelPro13 <- lm(DVTotalAct ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = prodat)
summary(ModelPro13)

#Compare Models
anova(ModelPro1, ModelPro12, ModelPro13)
#Model 2 has significant difference, go with polym

#Visualize
library(rsm)
par(mfrow=c(1,3))
image(ModelPro12, ProScoCont~PreScoCont)
contour(ModelPro12, ProScoCont~ PreScoCont)
persp(ModelPro12, ProScoCont~ PreScoCont, zlab = "Willingness to Engage")
##When there is a promotion framing
# people who score higher in promotion is motivated to engage regardless of their prevention score


## Linear Regression for Dialogue & Education
ModelPro2 <- lm(DVDiaEduCont ~ ProScoCont + PreScoCont, data = prodat)
summary(ModelPro2)
#Significant, moving onto polynomial

ModelPro22 <- lm(DVDiaEduCont ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = prodat)
summary(ModelPro22)
#Significant

ModelPro23 <- lm(DVDiaEduCont ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = prodat)
summary(ModelPro23)

#Compare Models
anova(ModelPro2, ModelPro22, ModelPro23)
#Go with Linear for this one


## Linear Regression for Social Activities
ModelPro3 <- lm(DVSocCont ~ ProScoCont + PreScoCont, data = prodat)
summary(ModelPro3)
#Significant, moving onto polynomial

ModelPro32 <- lm(DVSocCont ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = prodat)
summary(ModelPro32)
#Significant

ModelPro33 <- lm(DVSocCont ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = prodat)
summary(ModelPro33)

#Compare Models
anova(ModelPro3, ModelPro32, ModelPro33)
#Model 2 has significant difference, go with polym

#Visualize
par(mfrow=c(1,3))
image(ModelPro32, ProScoCont~PreScoCont)
contour(ModelPro32, ProScoCont~ PreScoCont)
persp(ModelPro32, ProScoCont~ PreScoCont, zlab = "Willingness to Engage")
##When there is a promotion framing
# people who score higher in promotion is motivated to engage regardless of their prevention score



####Prevention Condition######

## Linear Regression for Total Activities
ModelPre1 <- lm(DVTotalAct ~ ProScoCont + PreScoCont, data = predat)
summary(ModelPre1)
#Significant, moving onto polynomial

ModelPre12 <- lm(DVTotalAct ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = predat)
summary(ModelPre12)
#Significant

ModelPre13 <- lm(DVTotalAct ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = predat)
summary(ModelPre13)

#Compare Models
anova(ModelPre1, ModelPre12, ModelPre13)
#go with linear, not enough significant differences



## Linear Regression for Dialogue & Education
ModelPre2 <- lm(DVDiaEduCont ~ ProScoCont + PreScoCont, data = predat)
summary(ModelPre2)
#Significant, moving onto polynomial

ModelPre22 <- lm(DVDiaEduCont ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = predat)
summary(ModelPre22)
#Significant

ModelPre23 <- lm(DVDiaEduCont ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = predat)
summary(ModelPre23)

#Compare Models
anova(ModelPre2, ModelPre22, ModelPre23)
#Go with Linear for this one


## Linear Regression for Social Activities
ModelPre3 <- lm(DVSocCont ~ ProScoCont + PreScoCont, data = predat)
summary(ModelPre3)
#Significant, moving onto polynomial

ModelPre32 <- lm(DVSocCont ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = predat)
summary(ModelPre32)
#Significant

ModelPre33 <- lm(DVSocCont ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = predat)
summary(ModelPre33)

#Compare Models
anova(ModelPre3, ModelPre32, ModelPre33)
#Model 2 has significant difference, go with polym

#Visualize
par(mfrow=c(1,3))
image(ModelPre32, ProScoCont~PreScoCont)
contour(ModelPre32, ProScoCont~ PreScoCont)
persp(ModelPre32, ProScoCont~ PreScoCont, zlab = "Willingness to Engage")
##When there is a prevention framing
# people who score higher in prevention is motivated to engage regardless of their promotion score
# However, people who score high in both OR high prevention and extremely low in promotion are most strongly motivated 


########################
#Extreme Scores
########################
dat2 <- data.frame((filter(dat, Difference > 2.57)))
prodat2 <- data.frame((filter(dat2, Condition == 1)))
predat2 <- data.frame((filter(dat2, Condition == 2)))


##################################################
#Examining Fit with Polynomial for Extreme Scores
##################################################


#visualize the data for all
ggplot(dat2, aes(x=ProScoCont, y=PreScoCont)) +
  geom_point()
#looks so interesting lol

#####Promotion Condition#######
#visualize the data
ggplot(prodat2, aes(x=ProScoCont, y=PreScoCont)) +
  geom_point()
#looks kind of linear

## Linear Regression for Total Activities
Model2Pro1 <- lm(DVTotalAct ~ ProScoCont + PreScoCont, data = prodat2)
summary(Model2Pro1)
#Significant, moving onto polynomial

Model2Pro12 <- lm(DVTotalAct ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = prodat2)
summary(Model2Pro12)
#Significant

Model2Pro13 <- lm(DVTotalAct ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = prodat2)
summary(Model2Pro13)

#Compare Models
anova(Model2Pro1, Model2Pro12, Model2Pro13)
#Go with Linear



## Linear Regression for Dialogue & Education
Model2Pro2 <- lm(DVDiaEduCont ~ ProScoCont + PreScoCont, data = prodat2)
summary(Model2Pro2)
#Significant, moving onto polynomial

Model2Pro22 <- lm(DVDiaEduCont ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = prodat2)
summary(Model2Pro22)
#Significant

Model2Pro23 <- lm(DVDiaEduCont ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = prodat2)
summary(Model2Pro23)

#Compare Models
anova(Model2Pro2, Model2Pro22, Model2Pro23)
#Go with Linear for this one


## Linear Regression for Social Activities
Model2Pro3 <- lm(DVSocCont ~ ProScoCont + PreScoCont, data = prodat2)
summary(Model2Pro3)
#Significant, moving onto polynomial

Model2Pro32 <- lm(DVSocCont ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = prodat2)
summary(Model2Pro32)
#Significant

Model2Pro33 <- lm(DVSocCont ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = prodat2)
summary(Model2Pro33)

#Compare Models
anova(Model2Pro3, Model2Pro32, Model2Pro33)
#Model 2 has significant difference, go with polym

#Visualize
par(mfrow=c(1,3))
image(Model2Pro32, ProScoCont~PreScoCont)
contour(Model2Pro32, ProScoCont~ PreScoCont)
persp(Model2Pro32, ProScoCont~ PreScoCont, zlab = "Willingness to Engage")
##When there is a promotion framing
# people who score extremely high in one but extremely low in the other is highly motivated to engage



####Prevention Condition######

## Linear Regression for Total Activities
Model2Pre1 <- lm(DVTotalAct ~ ProScoCont + PreScoCont, data = predat2)
summary(Model2Pre1)
#Significant, moving onto polynomial

Model2Pre12 <- lm(DVTotalAct ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = predat2)
summary(Model2Pre12)
#Significant

Model2Pre13 <- lm(DVTotalAct ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = predat2)
summary(Model2Pre13)

#Compare Models
anova(Model2Pre1, Model2Pre12, Model2Pre13)
#go with linear, not enough significant differences



## Linear Regression for Dialogue & Education
Model2Pre2 <- lm(DVDiaEduCont ~ ProScoCont + PreScoCont, data = predat2)
summary(Model2Pre2)
#Significant, moving onto polynomial

Model2Pre22 <- lm(DVDiaEduCont ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = predat2)
summary(Model2Pre22)
#Significant

Model2Pre23 <- lm(DVDiaEduCont ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = predat2)
summary(Model2Pre23)

#Compare Models
anova(Model2Pre2, Model2Pre22, Model2Pre23)
#Go with Linear for this one


## Linear Regression for Social Activities
Model2Pre3 <- lm(DVSocCont ~ ProScoCont + PreScoCont, data = predat2)
summary(Model2Pre3)
#Significant, moving onto polynomial

Model2Pre32 <- lm(DVSocCont ~ polym(ProScoCont, PreScoCont, degree = 2, raw = TRUE), data = predat2)
summary(Model2Pre32)
#Significant

Model2Pre33 <- lm(DVSocCont ~ polym(ProScoCont, PreScoCont, degree = 3, raw = TRUE), data = predat2)
summary(Model2Pre33)

#Compare Models
anova(Model2Pre3, Model2Pre32, Model2Pre33)
#Go with linear

