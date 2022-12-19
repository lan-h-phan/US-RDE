library(haven)
library(tidyverse)
library(dplyr)
library(Hmisc)
dat <- read_sav(file.choose())


####################
#Data Cleaning
####################

colnames(dat) #know the variables

#rename variables
dat<- rename(dat, Duration = Duration__in_seconds_)
dat$RaceGroup <- factor(dat$RaceGroup, labels = c(1,2,3))

#select only variables of interests
dat2 <- select(dat, Duration, RaceGroup, Gender, Politics, Q22.0, Q24.0, SES, Condition, Diff, DiffAbs,
               Salience, ProCont, ProDich, PreCont, PreDich, DVDiaEdu, DVSoc, DVTech, DVTotal)

#create 2 sets of experimental conditions
datPro <- dat2[dat2$Condition == 1,]
datPre <- dat2[dat2$Condition == 2,]

################
#ANCOVA
################

#maybe identity salience is a covariate

# Identity Salience has a significant positive relationship with the DVs, except for Technology


###########Promotion################

##DV1 -- Dialogue and Education (Example of full process)#####

# Consider controlling for Identity Salience as a covariate

#The test of the slope coefficient from a regression 
# will test if the linear relationship 
# is significant. It is not significant 
# (p-value = .0546).
lm_slope1 <- lm(DVDiaEdu ~ Salience, data = datPro)

summary(lm_slope1)

library(car)
library(emmeans)

# As predicted, the interaction is significant 

# The ANCOVA model in lm3, below, assumes no interaction
# between treatment group and the covariate. This can be 
# tested by running the full model that includes the 
# interaction term.
lm2 <- lm(DVDiaEdu ~ RaceGroup + Salience + RaceGroup:Salience,
          data = datPre)
Anova(lm2, type = 3)

#the interaction is significant, which violates the 
# assumption of no treatment by covariate interaction
# in ANCOVA. We should run two-way ANOVA instead

#######################
#Moderation Analyses
########################


#Running Race Salience as a dichotomous variable


#Dialogue & Education
#Already learned that interaction is significant

#Create interaction plot for all data
ggplot(dat2, aes(x = Salience,
                   y = DVDiaEdu,
                   color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Dialogue and Education",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines


#Create interaction plot for Promotion
ggplot(datPro, aes(x = Salience,
                y = DVDiaEdu,
                color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Dialogue and Education",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines

#Create interaction plot for Prevention
ggplot(datPre, aes(x = Salience,
                   y = DVDiaEdu,
                   color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Dialogue and Education",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines

#Basically, for White participants, the more salient their identity is, the less they are willing to engage in dialogue & education activities (or there is no difference)
#On the other hand, for Black and non-Black POC, the more salient their identity is, the more they are willing to engage in dialogue & education activities


#Social Activities
#Check interaction
lm3 <- lm(DVSoc ~ RaceGroup + Salience + RaceGroup:Salience,
          data = datPre)
Anova(lm3, type = 3)
#interaction is significant

#Create interaction plot for all data
ggplot(dat2, aes(x = Salience,
                 y = DVSoc,
                 color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Social Activities",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines


#Create interaction plot for Promotion
ggplot(datPro, aes(x = Salience,
                   y = DVSoc,
                   color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Social Activities",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines

#Create interaction plot for Prevention
ggplot(datPre, aes(x = Salience,
                   y = DVSoc,
                   color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Social Activities",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines


#Tech
#Check interaction
lm4 <- lm(DVTech ~ RaceGroup + Salience + RaceGroup:Salience,
          data = datPre)
Anova(lm4, type = 3)
#interaction is significant

#Create interaction plot for all data
ggplot(dat2, aes(x = Salience,
                 y = DVTech,
                 color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Technological Activities",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines


#Create interaction plot for Promotion
ggplot(datPro, aes(x = Salience,
                   y = DVTech,
                   color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Technological Activities",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines

#Create interaction plot for Prevention
ggplot(datPre, aes(x = Salience,
                   y = DVTech,
                   color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Technological Activities",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines

#Basically, for White participants, the more salient their identity is, the less they are willing to engage in technological activities (or there is no difference)
#On the other hand, for Black and non-Black POC, the more salient their identity is, the more they are willing to engage in technological activities


#All Activities
#Check interaction
lm5 <- lm(DVTotal ~ RaceGroup + Salience + RaceGroup:Salience,
          data = datPre)
Anova(lm5, type = 3)
#interaction is significant

#Create interaction plot for all data
ggplot(dat2, aes(x = Salience,
                 y = DVTotal,
                 color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Total Activities",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines


#Create interaction plot for Promotion
ggplot(datPro, aes(x = Salience,
                   y = DVTotal,
                   color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Total Activities",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines

#Create interaction plot for Prevention
ggplot(datPre, aes(x = Salience,
                   y = DVTotal,
                   color = RaceGroup)) +
  theme_bw() +
  labs(x = "Identity Salience",
       y = "Total Activities",
       color = "Racial Groups") +
  geom_smooth(method = "lm") #turn it into lines
