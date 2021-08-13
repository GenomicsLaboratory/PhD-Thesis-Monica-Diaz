
library(plyr)
library(ggplot2)
library(gridExtra)
library("agricolae")
library(car)
library("agricolae")
getwd()

setwd("C:/Users/DACUI/Documents/attachments")

biomss <- read.csv(file = "biomss_lisi.csv", header = T,sep = ";", dec = ",")
attach(biomss)

te.biomasa <- ddply(biomss,.(TREATMENT),
                    summarise,n=length(Biomass_gr),
                    means=mean(Biomass_gr),sd=sd(Biomass_gr))
te.biomasa$error <- (te.biomasa$sd / sqrt(te.biomasa$n))
View(te.biomasa)

biomass_1 <- aov(formula = Fresh_weight_Kg_m2 ~ Treatment*Days_of_culture, data = biomss_1)
summary(biomass_1)
TukeyHSD(biomass_1, "Treatment")

##MODELO LINEAL 
lisimetro1 <- read.csv(file = "lisimetro1.csv", header = T,sep = ";", dec = ",")

attach(lisimetro1)
nitrate_day_1 <- aov(formula = NO3_mg.L ~ DAY*TREATMENT, data = lisimetro1)
summary(nitrate_day_1)
anova(nitrate_day_1)

nitrate_day_2 <- lm(formula = NO3_mg.L ~ DAY, data = subset(lisimetro1, TREATMENT =="Control-2"))
summary(nitrate_day_2)

nitrate_day_3 <- lm(formula = NO3_mg.L ~ DAY, data = subset(lisimetro1, TREATMENT =="Treatment-1"))
summary(nitrate_day_3)

nitrate_day_4 <- lm(formula = NO3_mg.L ~ DAY, data = subset(lisimetro1, TREATMENT =="Treatment-2"))
summary(nitrate_day_4)

