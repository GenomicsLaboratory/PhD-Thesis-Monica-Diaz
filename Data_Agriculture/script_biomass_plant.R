##biomass_plant

library(plyr)
library(readxl)
library("agricolae")
library(multcomp)

Data_CW <- read_excel("Data_CW.xlsx", sheet = 8)
View(Data_CW)
summary(Data_CW)

Data_CW$BFresh_weight_Kg_m2<-as.numeric(Data_CW$Fresh_weight_Kg_m2)

names(Data_CW)

te.biomass <- ddply(Data_CW,.(Treatment),
                    summarise,n=length(Fresh_weight_Kg_m2),
                    means=mean(Fresh_weight_Kg_m2),sd=sd(Fresh_weight_Kg_m2))
te.biomass$error <- (te.biomass$sd / sqrt(te.biomass$n))

View(te.biomass)

biomass_1 <- aov(formula = Fresh_weight_Kg_m2 ~ Treatment*Days_of_culture, data = Data_CW)
summary(biomass_1)

TukeyHSD(biomass_1)
plot(TukeyHSD(biomass_1))



