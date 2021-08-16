install.packages("readxl")
library(readxl)
library(plyr)
library(ggplot2)
library(gridExtra)
install.packages("ggpmisc")
library("ggpmisc")
library("data.table")

NO3_1 <- read_xlsx("Data.xlsx", sheet = 4)
NO3_1$Treatment <- as.factor(NO3_1$Treatment)
NO3_1$Period <- as.factor(NO3_1$Period)
summary(NO3_1)
        

summary(lm(`NO3_mg*L-1` ~ Treatment*Days_of_culture, data = subset(NO3_1,Period =="INPUT_1")))
summary(lm(`NO3_mg*L-1` ~ Treatment*Days_of_culture, data = subset(NO3_1,Period =="INPUT_2")))
summary(lm(`NO3_mg*L-1` ~ Treatment*Days_of_culture, data = subset(NO3_1,Period =="INPUT_3")))
summary(lm(`NO3_mg*L-1` ~ Treatment*Days_of_culture, data = subset(NO3_1,Period =="INPUT_4")))
summary(lm(`NO3_mg*L-1` ~ Treatment*Days_of_culture, data = subset(NO3_1,Period =="INPUT_5")))


NO3_2 <- read_xlsx("Data.xlsx", sheet = 5)
NO3_2$Treatment <- as.factor(NO3_2$Treatment)
NO3_2$Period <- as.factor(NO3_2$Period)
summary(NO3_2)

summary(lm(`NO3_mg*L-1` ~ Treatment*Days_of_culture, data = subset(NO3_2,Period =="INPUT_1")))
summary(lm(`NO3_mg*L-1` ~ Treatment*Days_of_culture, data = subset(NO3_2,Period =="INPUT_2")))
summary(lm(`NO3_mg*L-1` ~ Treatment*Days_of_culture, data = subset(NO3_2,Period =="INPUT_3")))
summary(lm(`NO3_mg*L-1` ~ Treatment*Days_of_culture, data = subset(NO3_2,Period =="INPUT_4")))
summary(lm(`NO3_mg*L-1` ~ Treatment*Days_of_culture, data = subset(NO3_2,Period =="INPUT_5")))

l1 <- ggplot(data = subset(NO3_1,Period =="INPUT_1"), aes(x = Days_from_imput, y = `NO3_mg*L-1`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(title = "Imput 1 / 1-4 days") +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",breaks = seq(0,20,2))+
  theme_bw() + theme(legend.position="right")
plot(l1)
l2<-ggplot(data = subset(NO3_1,Period =="INPUT_2"), aes(x = Days_from_imput, y = `NO3_mg*L-1`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(title = "Imput 2 / 1-4 days") +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",breaks = seq(0,20,2))+
  theme_bw() + theme(legend.position="right")
plot(l2)

l3<-ggplot(data = subset(NO3_1,Period =="INPUT_3"), aes(x = Days_from_imput, y = `NO3_mg*L-1`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(title = "Imput 3 / 1-4 days") +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",breaks = seq(0,20,2))+
  theme_bw() + theme(legend.position="right")
plot(l3)

l4<-ggplot(data = subset(NO3_1,Period =="INPUT_4"), aes(x = Days_from_imput, y = `NO3_mg*L-1`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(title = "Imput 4 / 1-4 days") +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",breaks = seq(0,20,2))+
  theme_bw() + theme(legend.position="right")
plot(l4)

l5<-ggplot(data = subset(NO3_1,Period =="INPUT_5"), aes(x = Days_from_imput, y = `NO3_mg*L-1`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(title = "Imput 5 / 1-4 days") +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",breaks = seq(0,20,2))+
  theme_bw() + theme(legend.position="right")
plot(l5)


###DAY 5 TO 8

m1<-ggplot(data = subset(NO3_2,Period =="INPUT_1"), aes(x = Days_from_imput, y = `NO3_mg*L-1`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(title = "Imput 1 / 5-8 days") +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",breaks = seq(0,20,2))+
  theme_bw() + theme(legend.position="right")

plot(m1)
m2<-ggplot(data = subset(NO3_2,Period =="INPUT_2"), aes(x = Days_from_imput, y = `NO3_mg*L-1`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(title = "Imput 2 / 5-8 days") +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",breaks = seq(0,20,2))+
  theme_bw() + theme(legend.position="right")
plot(m2)

m3<-ggplot(data = subset(NO3_2,Period =="INPUT_3"), aes(x = Days_from_imput, y = `NO3_mg*L-1`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(title = "Imput 3 / 5-8 days") +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",breaks = seq(0,20,2))+
  theme_bw() + theme(legend.position="right")
plot(m3)

m4<-ggplot(data = subset(NO3_2,Period =="INPUT_4"), aes(x = Days_from_imput, y = `NO3_mg*L-1`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(title = "Imput 4 / 5-8 days") +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",breaks = seq(0,20,2))+
  theme_bw() + theme(legend.position="right")
plot(m4)

m5<-ggplot(data = subset(NO3_2,Period =="INPUT_5"), aes(x = Days_from_imput, y = `NO3_mg*L-1`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(title = "Imput 5 / 5-8 days") +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), formula = y ~ x, parse = TRUE, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",breaks = seq(0,20,2))+
  theme_bw() + theme(legend.position="right")
plot(m5)



