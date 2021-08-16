###habilitar librerias

Data_CW <- read_excel("Data_CW.xlsx", sheet = 3)
View(Data_CW)
summary(Data_CW)

names(Data_CW)
Data_CW$Treatment <- as.factor(Data_CW$Treatment)
Data_CW$Input <- as.factor(Data_CW$Input)
Data_CW$NO3_mg<-as.numeric(Data_CW$NO3_mg)
summary(Data_CW)

names(Data_CW)
day_means<-ddply(Data_CW,.(Days_of_culture,Treatment),summarise,n=length(`NO3_mg`),means=mean(`NO3_mg`),sd=sd(`NO3_mg`))


L1<-ggplot(data = subset(Data_CW,Period =="INPUT_1"), aes(x = Days_from_imput, y = `NO3_mg`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, 
              level = 0.95) +
  labs(title = "Imput 1 ") +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")), formula = y ~ poly(x, 2), parse = T, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",limits=c(min(na.omit(day_means$means)),max(na.omit(day_means$means))))+
  theme_bw() + theme(legend.position="none")
plot(L1)

L2<-ggplot(data = subset(Data_CW,Period =="INPUT_2"), aes(x = Days_from_imput, y = `NO3_mg`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, 
              level = 0.95) +
  labs(title = "Imput 2 ") +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")), formula = y ~ poly(x, 2), parse = T, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",limits=c(min(na.omit(day_means$means)),max(na.omit(day_means$means))))+
  theme_bw() + theme(legend.position="none")
plot(L2)

L3<-ggplot(data = subset(Data_CW,Period =="INPUT_3"), aes(x = Days_from_imput, y = `NO3_mg`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, 
              level = 0.95) +
  labs(title = "Imput 3 ") +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")), formula = y ~ poly(x, 2), parse = T, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",limits=c(min(na.omit(day_means$means)),max(na.omit(day_means$means))))+
  theme_bw() + theme(legend.position = "bottom")
plot(L3)

L4<-ggplot(data = subset(Data_CW,Period =="INPUT_4"), aes(x = Days_from_imput, y = `NO3_mg`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, 
              level = 0.95) +
  labs(title = "Imput 4 ") +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")), formula = y ~ poly(x, 2), parse = T, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",limits=c(min(na.omit(day_means$means)),max(na.omit(day_means$means))))+
  theme_bw() + theme(legend.position="none")
plot(L4)

L5<-ggplot(data = subset(Data_CW,Period =="INPUT_5"), aes(x = Days_from_imput, y = `NO3_mg`, shape=Treatment, colour=Treatment, fill=Treatment)) +
  geom_point(size=1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, 
              level = 0.95) +
  labs(title = "Imput 5 ") +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")), formula = y ~ poly(x, 2), parse = T, label.x = 0.9)+
  scale_x_continuous(name="Time (days)",breaks = seq(0,8,1))+
  scale_y_continuous(name="Nitrate (mg/L)",limits=c(min(na.omit(day_means$means)),max(na.omit(day_means$means))))+
  theme_bw()+ theme(legend.position="none")
plot(L5)

