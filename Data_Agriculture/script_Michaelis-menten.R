library(drc)

library(ggplot2)


mm <- structure(list(S = c(0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4), v = c(0.064, 0.03097, 0.2585, 3.8583, 
                                                                               4.4120, 3.515, 3.6746, 4.5133, 5.9260, 6.7370, 
                                                                               5.6424, 4.1557, 4.5137, 6.1040, 6.8322)), .Names = c("S", "v"), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                   -15L))
model.drm <- drm (v ~ S, data = mm, fct = MM.2())

mml <- data.frame(S = seq(0, max(mm$S), length.out = 100))
mml$v <- predict(model.drm, newdata = mml)

ggplot(mm, aes(x = S, y = v)) +
  theme_bw() +
  xlab("Ammonium-N concentration [mM]") +
  ylab("Ammonium uptake rate [mM NH4Cl g-1 DW h-1]") +
  ggtitle("Michaelis-Menten kinetics") +
  geom_point(alpha = 0.5) +
  geom_line(data = mml, aes(x = S, y = v), colour = "red")

model.nls <- nls(v ~ Vm * S/(K+S), data = mm, 
                 start = list(K = max(mm$v)/2, Vm = max(mm$v)))

summary(model.drm)

summary(model.nls)