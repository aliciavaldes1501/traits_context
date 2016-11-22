with(data1comp,summary(glm(n_intact_fruits~n_eggs_max+population,family="poisson")))
model1<-glm(data1comp$n_intact_fruits~data1comp$n_eggs_max+data1comp$population,family="poisson")
range(data1comp$n_eggs_max)
xweight <- seq(0, 37, 0.01)
yweight <- predict(model1, list(wt = xweight),type="response")
plot(data1comp$n_eggs_max, data1comp$n_intact_fruits, pch = 16, xlab = "N eggs", ylab = "N intact fruits")

ggplot(data1comp, aes(n_eggs_max, n_intact_fruits,colour = population)) +
  geom_smooth(method = "glm", se = F, 
  method.args = list(family = "poisson"))+
  geom_point(size = 2)

ggplot(data1comp, aes(most_adv, n_eggs_max,colour = population)) +
  geom_smooth(method = "glm", se = T, 
  method.args = list(family = "poisson"))+
  geom_point(size = 2)

ggplot(data1comp, aes(avg_day_min_ja, n_eggs_max,colour = population)) +
  geom_smooth(method = "glm",  se = T, 
              method.args = list(family = "poisson"))+
  geom_point(size = 2)

ggplot(data1comp, aes(avg_day_min_ja, n_eggs_max,colour = population)) +
  geom_smooth(method = MASS::glm.nb,  se = T)+
  geom_point(size = 2)



