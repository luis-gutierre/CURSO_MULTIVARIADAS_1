library(readxl)
datos = read_excel("energia.xlsx")

modelo = lm(Y~X1+X2, data = datos)
summary(modelo)
summary(aov(modelo))

library(dplyr)
modelo %>% aov() %>% summary()

residuales = residuals(modelo)

plot(datos$Y,residuales,xlab="Gasto en energía",pch=18)
abline(h=0)
library(car)
ncvTest(modelo)

plot(residuales,xlab="posición",pch=18)
abline(h=0)
library(lmtest)
dwtest(modelo, alternative = c("two.sided"))

par(mfrow=c(2,2))
hist(residuales)
hist(residuales,nclass=8)
hist(residuales,breaks=c(-150,-100,-50,0,50,100,150))
hist(residuales,breaks=c(-150,0,150))
library(ggpubr)
ggdensity(residuales)
shapiro.test(residuales)
library(nortest)
ad.test(residuales)
ks.test(residuales,"pnorm")

plot(fitted(modelo),residuales,xlab="Y ajustado",pch=18)
abline(h=0)

vif(modelo)

step(modelo, direction=c("backward"))
step(modelo, direction=c("forward"))
step(modelo, direction=c("both"))

predict(modelo, data.frame(X1=52,X2=4))
predict(modelo, data.frame(X1=c(52,56),X2=c(4,2)))


predict(modelo, data.frame(X1=52,X2=4),interval = "confidence")
predict(modelo, data.frame(X1=52,X2=4),interval = "prediction")
