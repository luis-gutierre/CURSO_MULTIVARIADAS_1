
# CASO DE ESTUDIO: ANÁLISIS DE REGRESIÓN LINEAL MÚLTIPLE

# Carga de paquetes

library(corrplot) # para la función corrplot
library(PerformanceAnalytics) # para la función chart.Correlation
library(ggplot2)  # para la función ggplot2
library(olsrr)    # para las funciones de selección de variables
library(nortest)  # para la función ad.test
library(lmtest)   # para la función dwtest
library(car)      # para la función vif
library(dplyr)    # para usar el operador pipe %>% 

# Lectura de datos

datosreg = read.table("CasoRegresionLineal.txt", header = TRUE)

# Análisis exploratorio 

datosreg$AdultoMayor = as.factor(datosreg$AdultoMayor)
datosreg$Secundaria  = as.factor(datosreg$Secundaria)

summary(datosreg)

# Análisis exploratorio cuantitativo

datosreg2 = datosreg[,-c(4,6)]
cor(datosreg2)
corrplot(cor(datosreg2))
corrplot(cor(datosreg2),addCoef.col = "black")
heatmap(x = cor(datosreg2),symm = TRUE)
chart.Correlation(datosreg2, histogram=TRUE, pch=19)

# Análisis exploratorio cuantitativo y cualitativo
boxplot(datosreg$Area)
boxplot(datosreg$Area~datosreg$Secundaria)
boxplot(datosreg$Area~datosreg$AdultoMayor)

datosreg %>% 
  ggplot(aes(x=as.factor(Secundaria),y=Area)) +
  geom_boxplot(fill = "dodgerblue2") + 
  geom_jitter(width=0.1, colour="darkblue") +
  labs(x = "Estudió Secundaria",
       y = "Area") +
  theme_minimal()
  
# Modelo 

modelo = lm(Area~Ingreso+Familia+AdultoMayor+Posgrado+Secundaria, data = datosreg)
summary(modelo)

# Selección de variables

ols_step_forward_p(modelo, details = TRUE, penter = 0.05)
ols_step_backward_p(modelo, details = TRUE, prem = 0.05)
ols_step_both_p(modelo, details = TRUE, penter = 0.05, prem = 0.05)

ols_step_forward_aic(modelo, details = TRUE)
ols_step_backward_aic(modelo, details = TRUE)
ols_step_both_aic(modelo, details = TRUE)

modelof = lm(Area~Ingreso+Familia+AdultoMayor, data = datosreg)
summary(modelof)

# Area_estimada = 82.69 + 1.15Ingreso + 32.29Familia + 52.19AdultoMayor

# beta0 = 82.69, no tendría sentido su interpretación porque si Familia = 0...

# beta1 = 1.15, el área estimada / promedio se incrementa en 1.15m² cuando
# el ingreso se incrementa en 1000 soles anuales, manteniendo las
# demás características / variables constantes

# beta2 = 32.29, el área estimada se incrementa en 32.29 m² cuando 
# la familia tiene un miembro más, manteniendo las
# demás características / variables constantes

# beta3 = 52.19, el área estimada cuando hay un adulto mayor (1) en la
# familia es 52.19 m² mayor que cuando no lo hay (0).

# Verificación de supuestos

residuales = residuals(modelof)

hist(residuales,main="Histograma de los residuales")
shapiro.test(residuales)
ad.test(residuales)

plot(datosreg$Area,residuales,pch=18,main="Area (Y) vs residuales")
abline(h=0)
ncvTest(modelo)

plot(residuales,type="b",pch=18,main="Índice vs residuales")
abline(h=0)
dwtest(modelof, alternative="two.sided")

plot(fitted(modelof),residuales,pch=18,main="Valores ajustados vs residuales")
abline(h=0)

# Multicolinealidad

vif(modelof)

# Predicción

predict(modelof, data.frame(Ingreso=100,Familia=3,AdultoMayor="1"))
predict(modelof, data.frame(Ingreso=100,Familia=3,AdultoMayor="1"),interval="confidence")
predict(modelof, data.frame(Ingreso=100,Familia=3,AdultoMayor="1"),interval="prediction")

X = data.frame(Ingreso=c(100,150,200),Familia=c(3,4,3),AdultoMayor=c("0","0","1"))
predict(modelof, X)
predict(modelof, X,interval="confidence")
predict(modelof, X,interval="prediction")
predict(modelof, X,interval="prediction",level=0.99)
