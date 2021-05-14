
# ---------------- #
# Lectura de datos #
# ---------------- #

datos_empleo = read.csv2("empleo_discriminante2.csv")
datos_empleo$Situacion = as.factor(datos_empleo$Situacion)
datos_empleo
attach(datos_empleo)

# ----------------------------- #
# Análisis exploratorio inicial #
# ----------------------------- #

library(GGally)
summary(datos_empleo) # univariado
ggpairs(datos_empleo) # bivariado
hist(Edad)
hist(Hijos18)
hist(NumTrab)
sd(Edad)
3*(36.93 - 38.00)/10.90652 # -0.2943
sd(Hijos18)
3*(1.028 - 1)/0.8101469 # 0.1036
sd(NumTrab)
3*(3 - 3)/1.723783 # 0

# ----------------------- #
# Evaluación de supuestos #
# ----------------------- #

library(MVN)
mvn(datos_empleo[,-1], mvnTest = "royston")
mvn(datos_empleo[,-1], mvnTest = "mardia")

library(dplyr)
datos_empleo %>% 
  filter(Situacion=="0") %>% 
  select(Edad,Hijos18,NumTrab) %>% 
  cov()

datos_empleo %>% 
  filter(Situacion=="1") %>% 
  select(Edad,Hijos18,NumTrab) %>% 
  cov()

datos_empleo %>% 
  filter(Situacion=="2") %>% 
  select(Edad,Hijos18,NumTrab) %>% 
  cov()

library(biotools) # H0: Matrices de cov iguales # H1: Al menos una dif
boxM(datos_empleo[,-1], grouping = datos_empleo[, 1])

# ------ #
# Modelo #
# ------ #

library(MASS)
modelo_disc2 = lda(formula = Situacion ~ Edad + Hijos18 + NumTrab)
modelo_disc2
modelo_disc2$prior
modelo_disc2$scaling
x11();plot(modelo_disc2)
plot(modelo_disc2, dimen=1, type="b")

library(klaR)
partimat(Situacion ~ Edad + Hijos18 + NumTrab, data=datos_empleo, method="lda", nplots.hor=3)

# Predicción para datos observados

predicciones_disc2 = predict(modelo_disc2)
predicciones_disc2

# 9.615981e-08 = 0.000000096159 → 0 # muy poco probable de que sea
# una persona ocupada
# 1.573064e-02 = 0.01573 # es poco probable de que sea una persona
# subocupada
# 9.842693e-01 = 0.98427 # es muy probable de que sea una persona 
# desocupada

round(predicciones_disc2$posterior,3)

comparacion  = data.frame(OBS  = Situacion,
                          PRED = predicciones_disc2$class,
                          PROB = predicciones_disc2$posterior) 
comparacion

# Matriz de confusión

library(caret)
confusionMatrix(comparacion$PRED, comparacion$OBS)

# Predicción para datos no observados

predict(modelo_disc2, data.frame(Edad=50,Hijos18=3,NumTrab=3))

(X = data.frame(Edad=c(50,30,26),Hijos18=c(3,0,1),NumTrab=c(1,3,1)))
predict(modelo_disc2, X)



