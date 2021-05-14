
# ---------------- #
# Lectura de datos #
# ---------------- #

datos = read.table("maestria.txt", header = TRUE)

# ¿Qué pasa si intentamos modelar a través de una regresión lineal?
modelo_lineal = lm(Interes~Edad, data = datos)
predict(modelo_lineal, data.frame(Edad=c(25,35,45,55)))


# ---------------- #
# Modelo logístico #
# ---------------- #

datos$Interes = as.factor(datos$Interes)
modelo_logistico = glm(Interes~Edad, data = datos, family = "binomial")

# ------------------ #
# Resumen del modelo #
# ------------------ #
summary(modelo_logistico)
coef(modelo_logistico)

# -------------------------- #
# Prueba de hipótesis global #
# -------------------------- #

# Modelo nulo (sin variables) vs modelo en estudio
modelo_nulo = glm(Interes~1, data = datos, family = "binomial")
library(lmtest)
(TestRV = lrtest(modelo_nulo,modelo_logistico))

# Si se trabaja al revés, se obtiene el mismo pvalor pero los modelos se 
# invierten. Para evitar confusiones, se sugiere NO EJECUTARLO de la
# siguiente manera:
(TestRV = lrtest(modelo_logistico,modelo_nulo))

# Equivalente solo para regresión logística simple
anova(modelo_logistico, test = "Chisq") 

# ----------- #
# Pseudo - R² #
# ----------- #

(TestRV = lrtest(modelo_nulo,modelo_logistico))
# LogLik del modelo 1 (modelo_nulo) = -44.416
# LogLik del modelo 2 (modelo_logistico) = -32.3229
TestRV$LogLik

# Pseudo R2 = 1- logLik(modelo_logistico)/logLik(modelo_nulo)
1-TestRV$LogLik[2]/TestRV$LogLik[1]

# ---------- #
# Predicción #
# ---------- #

# Primero, se muestra una predicción del componente sistemático x*beta
predict(modelo_logistico, data.frame(Edad=25))
5.77413 - 0.21906*25

# Sin embargo, nuestro interés no es predecir x*beta, sino pi,
# entonces se debe indicar en el argumento type = "response"
# o aplicar la fórmula pi = exp(x*beta)/(1+exp(x*beta))
predict(modelo_logistico, data.frame(Edad=25), type="response")
exp(5.77413 - 0.21906*25)/(1+exp(5.77413 - 0.21906*25))

# Una segunda predicción, para Edad = 53
predict(modelo_logistico, data.frame(Edad=53))
5.77413 - 0.21906*53

predict(modelo_logistico, data.frame(Edad=53),type="response")
exp(5.77413 - 0.21906*53)/(1+exp(5.77413 - 0.21906*53))

# ------------------- #
# Matriz de confusión #
# ------------------- #

# Podemos obtener las predicciones estimadas para cada observación
predicciones = predict(modelo_logistico, type="response")

# Comparamos las observaciones y predicciones
comparacion  = data.frame(OBS  = datos$Interes,
                          PRED = as.factor(round(predicciones,0)))

# A partir de esta comparacion podemos crear nuestra propia tabla de confusión
table(comparacion)
#     PRED
#OBS     0   1
#  0   115   1
#  1    11   3
# 118 (115+3) observaciones correctamente clasificadas
# 1 falso positivo (se observa como 0 y se predice como 1)
# en otras palabras, la persona no piensa estudiar la maestría y se predice como sí
# 11 falsos negativos (se observa como 1 y se predice como 0)
# en otras palabras, la persona sí piensa estudiar la maestría y se predice como no

# También podemos usar la función confusionMatrix, del paquete caret
# La ventaja de esta función es que calcula automáticamente todos los
# indicadores a partir de la matriz
# El primer argumento corresponde a las predicciones y el segundo, las observaciones
library(caret)
confusionMatrix(comparacion$PRED, comparacion$OBS,  positive = "1")
#          Reference
#Prediction   0   1
#         0 115  11
#         1   1   3

# --------- #
# Curva ROC #
# --------- #

library(pROC)
rocobj = roc(predicciones, comparacion$OBS, ci = TRUE)
plot(rocobj)
plot.roc(rocobj,
         legacy.axes = F, 
         print.thres = "best",
         print.auc   = TRUE,
         auc.polygon = F,
         max.auc.polygon = F, 
         col  = "blue", 
         grid = T )

# Para futuras predicciones, se puede utilizar el punto de corte sugerido por la curva ROC

data.frame(datos$Interes, predicciones, predicciones>=0.098)
table(datos$Interes, predicciones>=0.098)
