
# ---------------- #
# Lectura de datos #
# ---------------- #

datos_empleo = read.csv2("empleo_discriminante.csv")
datos_empleo
datos_empleo$Situacion = as.factor(datos_empleo$Situacion)
attach(datos_empleo)

# ----------------------------- #
# Análisis exploratorio inicial #
# ----------------------------- #

library(GGally)
ggpairs(datos_empleo)
hist(Edad)
hist(Hijos18)

# ----------------------- #
# Evaluación de supuestos #
# ----------------------- #

library(nortest)
shapiro.test(Edad)
ad.test(Edad)
shapiro.test(Hijos18)
ad.test(Hijos18)

library(MVN)
mvn(datos_empleo[,-1], mvnTest = "royston")
mvn(datos_empleo[,-1], mvnTest = "mardia")

library(dplyr)

datos_empleo %>% # PIPE 
  filter(Situacion=="0") %>% 
  select(Edad,Hijos18) %>% 
  cov()

datos_empleo %>% 
  filter(Situacion=="1") %>% 
  select(Edad,Hijos18) %>% 
  cov()

library(biotools)
boxM(datos_empleo[,-1], grouping = datos_empleo[, 1])

# --------------------------- # 
# Modelo discriminante lineal #
# --------------------------- #

library(MASS)
modelo_disc = lda(formula = Situacion ~ Edad + Hijos18, data = datos_empleo)

# ------------------ #
# Resumen del modelo #
# ------------------ #

modelo_disc
modelo_disc$prior
modelo_disc$scaling

# --------------------------- #
# Visualización de los grupos #
# --------------------------- #

plot(modelo_disc)
library(klaR)
partimat(Situacion ~ Edad + Hijos18, data=datos_empleo, method="lda", nplots.hor=1)

# ---------- #
# Predicción #
# ---------- #

# Predicción para datos observados

predicciones_disc = predict(modelo_disc)
predicciones_disc
predicciones_disc$posterior # Probabilidades a posteriori

# ------------------- #
# Matriz de confusión #
# ------------------- #

comparacion  = data.frame(OBS  = Situacion,
                          PRED = predicciones_disc$class,
                          PROB = predicciones_disc$posterior) 
comparacion
table(comparacion[,1:2])

library(caret)
confusionMatrix(predicciones_disc$class, Situacion, positive = '1')

# --------- #
# Curva ROC #
# --------- #

library(pROC)

probabilidades = apply(predicciones_disc$posterior, 1, max)

rocobj = roc(Situacion, probabilidades, auc = TRUE, ci = TRUE  )
plot.roc(rocobj, 
         print.thres = "best",
         print.auc   = TRUE,
         auc.polygon = FALSE,
         max.auc.polygon = FALSE, 
         col  = "darkblue", 
         grid = TRUE )

# ---------------------------- #
# Predicción para nuevos datos #
# ---------------------------- #

# Supongamos que tenemos el caso de una persona de 50 años con 3 hijos menores
# de 18 años

predict(modelo_disc, data.frame(Edad=50,Hijos18=3))

# Ahora tenemos el caso de 3 personas:
# Una de 50 años con 3 hijos menores de edad
# Otra de 30 años, sin hijos menores de edad
# Una última de 26 años con 1 hijo menor de edad
# Para la predicción primero se construye el data frame con estos datos
# Luego se usa predict

(X = data.frame(Edad=c(50,30,26),Hijos18=c(3,0,1)))
predict(modelo_disc, X)

# ------------------------------------------------ #
# Comparación con el modelo de regresión logística #
# ------------------------------------------------ #

# Modelo
modelo_logistico   = glm(Situacion ~ Edad + Hijos18, data = datos_empleo, family = binomial)

# Predicciones
predicciones_logis = predict(modelo_logistico, type=c("response"))

# Matriz de confusión
confusionMatrix(Situacion, as.factor(round(predicciones_logis,0)), positive = "1")

# Curva ROC
rocobj = roc(Situacion, predicciones_logis, auc = TRUE, ci = TRUE)
plot.roc(rocobj, 
         print.thres = "best",
         print.auc   = TRUE,
         auc.polygon = FALSE,
         max.auc.polygon = FALSE, 
         col  = "darkblue", 
         grid = TRUE)

