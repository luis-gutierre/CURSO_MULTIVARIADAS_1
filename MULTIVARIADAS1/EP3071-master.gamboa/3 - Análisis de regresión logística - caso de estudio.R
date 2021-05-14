library(readxl)
datos = read_excel("incumplimiento.xlsx")

datos$incumplimiento = as.factor(datos$incumplimiento)
datos$minimo         = as.factor(datos$minimo)
datos$tipo           = as.factor(datos$tipo)

summary(datos)
attach(datos)

table(incumplimiento,minimo)

library(dplyr)
table(incumplimiento,minimo) %>% prop.table(margin=1)
table(incumplimiento,minimo) %>% prop.table(margin=2)
plot(minimo,incumplimiento,ylab="incumplimiento",xlab="realiza pago mínimo")

table(incumplimiento,tipo)
table(incumplimiento,tipo) %>% prop.table(margin=2)
plot(tipo,incumplimiento,ylab="incumplimiento",xlab="tipo de tarjeta")

library(DataExplorer)
plot_correlation(datos)

# Primera propuesta

modelo_logistico1 = glm(incumplimiento~sueldo+numeroi+minimo+tipo, family = "binomial")
summary(modelo_logistico1)

modelo_nulo = glm(incumplimiento~1, family = "binomial")
library(lmtest)
TestRV = lrtest(modelo_nulo,modelo_logistico1)
TestRV

# Segunda propuesta

modelo_logistico2 = glm(incumplimiento~sueldo+numeroi+minimo, family = "binomial")
summary(modelo_logistico2)

# ¿Esta segunda propuesta es mejor que un modelo nulo?
# H0: Modelo propuesto 2 no ajusta, elegir nulo
# H1: Modelo propuesto 2 sí ajusta, elegirlo
(TestRV = lrtest(modelo_nulo,modelo_logistico2))
# Pvalor < 0.05, rechaza H0, se elige modelo propuesto 2

# Entonces, esta segunda propuesta es mejor que un modelo nulo,
# pero, ¿será mejor que la primera propuesta?
# H0: Modelo propuesto 1 no ajusta, elegir modelo propuesto 2
# H1: Modelo propuesto 1 sí ajusta, elegirlo
(TestRV = lrtest(modelo_logistico2,modelo_logistico1))
# Pvalor > 0.05, no se rechaza Ho, se elige modelo propuesto 2

# Interpretación de coeficientes

exp(coef(modelo_logistico2))
modelo_logistico2 %>% coef() %>% exp()

# el intercepto va a tener interpretación cuando todas las X puedan tomar valor cero.
# X1 = Sueldo ← para el contexto no podría tomar el valor de 0
# X2 = Numero de incumplimientos ← sí podría ser 0
# X3 = Pago minimo ← si podría ser 0
# Si pudiésemos interpretarlo
# e^beta0 = pi/(-pi)
# 0.2121 = pi/(1-pi) → pi = 0.17 sería la probabilidad de incumplimiento cuando x1=0,x2=0,x3=0

# por cada S/ 1000 adicionales de sueldo, la chance de incumplir el pago mensual respecto
# a cumplirlo disminuye en 60.95%, manteniendo el número de incumplimientos y su situación respecto
# al pago mínimo constantes

# por cada incumplimiento de pago adicional en el último año, la chance de incumplir el pago mensual
# aumenta en 140.78% respecto a cumplirlo, manteniendo el suelo y su situación respecto
# al pago mínimo constantes

# La chance de incumplimiento (respecto a la de cumplimiento) cuando se realiza el pago mínimo
# (cuando x3=1) es 52.5 veces que cuando no realiza el pago mínimo (cuando x3=0)

# En resumen, los factores que incrementan el incumplimiento son:
# - número de incumplimentos en el último año  
# - realizar un pago mínimo
# Mientras que el sueldo es un factor que disminuye el incumplimiento

# Pseudo R² = 1-logLik(modelo)/logLik(modelo.nulo)
TestRV$LogLik
1-TestRV$LogLik[2]/TestRV$LogLik[1]

# Predicciones
predicciones = predict(modelo_logistico2, type=c("response"))
comparacion  = data.frame(OBS  = incumplimiento,
                          PRED = as.factor(round(predicciones,0))) 
table(comparacion)

library(caret)
confusionMatrix(comparacion$PRED, comparacion$OBS, positive = "1")

library(pROC)
rocobj = roc(predicciones, incumplimiento,  auc = TRUE, ci = TRUE  )
plot(rocobj)
plot.roc(rocobj, 
         print.thres = "best",
         print.auc   = TRUE,
         auc.polygon = FALSE,
         max.auc.polygon = FALSE, 
         col  = "darkblue", 
         grid = TRUE )
