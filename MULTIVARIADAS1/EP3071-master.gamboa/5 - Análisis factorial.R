
# ---------------- #
# Lectura de datos #
# ---------------- #

library(readxl)
datos = read_excel("MotivosCompra.xlsx")

# --------------------- #
# Análisis exploratorio #
# --------------------- #
# install.packages("skimr")
summary(datos)
library(skimr)
skim(datos)
cor(datos)
round(cor(datos),2)
library(Hmisc)
library(dplyr)
rcorr(as.matrix(datos))
datos %>% as.matrix %>% rcorr # esta línea es equivalente a la anterior

library(corrplot)
corrplot(cor(datos))
datos %>% cor %>% corrplot # esta línea es equivalente a la anterior
datos %>% cor %>% corrplot(method="square")
datos %>% cor %>% corrplot(method="ellipse")
datos %>% cor %>% corrplot(method="pie")
datos %>% cor %>% corrplot(method="shade")
datos %>% cor %>% corrplot(method="color")
datos %>% cor %>% corrplot(method="number")
datos %>% cor %>% corrplot(type="lower")
datos %>% cor %>% corrplot(method="square",type="upper")
datos %>% cor %>% corrplot(method="number",type="lower")
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(datos)
datos %>% chart.Correlation

datos %>% cor %>% heatmap
library(dichromat)
datos %>% cor %>% heatmap(col = colorRampPalette(c("blue", "white", "red"))(20))

# Más ejemplos: https://rstudio-pubs-static.s3.amazonaws.com/240657_5157ff98e8204c358b2118fa69162e18.html

# --------------------------------- #
# Viabilidad del análisis factorial #
# --------------------------------- #

library(psych)
psych::KMO(datos)
library(EFAtools)
EFAtools::KMO(datos)

cortest.bartlett(cor(datos), n = 433) # n = tamaño de muestra
cortest.bartlett(datos) # internamente calcula la matriz de correlación
cortest.bartlett(cor(datos)) # incorrecto porque falta especificar n

# ------------------ #
# Análisis factorial #
# ------------------ #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Modelo 1 - Sin rotación - 5 factores #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

modelo1 = fa(datos, nfactors = 5, rotate = "none") 

# Comunalidad y especificidad
round(modelo1$communalities,2)
round(modelo1$uniquenesses,2)
round(modelo1$model,2)
round(cor(datos),2)

# Cargas
modelo1
modelo1$loadings
print(modelo1$loadings,cutoff = 0.01)
print(modelo1$loadings,cutoff = 0.90)
print(modelo1$loadings,cutoff = 0.30) # sugerido para descartar las relaciones débiles

(-0.75)^2+0.00^2+(-0.04)^2+0.42^2+0.33^2
(-0.01)^2+0.30^2+0.39^2+0.01^2+0.09^2

# Índice de complejidad
modelo1$complexity

# Porcentaje de variabilidad explicado
modelo1$Vaccounted

# Correlación entre factores
cor(modelo1$scores)
corrplot(cor(modelo1$scores))
modelo1$scores %>% cor %>% corrplot # esta línea es equivalente a la anterior

# Número de factores
modelo1$e.values 
plot(modelo1$e.values, type = "b", pch = 18)
sum(modelo1$e.values)

modelo = fa(datos,nfactors = 11, rotate = "none") 
modelo$Vaccounted

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Modelo 2 - Sin rotación - 4 factores #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

modelo2 = fa(datos,nfactors = 4, rotate = "none") 
modelo2$loadings
modelo2$Vaccounted
modelo2$scores
cor(modelo2$scores)
corrplot(cor(modelo2$scores))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Modelo 3 - Rotación varimax - 4 factores #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

modelo3 = fa(datos,nfactors = 4, rotate = "varimax") 
modelo3$loadings
modelo3$Vaccounted
cor(modelo3$scores)
corrplot(cor(modelo3$scores))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Modelo 4 - Rotación oblimin - 4 factores #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

modelo4 = fa(datos,nfactors = 4, rotate = "oblimin") 
modelo4$loadings
modelo4$Vaccounted
cor(modelo4$scores)
corrplot(cor(modelo4$scores))

# Ajuste del modelo

modelo1$TLI # 5 factores, sin rotación
modelo2$TLI # 4 factores, sin rotación
modelo3$TLI # 4 factores, rotación varimax
modelo4$TLI # 4 factores, rotación oblimin

modelo1$RMSEA
modelo2$RMSEA
modelo3$RMSEA
modelo4$RMSEA

modelo1$BIC
modelo2$BIC
modelo3$BIC
modelo4$BIC

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Modelo con método de máxima verosimilitud #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

modelo1.mv = psych::fa(datos, nfactors = 5,rotate="none",fm="ml") 
modelo1_mv = factanal(datos, factors = 5) # máxima verosimilitud # 

modelo1.mv$loadings
modelo1_mv$loadings

modelo1.mv = psych::fa(datos, nfactors = 4,rotate="none",fm="ml") 
modelo1_mv = factanal(datos, factors = 4) # máxima verosimilitud # 

modelo1.mv$loadings
modelo1_mv$loadings

modelo2.mv = psych::fa(datos, nfactors = 4, rotate = "varimax",fm="ml") 
modelo2_mv = factanal(datos, factors = 4, rotation="varimax") # máxima verosimilitud # 

modelo2.mv$loadings
modelo2_mv$loadings

# Máxima verosimilitud
# F1 ~ X1, X2, X4, X9
# F2 ~ X3, X5, X7
# F3 ~ X8, X11
# F4 ~ X10, X6

# Mínimo residual
# F1 ~ X1, X2, X4, X9
# F2 ~ X3, X5, X7
# F3 ~ X8, X11
# F4 ~ X6, X10

# F1 