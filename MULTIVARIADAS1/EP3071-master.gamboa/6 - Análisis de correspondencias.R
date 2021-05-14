
# ---------------- #
# Lectura de datos #
# ---------------- #

library(readxl)
datos = read_xlsx('Bolsa.xlsx')
datos$DISTRITO = factor(datos$DISTRITO)
datos$BOLSA    = factor(datos$BOLSA, ordered = T,
                        levels = c("NUNCA","A VECES","REGULARMENTE","SIEMPRE"))

# ------------------ #
# Gráficos iniciales #
# ------------------ #

tabla = table(datos)

mosaicplot(tabla)
mosaicplot(tabla, col=c("dodgerblue","gold","mediumorchid","red"), main="")

library(gplots)
balloonplot(tabla)
balloonplot(t(tabla))

library(ggplot2) # paquete netamente para gráficas
library(dplyr)
datos %>% 
  count(BOLSA) %>% 
  ggplot(aes(x = BOLSA, y = n, label = n)) + 
  geom_bar(stat = "identity", fill = "gold") + 
  geom_text(position = position_stack(vjust = 0.5)) + 
  labs(x = "Frecuencia con la que pide bolsa",
       y = "Número de personas") + 
  theme_minimal()

# Link para ver más colores
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf?utm_source=twitterfeed&utm_medium=twitter

datos %>% 
  count(BOLSA,DISTRITO) %>% 
  ggplot(aes(x = BOLSA, y = n, label = n, fill = DISTRITO)) + 
  geom_bar(stat = "identity") + 
  geom_text(position = position_stack(vjust = 0.5)) + 
  labs(x = "Frecuencia con la que pide bolsa",
       y = "Número de personas") + 
  theme_minimal()

datos %>% 
  count(BOLSA,DISTRITO) %>% 
  ggplot(aes(x = DISTRITO, y = n, label = n, fill = BOLSA)) + 
  geom_bar(stat = "identity") + 
  geom_text(position = position_stack(vjust = 0.5)) + 
  labs(x = "Distrito",
       y = "Número de personas") + 
  scale_fill_manual(values=c("firebrick","gold","dodgerblue","forestgreen"))+
  theme_minimal()

# ------------------------------------------- #
# Viabilidad del análisis de correspondencias #
# ------------------------------------------- #

chisq.test(tabla)

# ---------------------------- #
# Análisis de correspondencias #
# ---------------------------- #

library(FactoMineR) # Para la función CA
library(FactoClass) # Para la función plotct

# Perfiles
corresp  = CA(tabla, graph = FALSE)
perfiles = plotct(tabla, 
                  profiles    = "none", # no grafica los perfiles
                  tables      = T) # muestra las tablas de perfiles

perfiles$ctm

perfiles$perR
plotct(tabla,"row",col=c("dodgerblue","gold","mediumorchid","red"))

perfiles$perC
plotct(tabla,"col",col=c("dodgerblue","gold","mediumorchid","red"))

# Masas
corresp$call$marge.col*100
corresp$call$marge.row*100

# Número de dimensiones 
(ndime = min(ncol(tabla),nrow(tabla))-1) # núm. máx. dimensiones
library(factoextra)
get_eigenvalue(corresp)
fviz_screeplot(corresp, addlabels = TRUE)

(media = 100/ndime)

fviz_screeplot(corresp, addlabels = TRUE) + 
  geom_hline(yintercept = media, col = "red", size = 2)

# Gráficos
corresp$row$coord
fviz_ca_row(corresp, repel = TRUE)

corresp$col$coord
fviz_ca_col(corresp, repel = TRUE)

fviz_ca_biplot(corresp)

plot(corresp,map="symbioplot")

# De lo más similar a lo más diferente
# 1. Puntos cercanos (en el mismo cuadrante de preferencia)
# implica mayor similitud
# 2. Puntos en distintos cuadrantes (hacia el lado o 
# hacia abajo), aquí comienzan las diferencias
# 3. Diferencias más marcadas: cuadrantes 
# diametralmente opuestos
# Puntos cerca al origen no muestran patrones asociados

# Asociación entre categoría y eje
corresp$row$cos2
corresp$col$cos2

# Contribución de cada categoría al eje
corresp$row$contrib
fviz_contrib(corresp, choice = "row", axes = 1)
fviz_contrib(corresp, choice = "row", axes = 2)
fviz_contrib(corresp, choice = "row", axes = 3)

corresp$col$contrib
fviz_contrib(corresp, choice = "col", axes = 1)
fviz_contrib(corresp, choice = "col", axes = 2)
fviz_contrib(corresp, choice = "col", axes = 3)

# Resumen
summary(corresp)

# ----------------------------------------- #
# Análisis de correspondencias (paquete ca) #
# ----------------------------------------- #

library(ca)
corresp2 = ca(tabla)
corresp2
plot(corresp2)

