#####################################
#    TERCERA PRÁCTICA CALIFICADA    # 
# CLUSTER JERÁRQUICO Y DE PARTICIÓN #
#     Mg. Jesús Salinas Flores      # 
#     jsalinas@lamolina.edu.pe      #
#####################################

# Para limpiar el workspace, por si hubiera algún dataset 
# o información cargada
rm(list = ls())
graphics.off()

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Otras opciones
options(scipen=999)      # Eliminar la notación científica
options(digits = 3)      # Número de decimales

# Paquetes
library(pacman)
p_load(readxl,cluster,aplpack,fpc,foreign,TeachingDemos,
       factoextra, ape,corrplot,DataExplorer,
       funModeling,compareGroups,tidyverse,dendextend,
       FeatureImpCluster,flexclust,LICORS)

#----------------------#
# Descripción del caso #
#----------------------#

# Para las entidades bancarias en el Perú, la tarjeta de 
# crédito es uno de los principales productos ofrecidos al 
# mercado. La competencia entre entidades bancarias por la
# participación en el mercado de tarjetas de crédito es alta, 
# lo que lleva al desarrollo de estrategias para la
# fidelización de los clientes y obtención de nuevos clientes. 
# Mediante la fidelización de los clientes se busca una
# relación a largo plazo y rentabilidad, una forma de esto es
# conociéndolos mediante el análisis de la información 
# disponible del cliente. De esta manera poder brindarles 
# promociones, beneficios y/o servicios que les den un valor
# agregado. 

# Para este caso se tomó como base las transacciones de compra
# con tarjeta de crédito de 5000 clientes durante el periodo de
# agosto 2020 y noviembre 2020. La base esta compuesta por 10
# variables cuantitativas

# Variable      Definición
# RUBROS_TC     Cantidad de rubros diferentes donde se realizó
#               consumo en el periodo
# COMERCIOS_TC  Cantidad de comercios diferentes donde se
#               realizó consumo
# DIAS_POR_MES  Cantidad promedio de días con compra en un mes
# TRX_MERCADO   Cantidad de transacciones en el rubro mercado
# TRX_ROPA      Cantidad de transacciones en el rubro ropa
# TRX_TRANSPORTE Cantidad de transacciones en el rubro transporte durante el periodo
# TRX_COMIDA    Cantidad de transacciones en el rubro comida
# TRX_BOTICA    Cantidad de transacciones en el rubro botica
# TRX_BODEGAS   Cantidad de transacciones en el rubro bodega
# TRX_GRIFO     Cantidad de transacciones en el rubro grifo


library(readxl)
BDD_TARJETAS_DE_CREDITO_ <- read_excel("BDD-TARJETAS DE CREDITO..xlsx")
base_inicial=BDD_TARJETAS_DE_CREDITO_  ;base_inicial

# Completando los valores vacíos con cero
# Los valores perdidos significa que no realizó transaccion en
# dicho rubro
base_inicial$TRX_MERCADO[is.na(base_inicial$TRX_MERCADO)] <- 0
base_inicial$TRX_ROPA[is.na(base_inicial$TRX_ROPA)] <- 0
base_inicial$TRX_TRANSPORTE[is.na(base_inicial$TRX_TRANSPORTE)] <- 0
base_inicial$TRX_COMIDA[is.na(base_inicial$TRX_COMIDA)] <- 0
base_inicial$TRX_BOTICA[is.na(base_inicial$TRX_BOTICA)] <- 0
base_inicial$TRX_BODEGAS[is.na(base_inicial$TRX_BODEGAS)] <- 0
base_inicial$TRX_GRIFO[is.na(base_inicial$TRX_GRIFO)] <- 0

# Creacion de la variable días por mes a partir de días y meses
base_inicial$DIAS_POR_MES <- base_inicial$DIAS_TC/base_inicial$MESES_TC

# Selección de las variables
library(dplyr)
base_inicial %>% select(RUBROS_TC ,COMERCIOS_TC ,DIAS_POR_MES,
                        TRX_MERCADO ,TRX_ROPA ,TRX_TRANSPORTE ,
                        TRX_COMIDA ,TRX_BOTICA ,TRX_BODEGAS ,
                        TRX_GRIFO ) -> datos


datos
#estandarizamos
datosc <- as.data.frame(scale(datos)) ;datosc


# Para cada una de las preguntas comente los resultados 
# obtenidos. Use donde sea necesario set.seed(2021)

# 1. Realice un análisis descriptivo y exploratorio de los 
#    datos (3 puntos)

#analisis exploratorio
library(DataExplorer)
library(ggplot2)
plot_missing(datosc,ggtheme=theme_minimal())
#se puede observar que no tenemos  valores perdidos


#analisis descriptivo
library(zoo)
library(PerformanceAnalytics)
library(xts)
library(graphics)
#pearson "escala de intervalo"#es mejor si el tamaño de muestra es mas grande.
chart.Correlation(datosc, histogram=TRUE, method = c("pearson"), pch=20) 
#se puede observar que hay variables  estan medias correlacionadas  otras altamente correlacionadas,
#es un indicador que puede realizar analisis factorial o analisis de componentes principales.
library(funModeling)
df_status(datosc)
#  variable q_zeros p_zeros q_na p_na q_inf p_inf    type unique
#1       RUBROS_TC       0       0    0    0     0     0 numeric      2
#2    COMERCIOS_TC       0       0    0    0     0     0 numeric     62
#3    DIAS_POR_MES       0       0    0    0     0     0 numeric    140
#4     TRX_MERCADO       0       0    0    0     0     0 numeric    114
#5        TRX_ROPA       0       0    0    0     0     0 numeric     36
#6  TRX_TRANSPORTE       0       0    0    0     0     0 numeric    145
#7      TRX_COMIDA       0       0    0    0     0     0 numeric     89
#8      TRX_BOTICA       0       0    0    0     0     0 numeric     53
#9     TRX_BODEGAS       0       0    0    0     0     0 numeric     78
#10      TRX_GRIFO       0       0    0    0     0     0 numeric     57


# 2. Aplique un análisis cluster jerárquico y por lo menos una
#    técnica de partición. 
#    De las técnicas aplicadas escoja la que considere la 
#    mejor solución (Justifique su respuesta)
#    Para la técnica elegida, indique el número de individuos 
#    por cluster y presente un diagrama de líneas que permita
#    describir el patrón de cada uno de los clusters. 
#    Describa el patrón de cada uno de los clusters. (17 puntos)

#solucion:

# análisis cluster jerárquico

d <- dist(datosc, method = "euclidean")
res.hc <- hclust(d, method = "ward.D2" )
str(res.hc)

################################
#HALLANDO EL NUMERO DE CLUSTER:#
################################

################################
#HALLANDO EL NUMERO DE CLUSTER:#
################################
#distancia
res.hc$height


alturas <- data.frame(etapa=1:4999,distancia=res.hc$height)
library(ggplot2)
ggplot(tail(alturas,10)) + aes(x=etapa,y=distancia)  +       ##10 ultimos  "tail(alturas,10)"
  geom_point() + geom_line()  + 
  scale_x_continuous(breaks=seq(1,4999))+ 
  geom_vline(xintercept = 4997,col="red",lty=2) + 
  theme_bw()
#considero  3 cluster 

# Dividir en 3 clusters
grp <- cutree(res.hc, k = 3)
grp 
table(grp)

datos.j <- cbind(datosc,grp)
str(datos.j)

datos.j$grp <- factor(datos.j$grp)
str(datos.j)

# Diagrama de lineas de promedios por cluster
#--------------------------------------------------

#1. necesito sacar los promedios por cada cluster.


library(dplyr)
datos.j %>%                                                     
  group_by(grp) %>%                                     #group by       :obtengo  todo lo que quiero     ->filtro
  summarise_all(list(mean)) -> medias                   #summarise_all  :obtengo todo lo que quiero      ->filtro  "all"  refiere a "todo"
medias                                                  # 3 x 11

#-------------------------------------------------

#2.sacar los promedios generales por variable.

#forma1:
datos.j %>%  summarise_if(is.numeric,mean) %>%         #sumarise_if  :obtengo un resumen condicionado
  round(4) -> general
general

#forma2:
datos.j %>% select(-grp) %>%                          #como un sql
  summarise_all(list(mean)) -> general
general                                               # 1 x 10

#uniendo  "grp"  y "general"  (columnas)               
general <- cbind(grp="general",general)
general                                              # 1 X 11

#uniendo "medias" y "general" (filas) y al mismo tiempo conviertiendo en un "data.frame"
medias  <- as.data.frame(rbind(medias,general))
medias 

#FORMATO  NO TIDY (SIRVE PARA GRAFICAR)


#forma 1

# Convirtiendo la data formato tidy

library(tidyr)
gathered_datos.j <- gather(data  = medias,
                           -grp,                       #pasame todas las variables(columnas) a filas  excepto la varible "grp"
                           key   = variable, 
                           value = valor) 

gathered_datos.j

#INSUMO:   "el formato no tidy"  < > "gathered_datos.j"
ggplot(gathered_datos.j) + aes(x=variable,y=valor,color=grp) + 
  geom_point() + geom_line(aes(group = grp)) +
  theme_bw() +
  theme(legend.position = "bottom",legend.title=element_blank()) +
  labs(title="Diagrama de líneas de Cluster por Variable",
       x="Variable",y="") + ylim(-3,3) +
  scale_colour_discrete("Cluster") 
coord_flip()
# en el cluster 3   los individuos que tienen alto rubro_tc  y trx_tranporte son buenos clientes
#el cluster 2 tienen alto comercio_tc , trx_bodegas, .... trx_ropa  estos son cliente ideales

#¿cuantos individuos tengo en cada grupo?
table(datos.j$grp)
#1    2    3 
#3029  958 1013
# en el cluster 1  hay 3029 individuos
# en el  cluster 2  hay 958 individuos
# en el cluster 3  hay 1013 individuos


#--------------------------------------------
#De las 3 variaciones del k means opte por usar  el algoritmo Hartigan-Wong
#por que es mas estable.
#usando el algoritmo Hartigan-Wong

set.seed(2021)
km <- kmeans(datosc, 
             centers=3,      # Número de Cluster
             iter.max = 100, # Número de iteraciones máxima
             nstart = 25,  # Número de puntos iniciales(25 SOLUCIONES,25 partidas inciales)
             algorithm="Hartigan-Wong")

#numero de iteracciones 
km$iter
#3
################################
# suma de cuadrados intra total#  ---- 3 cluster
################################
#(Suma de cuadrados intra del cluster1+Suma de cuadrados intra del cluster2 +Suma de cuadrados intra del cluster3 )
km$tot.withinss 
#35770.07

#cuando probe con 2 cluster  la suma de cuadrados intra era superior 35770.07 es otra razon por la cual elige 3 cluster

#GRAFICA
library(factoextra)
fviz_cluster(km, data = datosc, ellipse.type = "convex") +
  theme_classic()

#se puede observar que con 3 cluster  se explica a una inertia total de 46.6% 


#CORROBORANDO
#
#
# Análisis de Componentes Principales con el paquete ade4
library(ade4)
acp <- dudi.pca(datosc,scannf=FALSE,nf=2)
summary(acp)
#Cumulative projected inertia (%):
#  Ax1   Ax1:2   Ax1:3   Ax1:4   Ax1:5 
#33.94   46.67   56.68   65.87   74.70 

# Valores propios
acp$eig
#3.39387107 1.27280658 1.00085818 0.91910606 0.88355890 0.80304626 0.70272131 0.57166626 0.36539886 0.08696652
#con 2 componentes  se retiene 46.67 de la inertia total
