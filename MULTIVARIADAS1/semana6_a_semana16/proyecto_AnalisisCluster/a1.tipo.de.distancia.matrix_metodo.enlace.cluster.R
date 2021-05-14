library(pacman)
p_load(cluster,aplpack,fpc,foreign,TeachingDemos,
       factoextra,NbClust,ape,corrplot,DataExplorer,
       funModeling,compareGroups,tidyverse,dendextend,
       igraph,FeatureImpCluster,flexclust,LICORS)
#------------
library(foreign)
datosc <- read.spss("compras-cluster.sav",
                    use.value.labels=TRUE, 
                    max.value.labels=TRUE,
                    to.data.frame=TRUE)

attr(datosc,"variable.labels") <- NULL
datosc$caso <- NULL
str(datosc)
#--------------
##########################
#II.ANALISIS EXPLORATORIO#
##########################

summary(datosc)#1-7

library(DataExplorer)

plot_str(datosc)#EQUIVALENTE A UN STR

# Detectando y graficando los % de datos perdidos
plot_missing(datosc,ggtheme=theme_minimal()) #importante
#no tengo datos perdidos 'NA'

#voy a simular datos perdidos 'NA'
fix(datosc)
# Detectando y graficando los % de datos perdidos
plot_missing(datosc,ggtheme=theme_minimal()) #importante datos perdidos en porcentaje
#se puede cambiar 'help'

library(mice)
md.pattern(datosc, rotate.names = TRUE)#importante datos perdidos en numeros



# Histograma para las variables numéricas
plot_histogram(datosc)#datos faltantes

plot_density(datosc)#

create_report(datosc)#crea un html un reporte.

#-------------------------------------------------------------
library(funModeling) # Pablo Casas  Libro Vivo de Ciencia de Datos
# Udemy R tidyverse

# Descripción de los datos
df_status(datosc)

# Gráfico de variables numéricas
plot_num(datosc)#me esta dando un histograma

# Descripción de las variables numéricas
profiling_num(datosc)# da mas cosas que el summary

#################################
#III.USANDO MEDIDAS DE DISTANCIA#
#################################


#-----------------------------------------------------
datos.e <- as.data.frame(scale(datosc))#datos originales#estan en la misma escala 1-7
# a todos los datos lo esta restando la media de la colunma1
# diviendiendo entre la desviacion estandar de la columna1. 
#donde Xi es un dato.21 filas x 6 columnas = 126 datos # hay 21 individuos
#(Xi-media_columna)/(desviacion columna)


str(datos.e)#datos estandarizados

#----------MATRIZ-DISTANCIA-se usara de insumo.


#----------------------------------------------------
# 1.a Calculando la matriz de distancia euclidiana #Es una matrix de 21* 21 xq son 21 individuos.
#     con la funcion dist()

#insumo datos originales(21 filas y 6 columnas),ya que estan
#en la misma escala(1-7).

dist.eucl <- dist(datosc, method = "euclidean")#distancia euclidiana 21 *21 


# Visualizando un subconjunto de la matriz de distancia
round(as.matrix(dist.eucl)[1:6, 1:6], 1)#sacame los 6 filas y 6 columas rendondeado a 1 cifra
#matrix simetrica

# Otras distancias: "maximum", "manhattan", "minkowski"

#-------------------------------------------------------------
# 1.b Calculando la matriz de distancia euclidiana 
#     con la función daisy()

library(cluster)
dist.eucl2 <- daisy(datosc, metric= "euclidean")

# Visualizando un subconjunto de la matriz de distancia
round(as.matrix(dist.eucl2)[1:6, 1:6], 1)

# Otras distancias: "gower", "manhattan"
#---------------------------------------------------------------
# 1.c Calculando la matriz de distancia euclidiana con la
#     funcion get_dist()
library(factoextra)
res.dist <- get_dist(datosc, stand = FALSE, #¿quieres estandarizar o no quieres estandarizar tus datos originales?
                     method = "euclidean")    #rpta:depende de la escala
round(res.dist,1)
# Visualizando un subconjunto de la matriz de distancia
round(as.matrix(res.dist)[1:6, 1:6], 1)

# Otras distancias: "pearson", "kendall", "spearman"
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
# 2.a Visualizando la matriz de distancia con fviz_dist()
fviz_dist(res.dist)#rojo :mas parecido   #azul:menos parecido  #paquete "factoextra"
#reagrupa los individuos en grupos o cluster,podemos visualizar 3 cluster

fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", 
                          high = "#FC4E07"))

# 2.b Visualizando la matriz de distancia con corrplot()
library(corrplot)
corrplot(as.matrix(dist.eucl), 
         is.corr = FALSE, 
         method = "color")

# 2.c Visualizando solo el triangulo superior
corrplot(as.matrix(dist.eucl), is.corr = FALSE, 
         method = "color",
         order="hclust", type = "upper")

#-------------------------------------------------------------
# Mapas de calor y dendrograma 
distan <- as.matrix(dist.eucl)
heatmap(distan,xlab="Individuos",
        ylab="Individuos", 
        main="Mapa de Calor del Ejemplo de Compras")
####################################### 
#CLUSTER JERARQUICO AGLOMERATIVO:AGNES#
#######################################

#-----------------------------------
#calculando la matriz de disimilaridad
#usando la distanicia euclidiana

d <- dist(datosc, method = "euclidean")

# method = euclidean, maximum, manhattan, canberra, 
#          binary, minkowski

as.matrix(d)[1:6, 1:6]

#-------------------------------------------------------------
# Cluster Jerárquico usando el método de enlace average
res.hc <- hclust(d, method = "average" )#insumo "d":matriz distancia(euclidiana) y metodo de enlace(vinculacion promedio)

# method = ward.D, ward.D2, single, complete, average, median
#metodo de enlace mas usado:average  y ward
res.hc

str(res.hc)

# Proceso de agrupamiento indicando los individuos
res.hc$merge#individuos son lo mas parecidos
#      [,1] [,2]
#[1,]   -6   -7          etapa1:el individuo "6" y "7" forman un cluster; en total hay 19 cluster
#[2,]  -14  -16          etapa2:el individuo "14" y "16" forman un cluster; en total hay 18 cluster
#[3,]   -2  -13
#[4,]   -3   -8
#[5,]   -5  -11
#[6,]  -21    1          etapa6:el individuo "21" y los individuos de la etapa "1" forman un cluster;el numero de cluster se ira aproximando a 1
#[7,]  -12    6
#[8,]   -4    2
#[9,]   -9    5
#[10,]   -1    7
#[11,]  -19    8
#[12,]  -20    9
#[13,]  -17   10
#[14,]    3   12
#[15,]    4   13
#[16,]  -10   11
#[17,]  -15   15
#[18,]  -18   16
#[19,]   14   18
#[20,]   17   19        etapa20: todos los individuos ;un solo cluster.


# Proceso de agrupamiento indicando los distancias
res.hc$height#altura
# [1] 1.414214 1.414214 1.732051 1.732051 1.732051 1.732051 1.910684 1.984059 2.118034 2.360574 2.641223 2.681366
#[13] 2.817189 3.199661 3.356651 3.379786 3.726555 4.742100 6.059012 6.693697#20 valores<>20 etapas

#el individuo "6" y "7" estan en una distancia de 1.414214 
#el individuo "14" y "16" estan en una distancia de 1.414214
#el individuo "2" y "13" estan en una distnacia de 1.732051 
#¿un cambio brusco?#histograma 20 valores <> 20 etapas
#3.726555(posicion17) 4.742100 (posicion18) si corto aca cuento de izquierda(3.726555) a derecha y solo me quedaria con 4 cluster
#4.742100 6.059012      3 cluster


head(res.hc$height)
tail(res.hc$height)

#-------------------------------------------------------------
# Visualizar el dendrograma
plot(res.hc, cex = 0.6, hang=-1)  # Opciones ó Argumentos

rect.hclust(res.hc, k = 3, border = 2:5) # De segundo nivel#dame el dedongrama con 3 cluster

# Dendograma con el paquete factoextra
library(factoextra)

fviz_dend(res.hc, cex = 0.5)