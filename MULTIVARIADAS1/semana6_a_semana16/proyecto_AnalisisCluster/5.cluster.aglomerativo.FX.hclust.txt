library(foreign)
datosc <- read.spss("compras-cluster.sav",
                    use.value.labels=TRUE, 
                    max.value.labels=TRUE,
                    to.data.frame=TRUE)

attr(datosc,"variable.labels") <- NULL
datosc$caso <- NULL
str(datosc)

#si estandarizamos

datos.e <- as.data.frame(scale(datosc))#datos originales#estan en la misma escala 1-7
# a todos los datos lo esta restando la media de la colunma1
# diviendiendo entre la desviacion estandar de la columna1. 
#donde Xi es un dato.21 filas x 6 columnas = 126 datos # hay 21 individuos
#(Xi-media_columna)/(desviacion columna)

####################################### 
#CLUSTER JERARQUICO AGLOMERATIVO:AGNES#
#######################################

#-----------------------------------
#calculando la matriz de disimilaridad
#usando la distanicia euclidiana

d <- dist(datosc, method = "euclidean")
res.hc <- hclust(d, method = "average" )#insumo "d":matriz distancia(euclidiana) y metodo de enlace(vinculacion promedio)

# method = ward.D, ward.D2, single, complete, average, median
#metodo de enlace mas usado:average  y ward

str(res.hc)#en el "merge" , observamos la cantidad de etapas 1:20

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

################################
#HALLANDO EL NUMERO DE CLUSTER:#
################################

alturas <- data.frame(etapa=1:20,distancia=res.hc$height)
alturas
ggplot(alturas) + aes(x=etapa,y=distancia)  +
  geom_point() + geom_line()  + 
  scale_x_continuous(breaks=seq(1,20)) + 
  geom_vline(xintercept = 18,col="red",lty=2) + 
  theme_bw()
# Dividir en 3 clusters
grp <- cutree(res.hc, k = 3)
grp

############
#DEDONGRAMA#
############

library(factoextra)
fviz_dend(res.hc, k=3, cex = 0.5,
          k_colors = rainbow(3),   # Colores del arco iris
          color_labels_by_k = TRUE, 
          rect=T)

# rainbow() topo.colors() heat.colors()
colors()

#########################
#GRAFICA#ACP con CLUSTER#
#########################

library(factoextra)
fviz_cluster(list(data = datosc, cluster = grp),
             palette = c("#2E9FDF",  "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = T, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_bw())

# Junta el archivo de datos con la columna de cluster
datos.j <- cbind(datosc,grp)
str(datos.j)

datos.j$grp <- factor(datos.j$grp)
str(datos.j)
###library(colourpicker)
# write.csv(datos.j,"Compras con Jerarquico Aglomerativo.csv")
#
#
#CORROBORANDO
#
#
# Análisis de Componentes Principales con el paquete ade4
library(ade4)
acp <- dudi.pca(datosc,scannf=FALSE,nf=ncol(datosc))
summary(acp)

# Valores propios
acp$eig

inertia.dudi(acp)

# Correlaciones entre las variables y los componentes
acp$co[c(1,2)]

# Grafica de Valores propios - ScreePlot
fviz_eig(acp, addlabels=TRUE, hjust = -0.3,
         barfill="white", barcolor ="darkblue",
         linecolor ="red") + ylim(0,80) + theme_minimal()

# Scores o Puntuaciones de cada individuo
acp$li[1:10,]

# Gráfica de individuos sobre el primer plano de componentes
s.label(acp$li,clabel=0.7,grid=FALSE,boxes=FALSE)