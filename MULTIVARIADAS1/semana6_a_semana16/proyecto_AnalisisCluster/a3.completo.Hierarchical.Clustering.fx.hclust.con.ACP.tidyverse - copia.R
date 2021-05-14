#NO HEMOS ESTANDARIZADO LOS DATOS PORQUE ESTAN EN UNA ESCALA DE 1 - 7
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
#CLUSTER JERARQUICO AGLOMERATIVO:######Agrupación jerárquica:Hierarchical Clustering
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
library(ggplot2)
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

res.hc$height
############
#DEDONGRAMA#
############

library(factoextra)
fviz_dend(res.hc, k=3, cex = 0.5,
          k_colors = rainbow(3),   # Colores del arco iris # "3" porque son 3 cluster 
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

############ 
#TIDY.VERSE#
############

#exportar:

#library(rio)
#export(datos.j,"medias.xlsx" )

# Diagrama de lineas de promedios por cluster
#--------------------------------------------------

#1. necesito sacar los promedios por cada cluster.


library(dplyr)
datos.j %>%                                                     
  group_by(grp) %>%                                     #group by       :obtengo  todo lo que quiero     ->filtro
  summarise_all(list(mean)) -> medias                   #summarise_all  :obtengo todo lo que quiero      ->filtro  "all"  refiere a "todo"
medias                                                  # 3 x 7

#remove(general)

#-------------------------------------------------

#2.sacar los promedios generales por variable.

#forma1:
datos.j %>%  summarise_if(is.numeric,mean) %>%         #sumarise_if  :obtengo un resumen condicionado
  round(4) -> general
general

#forma2:
datos.j %>% select(-grp) %>%                          #como un sql
  summarise_all(list(mean)) -> general
general                                               # 1 x 6 

#uniendo  "grp"  y "general"  (columnas)               
general <- cbind(grp="general",general)
general                                               # 1 X 7

#uniendo "medias" y "general" (filas) y al mismo tiempo conviertiendo en un "data.frame"
medias  <- as.data.frame(rbind(medias,general))
medias                                                # 4 x 7   =  3 x 7 (medias) +  1 x 7 (general)


#--------------------------------------------------

#FORMATO TIDY 

#Es cuando es cuando en las filas tienes tienes los clusters y en las columnas tienes las variables. 

#      grp divertid  presupu aprovech buenacom noimport   ahorro
#1       1 5.666667 3.666667 6.000000 3.222222 2.000000 4.000000
#2       2 1.666667 3.000000 1.833333 3.500000 5.500000 3.333333
#3       3 3.500000 5.500000 3.333333 6.000000 3.500000 6.000000
#4 general 3.904762 4.000000 4.047619 4.095238 3.428571 4.380952

#--------------------------------------------------------------------------------------------------

#FORMATO  NO TIDY (SIRVE PARA GRAFICAR)

# Otra forma con pivot_longer
library(tidyverse)
gathered_datos.j <- pivot_longer(data  = medias, 
                                 -grp,
                                 names_to = "variable",
                                 values_to = "valor")

gathered_datos.j

#grp   variable valor
#<fct> <chr>    <dbl>
#  1 1     divertid  5.67
#2 1     presupu   3.67
#3 1     aprovech  6   
#4 1     buenacom  3.22
#5 1     noimport  2   
#6 1     ahorro    4   
#7 2     divertid  1.67
#8 2     presupu   3   
#9 2     aprovech  1.83
#10 2     buenacom  3.5 
# ... with 14 more rows

######
#######trabajando con la media de cada variable por cluster
######


#forma 1

# Convirtiendo la data formato tidy

library(tidyr)
gathered_datos.j <- gather(data  = medias,
                           -grp,                       #pasame todas las variables(columnas) a filas  excepto la varible "grp"
                           key   = variable, 
                           value = valor) 

gathered_datos.j

#forma 2

# Otra forma con pivot_longer
library(tidyverse)
gathered_datos.j <- pivot_longer(data  = medias, 
                                 -grp,
                                 names_to = "variable",
                                 values_to = "valor")

gathered_datos.j
#------------------------------------------------------------------------------------------
######
#######trabajando con la media de cada variable por cluster
######

# general :

#INSUMO:   "el formato no tidy"  < > "gathered_datos.j"


ggplot(gathered_datos.j) + aes(x=variable,y=valor,color=grp) + 
  geom_point() + geom_line(aes(group = grp)) +
  theme_bw() +
  theme(legend.position = "bottom",legend.title=element_blank()) +
  labs(title="Diagrama de líneas de Cluster por Variable",
       x="Variable",y="") + ylim(0,8) +
  scale_colour_discrete("Cluster") 
coord_flip()
#¿cuantos individuos tengo en cada grupo?
table(datos.j$grp)
#1 2 3 
#9 6 6 
round(prop.table(table(datos.j$grp)),2)
#  1    2    3 
#0.43 0.29 0.29 

#NOTA:

#hemos hecho una agrupacion de que la suma de los cuadrados dentro de los cluster sea la minima posible 
#(cohesion)y entre cluster sea la maxima posible(separacion).

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------

####################
#CLUSTER JERARQUICO#
####################


#################################
# 7. Comparando dos dendogramas #
#################################


#metodo de enlace promedio  y metodo de enlace de ward


library(dendextend)

# Calculando la matriz de distancia
res.dist <- dist(datosc, method = "euclidean")

# Calculando 2 clusters jerárquicos aglomerativos
hc1 <- hclust(res.dist, method = "average")#metodo de enlace promedio
sol1=cutree(hc1,3)
hc2 <- hclust(res.dist, method = "ward.D2")#metodo de enlace de ward
sol2=cutree(hc2,3)
cbind(sol1,sol2) #similares 
#sol1 sol2
#[1,]    1    1
#[2,]    2    2
#[3,]    1    1
#[4,]    3    3
#[5,]    2    2
#[6,]    1    1
#[7,]    1    1
#[8,]    1    1
#[9,]    2    2      #igual
#[10,]    3    2     #diferente 
#[11,]    2    2     #igual
#[12,]    1    1
#[13,]    2    2
#[14,]    3    3
#[15,]    1    1
#[16,]    3    3
#[17,]    1    1
#[18,]    3    3
#[19,]    3    3
#[20,]    2    2
#[21,]    1    1
table(sol1,sol2)#observamos que un individuo se paso del cluster3 al cluster2
#       sol2
#sol1  1 2 3
#1     9 0 0
#2     0 6 0
#3     0 1 5
# Creando dos Dendrogramas
dend1 <- as.dendrogram(hc1)
plot(dend1)

dend2 <- as.dendrogram(hc2)
plot(dend2)
# Creando una lista de Dendrogramas
dend_list <- dendlist(dend1, dend2)


#
tanglegram(dend1, dend2)

#la grafica trata de 
#evitar q haya el menor cruze entre las lineas de los individuos 
#sino hay cruze entre las lineas de los individuos indicaria 
#que  ambas  soluciones son practicamente similares(metodo ward o metodo de average(promedio))

#

#-------------------------------------------------------------------

#Entanglement

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
          )
#¿que tan cruzado estan estos 2 dendogramas?

# Entanglement es una medida entre 1 (full entanglement) #recontra cruzado # son diferente
# y 0 (no entanglement).                                 #no hay cruzamiento# los metodos son iguales
# Un bajo coeficiente de entanglement corresponde a 
# un buen alineamiento

# nos conviene que se aproxime a cero porque podriamos decir que son similares(metodos de enlace).

######################################################################################################
#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------

#TIPOS DE GRAFICAS

############
# Cladogram#
############

#INSUMO:

##############################################################
library(ape)
d  <- dist(datosc, method = "euclidean")
hc <- hclust(d, method = "ward.D2")#metodo de enlace ward.D2
##############################################################

#FORMA1:
plot(hc, cex = 0.6, hang=-1)
#FORMA2:
plot(as.phylo(hc), type = "cladogram", cex = 0.6, 
     label.offset = 0.5)

#----------------------------

###########
# Unrooted#
###########

#FORMA1:
plot(as.phylo(hc), type = "unrooted", cex = 0.6,
     no.margin = TRUE)
#FORMA2:
library(igraph)
library(factoextra)
fviz_dend(hc,k=3,k_colors="jco",
          type="phylogenic", repel=TRUE)
#----------------------------

######
# Fan#
######

#FORMA1:
plot(as.phylo(hc), type = "fan")

#FORMA2:
library(factoextra)
fviz_dend(hc, cex= 0.5, k=3, k_colors="jco",
          type="circular")

#----------------------------

#########
# Radial#
#########

#FORMA 1:
plot(as.phylo(hc), type = "radial")



# Dividir el dendrograma en 3 clusters
colors <- c("red", "blue", "green", "black")
clus4  <- cutree(hc, 3)
#1 2 1 3 2 1 1 1 2 2 2 1 2 3 1 3 1 3 3 2 1
colors[clus4]
#"red"   "blue"  "red"   "green" "blue"  "red"   "red"   "red"   "blue"  "blue"  "blue"  "red"   "blue"  "green" "red"  
#"green" "red"   "green" "green" "blue"  "red"
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)
#------------------------------

# Cambiando la apariencia
plot(as.phylo(hc), type = "cladogram", cex = 0.6,
     edge.color = "steelblue", edge.width = 2, edge.lty = 2,
     tip.color = "steelblue")

#------------------------------

######################################################################################################
#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------

############################# 
#METODOS GRAFICOS DE CHERNOF#CARITAS 
#############################

library(aplpack)
aplpack::faces(datosc)
aplpack::faces(datosc,face.type=0)
aplpack::faces(datosc,face.type=1)
aplpack::faces(datosc,face.type=2)


library(TeachingDemos)
TeachingDemos::faces(datosc)


# Otros gráficos univariados
# Gráfico de estrellas con la función stars() de graphics
stars(datosc,labels=seq(1,21))

################################################
################################################
################################################
################################################

####################################### 
#CLUSTER PARTICION               ######PROPONES EL NUMERO DE CLUSTER.
#######################################
#-----------------------------------------------------------------------------------
# 1.1. Usando la función fviz_nbclust() del paquete factoextra  # pocos datos

###################
#ALGORITMO K-MEANS#
###################


#############################################
#CRITERIO DE SUMA DE CUADRADOS INTRA < > WSS#
#############################################


library(factoextra)
library(tictoc)  # system.time()
tic()
set.seed(123)
fviz_nbclust(datosc, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Método Elbow")
toc()

#Se puede observar en la grafica que con 1 cluster suma de cuadrados intra es maximo 
#luego la suma de cuadrados va decreciendo a manera que que aumenta el numero de cluster
#hasta que el numero cluster es igual numero de individuos y ahi la suma de cuadrados intra
#es igual a cero.

#nota:cuando tengas muchos individuos no se aprecia muy bien en la grafica.(ejemplo:5000 mil inidividuos)

#-----------------------------------------------------------------------------------


#vamos hacer el mismo grafico anterior pero usando "r basico" combinado  con ggplot2

# 1.2. Usando la función kmeans() # se recomienda # muchos datos 
tic()
set.seed(123)
wss <- numeric()
for(h in 1:10){
  b<-kmeans(datosc,h)#h puede ser h=1 1cluster hasta h=10 10 cluster ,numero de cluster=k o hiperparametros(le doy a priori, le doy como investigador el numero de cluster)
  wss[h]<-b$tot.withinss# "tot.withinss" suma de cuadrados intra cluster(dentro del cluster)
 }
wss#suma de cuadrados intra 
#decreciente:

#334.66667 177.55556  89.88889  78.98889  67.50000  55.30556  40.75000  35.58333  30.31667  24.50000



#representarlo en un grafico:  

#requisito : convertirlo en un data.frame
wss1 <- data.frame(cluster=c(1:10),wss)
wss1

#   cluster       wss
#1        1 334.66667
#2        2 177.55556
#3        3  89.88889
#4        4  78.98889
#5        5  67.50000
#6        6  55.30556
#7        7  40.75000
#8        8  35.58333
#9        9  30.31667
#10      10  24.50000

#---------------------------------------------------------------------------------

#1.3 representa la misma grafica que en el punto "1.1", es mas eficiente 
#porque se ejecuta en menor tiempo(ejemplo: 5000 mil individuos,mas rapido)

library(ggplot2)
tic()
ggplot(wss1) + aes(cluster,wss) + geom_line(color="blue") + 
  geom_point(color="blue") +
  geom_vline(xintercept = 3, linetype = 2,col="red") +
  labs(title = "Método Elbow") + 
  scale_x_continuous(breaks=1:10) + 
  theme_classic()
toc()

#la solucion es con 3 cluster  o 3 k-means(codo, inflexion).
#hasta s11.thursday.cluster

#--------------------------------------------------------------------------------------

library(tictoc)
elbow <- function(k) {
  set.seed(123)
  fviz_nbclust(datosc, kmeans, method = "wss",k.max=k) +
    geom_vline(xintercept = 3, linetype = 2) +
    labs(subtitle = "Método Elbow")
}

elbow(8)

#21 individuos, es decir hay 20 estapas(hay 20 suma de cuadrados intra y eso es porque el ultimo su
#suma cuadrados es igual a 0) 
library(tictoc)

#logica # importante 
tic()
  set.seed(123)
  fviz_nbclust(datosc, kmeans, method = "wss",k.max=8) +
    geom_vline(xintercept = 3, linetype = 2) +
    labs(subtitle = "Método Elbow")
toc()

#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------


################# 
#CLUSTER K-MEAS #
#################

#######################################
#CRITERIO DE SUMA CUADRADOS INTRA"WSS"#
#######################################


#ALGORITMO K-MEAS 
#define los clusters,que tenga la VARIACION TOTAL INTRA minima

#NUMERO DE CLUSTER
#¿cuantos clusters hay ?
#criterio de suma cuadrados intra "wss"


#primero

elbow <- function(k) {
  set.seed(123)
  fviz_nbclust(datosc, kmeans, method = "wss",k.max=k) +
    geom_vline(xintercept = 3, linetype = 2) +
    labs(subtitle = "Método Elbow")
}

#segundo
elbow(15)

#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#------------------------------------
##-----------------------------------VEREMOS:_NO_CORRE_INICIO
##-----------------------------------NO SALE EL ENGRANAJE
#poniendo en una funcion:

library(factoextra)#fviz_nbclust
k1=10
t1=1
elbow <- function(k1,t1) {
  set.seed(123)
  fviz_nbclust(datosc, kmeans, method = "wss",k.max=k1) +
    geom_vline(xintercept = 3, linetype = t1) +
    labs(subtitle = "Método Elbow")
}

#segundo
elbow(5,4)
library(manipulate)
manipulate(elbow(a,b),a=slider(2,20),b=slider(1,5))


#------------
#------------
#------------
#------------
#21:36
#¿QUE HACE MANIPULATE?
#pone automaticamente un engranaje para que  nosotros pongamos el valor de "k",
#donde k es hasta que suma de cuadrados intra la fx no permite 1 y no permite 21 
#porque es el maximo,permite entre 1 y 21 es decir 2 al 20

elbow2 <- function(k1) {
  set.seed(123)
  fviz_nbclust(datosc, kmeans, method = "wss",k.max=k1) +
    geom_vline(xintercept = 3, linetype = 2) +
    labs(subtitle = "Método Elbow")
}
elbow2(20)
library(manipulate)
manipulate(elbow2(k),k=slider(2,20))



#------------------------------------
##-----------------------------------VEREMOS:_NO_CORRE_FIN
##-----------------------------------NO SALE EL ENGRANAJE

#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
################### 
#CLUSTER PARTICION#
###################

#####################################
#ALGORITMO K-MEAS Y CRITERIO SILUETA#
#####################################



# 2.1. Usando el paquete factoextra


#grafica actual
library(factoextra)
#method = c("silhouette", "wss", "gap_stat")  < > "silueta";"suma de cuadrados intra";"gap_stat"
tic()
set.seed(123)
fviz_nbclust(datosc, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
toc()




# 2.2. Usando la función k-means

#grafica antigua
windows()
library(cluster)

tic()
set.seed(123)
diss.datos <- daisy(datosc)
par(mfrow=c(1,3))
for(h in 2:4){
  clu=kmeans(datosc,h)
  plot(silhouette(clu$cluster,diss.datos))
}
toc()

par(mfrow=c(1,1))


#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------

################### 
#CLUSTER PARTICION#
###################

################################################################
#CLUSTER K-MEANS Y CRITERIO DE SUMA DE CUADRADOS INTRA(ELBOW)#CODO
################################################################

#estructura del k-means:

#kmeans(x, centers, iter.max = 10, nstart = 1,
#       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
#                     "MacQueen"), trace=FALSE)


set.seed(123)
km <- kmeans(datosc, 
             centers=3,      # Número de Cluster
             iter.max = 100, # Número de iteraciones máxima
             nstart = 25,  # Número de puntos iniciales(25 SOLUCIONES)
             algorithm="Lloyd")#algoritmo por defecto es "Lloyd"
#k-means me dara de las 25 soluciones ,en este caso  3  centroides
#inciales al azar(pueden estar cerca o lejos) basandose del criterio
#de la suma de cuadrado intra (minima,la mejor posible,se esfuerza)



# ntart=25, significa que se probarán 25 puntos iniciales 
# aleatorios y luego elegirá aquel donde la variación 
# dentro (intra) de cluster sea mínima. 
# El valor por defecto es 1

#numero de iteracciones -algoritmo "Lloyd"
km$iter
#9

#numero de iteracciones -algoritmo "Hartigan-Wong"
km$iter
#2

#numero de iteracciones -algoritmo "MacQueen"
km$iter
#5

##numero de iteracciones -algoritmo por defecto es "Hartigan-Wong"
km$iter
#2


# Mostrar resumen de los clusters
km


##########################################
# Suma de cuadrados intra de cada cluster#
##########################################
km$withinss      
#31.83333 20.50000 37.55556



################################
# suma de cuadrados intra total#
################################
#(Suma de cuadrados intra del cluster1+Suma de cuadrados intra del cluster2 +Suma de cuadrados intra del cluster3 )
km$tot.withinss 
#89.88889                        #menor posible

#########################
#suma de cuadrados total#
#########################
# Suma de cuadrados total suma(cuadrado(x - media)) 

#(dato1-media)^2 + (dato2-media)^2+ (dato3-media)^2 +...+ (daton-media)^2
km$totss                              
#334.6667                         


#################################
#suma de cuadrados entre cluster#
#################################
km$betweenss                     #mayor posible
#244.7778
# Se obtiene por diferencia #(#suma de cuadrados total# y # suma de cuadrados intra total#)
km$totss-km$tot.withinss
#244.7778



# Tamaño de cada cluster
km$size

# Promedios de cada cluster 
km$centers

# Número de iteraciones
km$iter
#9  iteraccion es la mejor de mis 100 iteracciones(hay 25 formas de empezar con 3 centroides iniciales)



#forma1:
# Junta el archivo de datos con la columna de cluster
library(dplyr)#tidyverse
datosc %>% mutate(grp=km$cluster)->datos.k  ;datos.k       #LOS CLUSTER SERVIRAN DE INSUMO A UN MODELO DE REGRESION(TECNICA SUPERVISADA)


#forma2:
datos.k <- cbind(datosc,grp=factor(km$cluster))  ;datos.k  #LOS CLUSTER SERVIRAN DE INSUMO A UN MODELO DE REGRESION(TECNICA SUPERVISADA)

head(datos.k)
str(datos.k)
datos.k$grp <- factor(datos.k$grp)#convirtiendo los cluster en factor   #LOS CLUSTER SERVIRAN DE INSUMO A UN MODELO DE REGRESION(TECNICA SUPERVISADA)

#GRAFICA
library(factoextra)
fviz_cluster(km, data = datosc, ellipse.type = "convex") +
  theme_classic()
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

###################### 
#CLUSTER K-MEANS ++  #
###################### 
library(LICORS)
set.seed(123)
kmpp <- kmeanspp(datosc, 
                 k=3, 
                 start="random",
                 nstart = 25,
                 iter.max=100)
                 

# ntart=25, significa que R probará 25 puntos iniciales 
# aleatorios y luego elegirá aquel donde la variación 
# dentro de cluster sea mínima. El valor por defecto es 1
#----------------------------------------------------------
# Mostrar resumen de los clusters
kmpp


#numero de iteracciones -algoritmo "Lloyd"
kmpp$iter
#2

#numero de iteracciones -algoritmo "Hartigan-Wong"
kmpp$iter
#2

#numero de iteracciones -algoritmo "MacQueen"
kmpp$iter
#3

##numero de iteracciones -algoritmo por defecto es "Hartigan-Wong"
kmpp$iter
#2




#----------------------------------------------------------
##########################################
# Suma de cuadrados intra de cada cluster#
##########################################
kmpp$withinss    
#[1] 31.83333 37.55556 20.50000

#----------------------------------------------------------

################################
# suma de cuadrados intra total#
################################
#(Suma de cuadrados intra del cluster1+Suma de cuadrados intra del cluster2 +Suma de cuadrados intra del cluster3 )

kmpp$tot.withinss                    #menor posible
#89.88889

#----------------------------------------------------------

#########################
#suma de cuadrados total#
#########################
# Suma de cuadrados total suma(cuadrado(x - media)) 

#(dato1-media)^2 + (dato2-media)^2+ (dato3-media)^2 +...+ (daton-media)^2

kmpp$totss          
#334.6667
#----------------------------------------------------------


#################################
#suma de cuadrados entre cluster#
#################################
kmpp$betweenss                       #mayor posible

#244.7778

# Se obtiene por diferencia (#suma de cuadrados total# y # suma de cuadrados intra total#)
#334.6667-89.88889

#----------------------------------------------------------

# Tamaño de cada cluster
kmpp$size
#cluster1 cluster2 cluster3
#6          9         6


# Promedios de cada cluster 
kmpp$centers
#   divertid  presupu aprovech buenacom noimport   ahorro
#1 3.500000 5.500000 3.333333 6.000000    3.5     6.000000
#2 5.666667 3.666667 6.000000 3.222222    2.0     4.000000
#3 1.666667 3.000000 1.833333 3.500000    5.5     3.333333



# Otra forma de obtener promedios de cada cluster
aggregate(datosc, by=list(cluster=kmpp$cluster), mean)
#    cluster divertid  presupu aprovech buenacom noimport   ahorro
#1       1  3.500000 5.500000 3.333333 6.000000     3.5    6.000000
#2       2  5.666667 3.666667 6.000000 3.222222     2.0    4.000000
#3       3  1.666667 3.000000 1.833333 3.500000     5.5    3.333333

#asignacion de los individuos a cada cluster
kmpp$cluster

library(factoextra)
fviz_cluster(list(data = datosc, cluster = kmpp$cluster),
             palette = c("#2E9FDF",  "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = T, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_bw())



#forma1:
# Junta el archivo de datos con la columna de cluster
datos.kpp <- cbind(datosc,grp=as.factor(kmpp$cluster))

head(datos.kpp)
str(datos.kpp)

#forma2:
# Junta el archivo de datos con la columna de cluster
library(dplyr)#tidyverse
datosc %>% mutate(grp=km$cluster)->datos.k

head(datos.k)
str(datos.k)#grp o cluster esta formato numero lo tenemos q cambiar a formato factor
datos.k$grp=factor(datos.k$grp)#convirtiendo en factor
str(datos.k)#grp o cluster esta formato factor



#------------------------------------------------------------------------------

#libro para suscribirme: 11:55 pm salen las publicaciones mas recientes.

#https://www.analyticsvidhya.com/blog/2019/08/comprehensive-guide-k-means-clustering/
  
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


############################
#CLUSTER K-MEANS JERARQUICO#
############################

#lo mejor de k-means con lo mejor del jerarquico.

library(factoextra)

res.hk <- hkmeans(datosc, 3,#quiero 3 cluster esto sale del cluster jerarquico.
                  hc.metric = "euclidean", #jerarquico
                  hc.method = "ward.D2",   #jerarquico
                  iter.max = 10)#no necesita muchas iteracciones ya que combinamos al jerarquico y kmeas

#numero de iteraciones
res.hk$iter
#1

# Elementos que retorna hkmeans()
names(res.hk)

# Mostrar los resultados
res.hk

#asignacion de los individuos a cada cluster
res.hk$cluster

#juntando datosc y los clusters
head(cbind(datosc,cluster=res.hk$cluster))

#promedios 
res.hk$centers


#tammaño de cad cluster
res.hk$size

#juntando datosc y los clusters
datos.hk <- cbind(datosc,grp=as.factor(res.hk$cluster))

#########
#grafica#
#########

#insumo    "res.hk$cluster   y datosc"  #asignacion de los individuos a cada cluster


library(factoextra)
fviz_cluster(list(data = datosc, cluster = res.hk$cluster),
             palette = c("#2E9FDF",  "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = T, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_bw())