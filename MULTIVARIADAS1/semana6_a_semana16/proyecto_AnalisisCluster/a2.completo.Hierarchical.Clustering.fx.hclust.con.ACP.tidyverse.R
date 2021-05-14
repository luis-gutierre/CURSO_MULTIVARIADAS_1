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
#********************************************************************
#********************************************************************
##*******************************************************************
#********************************************************************
#--------------------------------------------------------------------



#################################
# 7. Comparando dos dendogramas #
#################################

#cluster jerarquicos:

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

# 1.1. Usando la función fviz_nbclust() del paquete factoextra  # pocos datos
library(factoextra)
library(tictoc)  # system.time()
tic()
set.seed(123)
fviz_nbclust(datosc, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Método Elbow")
toc()

# 1.2. Usando la función kmeans() # se recomienda # muchos datos 
tic()
set.seed(123)
wss <- numeric()
for(h in 1:10){
  b<-kmeans(datosc,h)
  wss[h]<-b$tot.withinss
}
wss
wss1 <- data.frame(cluster=c(1:10),wss)
wss1

library(ggplot2)
ggplot(wss1) + aes(cluster,wss) + geom_line(color="blue") + 
  geom_point(color="blue") +
  geom_vline(xintercept = 3, linetype = 2,col="red") +
  labs(title = "Método Elbow") + 
  scale_x_continuous(breaks=1:10) + 
  theme_classic()
toc()
