
# Carga de paquetes -------------------------------------------------------

library(readxl) # lectura de datos en excel
library(skimr)  # resumen de datos
library(dplyr)  # pipe ( %>% ) y manipulación de datos
library(clustertend)
library(psych)
library(factoextra) # para la función get_clust_tendency, fviz_dend

# Análisis exploratorio ---------------------------------------------------

datos = read_xlsx('Rusia2018.xlsx')

skim(datos)

datos1 = datos[,-1]
datos1 = datos %>% select(-PAIS) # esta línea es equivalente a la anterior
datos1

datos1 %>% scale()
datos1 %>% scale() %>% skim
datos2 = datos1 %>% scale() %>% data.frame()
rownames(datos2)= as.vector(as.matrix(datos[,1]))
datos2

# Viabilidad --------------------------------------------------------------

get_clust_tendency(datos2, n = nrow(datos2)-1)

# Distancias --------------------------------------------------------------

datos2 %>% dist() %>% round(2) 
datos2 %>% dist() -> Dist.Euc

datos2 %>% dist(method="manhattan") %>% round(2) 
datos2 %>% dist(method="manhattan") -> Dist.Man

datos2 %>% dist(method="minkowski",p=4) %>% round(2) 
datos2 %>% dist(method="minkowski",p=4) -> Dist.Min

# Agrupamientos -----------------------------------------------------------

Dist.Euc %>% hclust(method="centroid") %>% plot()
x11();Dist.Euc %>% hclust(method="single") %>% plot()
x11();Dist.Euc %>% hclust(method="complete") %>% plot()
x11();Dist.Euc %>% hclust(method="average") %>% plot()
Dist.Euc %>% hclust(method="ward.D") %>% plot()
Dist.Euc %>% hclust(method="ward.D") -> clustering ###### importante

clustering$merge
clustering$height
clustering$height %>% diff()
clustering$height %>% diff() %>% plot(type="b", pch=18)
clustering$order
clustering$labels
clustering$method
clustering$call
clustering$dist.method

# Dendrograma -------------------------------------------------------------

clustering %>% plot()
clustering %>% fviz_dend()
clustering %>% fviz_dend(horiz=TRUE)
clustering %>% fviz_dend(k=2)
clustering %>% fviz_dend(k=2, k_colors = c("red","darkblue"))
clustering %>% fviz_dend(k=4, k_colors = c("red","darkblue","gold","forestgreen"))
clustering %>% fviz_dend(k=4, 
                         k_colors = c("red","darkblue","gold","forestgreen"),
                         cex  = 1,
                         ylab = "Altura", 
                         xlab = "Selección") +
  geom_hline(yintercept = 7, linetype = "dotted")

# Interpretación ----------------------------------------------------------

cluster = cutree(clustering, 4)
cluster

datos1 = data.frame(datos1,cluster)
datos1

datos1 %>% 
  group_by(cluster) %>% 
  summarise_all(mean)

datos1 %>% 
  group_by(cluster) %>% 
  summarise_all(median)

datos1 %>% 
  mutate(cluster=as.factor(cluster)) %>% 
  ggplot(aes(x=G,y=GF,color=cluster)) + 
  geom_point(size = 4) +
  labs(x="Partidos ganados",
       y="Goles a favor")+
  theme_minimal()


# Análisis no jerárquico --------------------------------------------------
# kmeans es una técnica no jerárquica

datos2 %>%  # datos2 es el conjunto de datos estandarizado
  fviz_nbclust(FUNcluster = kmeans,  # paquete factoextra
               method     = "silhouette", 
               k.max      = 9) +
  labs(title = "Número óptimo de clusters",
       x = "Número de clusters",
       y = "Ancho medio de la silueta") 

datos2 %>% 
  kmeans(centers = 4) -> resu_clust

resu_clust$cluster
resu_clust$centers
resu_clust$totss
resu_clust$withinss # dentro de clusters
resu_clust$tot.withinss
resu_clust$betweenss # entre clusters
resu_clust$size
resu_clust$iter
resu_clust$ifault 

datos1 %>% 
  mutate(cluster=as.factor(resu_clust$cluster)) %>% 
  group_by(cluster) %>% 
  summarise_all(mean)

datos1 %>% 
  mutate(cluster=as.factor(resu_clust$cluster)) %>% 
  group_by(cluster) %>% 
  summarise_all(median)

datos1 %>% 
  mutate(cluster=as.factor(resu_clust$cluster)) %>% 
  ggplot(aes(x=P,y=GC,color=cluster)) + 
  geom_point(size = 4) +
  labs(x="Partidos ganados",
       y="Goles a favor")+
  theme_minimal()

datos1 %>% 
  mutate(cluster=as.factor(resu_clust$cluster)) %>% 
  ggplot(aes(x=cluster,y=DIF)) +
  geom_boxplot(fill = "dodgerblue1")+
  labs(x="Cluster",
       y="Diferencia de goles")+
  theme_minimal()

