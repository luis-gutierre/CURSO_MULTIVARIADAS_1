library(readxl)
data <- read_excel("BD-Billetes.xlsx")
data <- as.data.frame(data)
datos.acp <- data[,-1]

library(ade4)
acp <- dudi.pca(datos.acp,           # datos.s , scale = F
                scannf=FALSE, 
                scale=T, center = T,   # Matriz de Correlación
                nf=2) # ncol(datos.acp)

summary(acp)

print(acp)

# Valores propios (autovalores)
acp$eig

inertia.dudi(acp)

# Vectores propios
acp$c1

#--------------------------------------------------------------
# Gráfica de Valores propios - ScreePlot

library(ggplot2)
ggplot(as.data.frame(acp$eig)) + 
  aes(x=seq(1:6),acp$eig) + 
  geom_point(color="red") + geom_line() + theme_bw() +
  scale_x_continuous(breaks=seq(1:6)) +
  labs(title="Scree-Plot",
       x= "Componentes",
       y= "Autovalor") +
  geom_hline(yintercept = 1,lty=2,color="blue")
#
#
#
# Correlaciones entre las variables y los componentes
acp$co

#--------------------------------------------------------------
# Gráfica de Variables sobre el círculo de correlaciones

library(factoextra)
fviz_pca_var(acp)

# Contribución de los componentes principales a las variables 
(contrib <- acp$co*acp$co)

# Scores o Puntuaciones de cada individuo
acp$li[1:10,]


# Biplots

# Primera forma
library(ade4)
s.label(acp$li,clabel=0.7,grid=FALSE,boxes=FALSE)
s.corcircle(acp$co,grid=FALSE,add=TRUE,clabel=0.7)

# Segunda forma
library(factoextra)
fviz_pca_biplot(acp, repel = F,
                col.var = "steelblue",
                col.ind = "black" )
# Tercera forma
comparacion <- as.data.frame(cbind(Tipo=data[,1],acp$li))
ggplot() + geom_point(aes(x=comparacion$Axis1,
                          y=comparacion$Axis2,
                          color=comparacion$Tipo))  + 
  geom_point(aes(x=acp$co$Comp1,y=acp$co$Comp2)) +
  geom_text(aes(x=acp$co$Comp1,
                y=acp$co$Comp2,
                label= row.names(acp$co)),
            col="blue4",size=4) + 
  theme_bw() + theme(legend.position = "bottom") + 
  labs(x="Componente 1",
       y="Componente 2",
       color="Billete") +
  geom_segment(aes(x=0, y=0, xend=acp$co$Comp1, yend=acp$co$Comp2),      
               size=1,
               arrow=arrow(length = unit(0.2, "cm")))
library(tibble) 
options(pillar.sigfig = 7)
data %>% dplyr::group_by(factor(`note type`)) %>% dplyr::summarize(Length=mean(Length),
                                                                   Left=mean(Left),
                                                                   Right=mean(Right),
                                                                   Bottom=mean(Bottom),
                                                                   Top=mean(Top),
                                                                   Diagonal=mean(Diagonal))

