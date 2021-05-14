###########################################################################################################
##########################TRABAJO_DE_ANALISIS_COMPONENTES_PRINCIPALES##################################################
###########################################################################################################
###########################################################################################################
#PROFESOR:JESUS SALINAS FLORES 
###INTEGRANTES:
#Cruzate Contreras Christian Paul
#Gutierrez Saldaña Luis Edgar 
#Montoya Toribio Jenny Antonella
#DESCRIPCION:
#LINK: https://archive.ics.uci.edu/ml/machine-learning-databases/00477/
#El conjunto de datos históricos del mercado de valoración de bienes raíces se recopila 
#de Sindian Dist., New Taipei City, Taiwán.
#Información de atributos:
#Las entradas son las siguientes
#X1 = fecha de la transacción (por ejemplo, 2013.250 = 2013 marzo, 2013.500 = 2013 junio, etc.)
#X2 = edad de la vivienda (unidad: año)
#X3 = distancia a la estación MRT más cercana (unidad: metro )
#X4 = número de tiendas de conveniencia en el círculo habitable a pie (entero)
#X5 = coordenada geográfica, latitud. (unidad: grado)
#X6 = coordenada geográfica, longitud. (unidad: grado)
#La salida es la siguiente:
#Y = precio de la vivienda de la unidad de área (10000 Nuevo dólar taiwanés / Ping, donde Ping es una unidad local, 1 Ping = 3,3 metros cuadrados)
#--------------------------------------------------------------
# Para limpiar el workspace, por si hubiera algún dataset 
# o información cargada
rm(list = ls())
dev.off()
#--------------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#--------------------------------------------------------------
# Otras opciones
options(scipen = 999)    #Eliminar la notación científica
options(digits = 3)      #Número de decimales
#--------------------------------------------------------------
#library(pacman)
#p_load("corrr","GGally","ade4","car","carData","readxl","FactoClass","corrplot","ggplot2","funModeling")
#--------------------------------------------------------------
#LECTURA DE DATOS
library(readxl)
vivienda <- read_excel('D:/UNALM-2020-2/MULTIVARIADAS1/trabajos/entrega1_luis/vivienda1.xlsx')
vivienda$No=NULL
vivienda$X1=NULL
vivienda$Y=NULL
View(vivienda)
str(vivienda)
###data=machin[,c(3:9)]##jalar columnas de mi data
#######################
#ANALISIS EXPLORATORIO#
#######################
library(funModeling) 
df_status(vivienda)
#Se puede obervar que no hay valores nulos.
#VERIFICANDO LAS CORRELACIONES ENTRE LAS VARIABLES
library(GGally)
ggcorr(vivienda)
#Se puede observar que las variables estan altamente correlacionadas.
################################################
#####ESTADARIZAMOS LOS DATOS#####
################################################
#ESTANDARIZAMOS LOS DATOS PORQUE SE ENCUENTRAN EN DIFERENTES RANGOS
vivienda.s <- scale(vivienda)##datos estandarizados
###################################
#####ANALISIS DE LAS VARIABLES#####
###################################
library(ade4)
vivienda.s <- scale(vivienda)##datos estandarizados
acp <- dudi.pca(vivienda.s,            
                scannf=FALSE, 
                scale=T, center = F,   # Matriz covariancia-variancia
                nf=ncol(vivienda.s))
#autovalores
acp$eig
#autovectores
acp$c1
#############################  
####CRITERIO DE LA MEDIA#####
############################# 
library(ggplot2)
ggplot(as.data.frame(acp$eig)) + 
  aes(x=seq(1:5),acp$eig) + 
  geom_point(color="Brown") + geom_line() + theme_bw() +
  scale_x_continuous(breaks=seq(1:5)) +
  labs(title="Scree-Plot",
       x= "Componentes",
       y= "Autovalor") +
  geom_hline(yintercept = 1,lty=2,color="green")
#
inertia(acp)
#
# Decomposition of total inertia:
#    inertia     cum  cum(%)
#Ax1  2.6736   2.674   53.47
#Ax2  1.0207   3.694   73.89
#Ax3  0.6046   4.299   85.98
#Ax4  0.5497   4.849   96.97
#Ax5  0.1514   5.000  100.00
#CONLUSION:
#Basandonos en el criterio de la media, apoyado
#Criterio del grafico (Scree Plot)
#podemos visualizar que con los 2 primeros componentes
#explicamos 73.89%  de variabilidad total,con ello
#podemos explicar las variables.
#
###############################################
#ANALISIS CON 2 COMPONENTES PRINCIPALES#
###############################################
library(ade4)
#vivienda.s <- scale(vivienda)##datos estandarizados
acp1 <- dudi.pca(vivienda.s,            
                 scannf=FALSE, 
                 scale=T, center = F,   # Matriz covariancia-variancia
                 nf=2)
#autovalores
acp1$eig
#autovectores
acp1$c1
##
#############################################################
#CONTRIBUCION DE LOS COMPONENTES PRINCIPALES A LAS VARIABLES#
#######(PARA GRUPOS)#########################################
#############################################################
contrib=acp1$co*acp1$co
contrib
contrib=as.matrix(contrib)#necesitamos llevarlo a una matriZ
#Por ejm:
#comp1 explica (0.0000616) y la comp2 explica (0.97039) de la variable x2.
#
#############################################################
library(corrplot)
corrplot(contrib,is.corr = FALSE,addCoef.col = T)
#CONCLUSION:
#
#componente1:
#
#explica las variables:
#X3 = distancia a la estación MRT más cercana (unidad: metro )
#X4 = número de tiendas de conveniencia en el círculo habitable a pie (entero)
#X5 = coordenada geográfica, latitud. (unidad: grado)
#X6 = coordenada geográfica, longitud. (unidad: grado)
#
#componente2:
#
#explica la variable:
#X2 = edad de la vivienda (unidad: año)
#
########################## 
##GRAFICA DE DIMENSIONES## 
############(QUE MANERA)##
library(FactoClass)
plotcc(acp1$co)
#######
#### Scores o Puntuaciones de cada individuo
acp1$li
#####
#####
cov(acp1$li)
#acp1$eig
#####
cor(acp1$li)
#OBSERVACION:No existe correlacion entre los componentes 
#principales.
#########################################################
##ANALISIS DE REGRESION CON LOS COMPONENTES PRINCIPALES##
#########################################################
cp1=acp1$li[,1]
cp2=acp1$li[,2]
cbind(cp1,cp2)
###########################################################
library(readxl)
vivienda2 <- read_excel('vivienda1.xlsx')
Y=vivienda2$Y
nrow(vivienda2)
#####################################################
#Formando un dataframe con la variable respuesta('y')
#y los 2 componentes('cp1' y 'cp2')
#####################################################
#### 
#### 
data1=data.frame(Y,cp1,cp2)
str(data1)#414 instancias o filas o observaciones
########################################
####FORMANDO MI MODELO DE REGRESION#####
########################################
RegCP=lm(Y~cp1+cp2,data=data1)
summary(RegCP)
######### 
##BETAS##
#########
B0=RegCP$coefficients[1]
B1=RegCP$coefficients[2]
B2=RegCP$coefficients[3]
cbind(B0,B1,B2)
Y1=B0+B1*cp1+B2*cp2 
Y1=B0+B1*cp1+B2*cp2
Y2=B0+B1*nuevo_cp11+B2*nuevo_cp22
Y2
Y3=B0+B1*cp11+B2*cp22#fila_1
Y3
head(cbind(Y,Y1),10)
##################################################
#La ecuación de regresión se obtiene a través de:#
##################################################
#
#Y=-5.89*CP1-2.60*CP2##
#
###########################
#-5.89* -2.60
#######MULTICOLINEALIDAD###
###########################
library(car)
library(carData)
#En este modelo las variables predictoras no presentan multicolinealidad ya que los FIVs
#son todos iguales a 1:
car::vif(RegCP)
#cp1 cp2 
#1   1
####################################
#Prediciendo con nuevos valores#####
####################################
#Estandarizamos
medias<-sapply(vivienda,mean)
varianza<-sapply(vivienda,var)
#fila_1
32.0
84.87882
10
24.98298
121.5402
vc<-c(32.0,84.87882, 10 ,24.98298, 121.5402) #-- valores correspondientes a cada variable. 
vc.est<-(vc-medias)/sqrt(varianza)#estandarizando
vc.est
###autovectores
acp1$c1
#cp11=-0.0048*z2+0.5703*z3-0.4615*z4-0.4497*z5-0.5094*z6
#cp22=0.9750*z2+0.0655*z3+0.1020*z4+0.1245*z5-0.1382*z6
########################################
#DATA _FILA_1_estandarizada-vivienda.s##
########################################
1.2541
-0.7915
2.005
1.1241
0.4482
#metiendo_FILA_1_estandarizada-vivienda.s
cp11=-0.0048*1.2541+0.5703*-0.7915-0.4615*2.005-0.4497*1.1241-0.5094*0.4482
cp22=0.9750*1.2541+0.0655*-0.7915+0.1020*2.005+0.1245*1.1241-0.1382*0.4482
cbind(cp11,cp22)#obtengo lo mismo
head(acp1$li,1)#obtengo lo mismo
#
#    X2      X3      X4      X5      X6 
# 1.430  -0.463   1.326   2.496 -34.753
#valores_inventados
#Hallando las componentes
# Y1 = 0.235*X1-0.397*X2-0.433*X3-0.386*X4-0.371*X5-0.328*X6-0.451*x7
# Y2 = 0.930*X1-0.0507*X2+0.043*X3-0.004*X4+0.133*X5+0.288*X6+0.1713*X7
acp1$c1
comp<-function(a1){
  W1=-0.0048*a1[1] +0.5703*a1[2]-0.4615*a1[3]-0.4497*a1[4]-0.5094*a1[5]
  W2=0.9750*a1[1] +0.0655*a1[2]+0.1020*a1[3]+0.1245*a1[4]-0.1382*a1[5]
  W<-c(W1,W2)
  return(W)
}
yu<-comp(vc.est)
names(yu)<-c("componente1","componente2")
yu
#Hallando el rendimiento estimado
cp<-function(W1,W2){{
  Y = B0+B1*W1+B2*W2}
  print(Y)
}
r<-cp(yu[1],yu[2])
names(r)<-("Y_estimado")
r#BOTA EL MISMO RESULTADO QUE 'Y2'
#---------------------------------
