####################
#ANALISIS FACTORIAL#
####################
library(readr)
datos<- read_csv("D:/UNALM-2020-2/MULTIVARIADAS1/trabajos/entrega2_luis/BD-Autos.csv")
#
library(pacman)
p_load(rela, psych, ade4,GGally,PerformanceAnalytics)
# Lectura de datos con 7 variables
str(datos)

# No considerar la primera columna Id
str(datos)############################################
# 2. Análisis Exploratorio   #                                       #
############################################
datos$id=NULL
# Análisis Descriptivo y Análisis de Correlación
library(psych)
describe(datos)
cor(datos)

corr.test(datos)
windows()
#MAPA DE CALOR##SUGERENCIA usar- 'windows()'
library(GGally)
ggcorr(datos)

#####################################
# Prueba de Esfericidad de Bartlett##
#####################################
library(rela)
options(scipen=0)  # scipen = 999 Eliminan la notación científica
cortest.bartlett(cor(datos),n=nrow(datos))
#pvalor es menor a 0.05,se rechaza la hipotesis nula H0.
# La matriz de correlación difirente a la matriz Identidad.

# Indicador Kaiser-Meyer-Olkinn KMO y MSA 
KMO(datos)
##como 0.72 es  superior a 0.5 podemos trabajar con todas las varibles 

#######################################################
#ANALISIS FACTORIAL SIN ROTACION CON FUNCION PRINCIPAL#
#######################################################
library(psych)
facto.sin.rota <- principal(r=datos,#r=matriz de correlacion
                            nfactors=10,#¿CUANTOS FACTORES?
                            covar = F,#¿QUIERES TRABAJAR CON LA MATRIZ VARIANCIA Y COVARIANCIA?RPTA:NO,yo quiero trbjar con los datos estandarizados
                            rotate="none")#no  quiero rotar
str(facto.sin.rota)

# Autovalores
facto.sin.rota$values
#####################################################
#ES NECESARIO LLEVARLO A DATAFRAME A LOS AUTOVALORES#
#####################################################
eig.val=as.data.frame(facto.sin.rota$values)#para llevarlo a un screp plot 
#es necesario convertir los autovalores en un dataframe.
#
######################## 
###criterio screpp plot#insumo autovalores en dataframe
########################
library(ggplot2)
ggplot(eig.val) + aes(x=seq(1:10),y=facto.sin.rota$values) +
  geom_point(color="red") +
  scale_x_continuous(breaks=seq(1:10)) +
  labs(title="Scree-Plot",
       x= "Componentes",
       y= "Autovalor") +
  geom_col(fill="white",
           col="black") +
  geom_line() + 
  geom_hline(yintercept = 1,lty=2,color="blue") +
  geom_text(aes(label=round(facto.sin.rota$values,2) ), vjust=1.5,color="black") +
  theme_bw()
#####es sufienciente quedamos con 2 factores,por el criterio de la media 
######################################
###corriendo los datos con 2 factores#
######################################
library(psych)
facto.sin.rota1 <- principal(r=datos,#r=matriz de correlacion
                            nfactors=2,#¿CUANTOS FACTORES?
                            covar = F,#¿QUIERES TRABAJAR CON LA MATRIZ VARIANCIA Y COVARIANCIA?RPTA:NO,yo quiero trbjar con los datos estandarizados
                            #lo estandariza la funcion 'principal'
                            rotate="none")
####
facto.sin.rota1$loadings               
#en V5,estan demasiados cercanos y ahora usaremos rotacion
# Gráfica de circulo de correlaciones
library(ade4)
load.sin.rota <- facto.sin.rota1$loadings[,1:2]
s.corcircle(load.sin.rota,grid=FALSE)
#################################
#rotando con el metodo 'varimax'#
#################################
library(psych)
facto.con.rota <- principal(r=datos,
                            nfactors=2,
                            rotate="varimax")#matriz de correlacion
###circuferencia
library(ade4)
load.con.rota <- facto.con.rota$loadings[,1:2]
s.corcircle(load.con.rota,grid=FALSE)


facto.con.rota$loadings
##########################################
#¿que varaibles pertenecen a cada factor?#
##########################################
library(psych)
fa.diagram(facto.con.rota)
#3.   Trabaje con la matriz de correlación y el método 
#de componentes principales. Indique cuál es el número d
#de factores comunes del modelo factorial. Mencione dos criterios.
#criterio de la media , y dos factores 
#
#En caso sea necesario use uno o varios métodos de rotación ortogonal.
#rpta_
#sin rotacion no podiamos determinar al 100% que la  variable 5 
#"perteneciera" al factor 2,con rotacion ddeterminamos que si peretence 
#al factor 2
#5. Indique cuáles son los grupos de variables que ha encontrado como resultado del análisis factorial. 
#Presente las conclusiones finales del estudio.
#en v5 pertence al factor 2
#conclusion:
#factor 1
#v1
#v2
#v3
#v4
#v8
#v10
#factor 2
#v5
#v6
#v7
