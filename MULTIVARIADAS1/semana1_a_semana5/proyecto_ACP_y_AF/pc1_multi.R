################################################
############Práctica Calificada 1###############
################################################
#### Profesor: Jesús Salinas.
#### Integrantes: 
#### Luis Edgar Gutierrez Saldaña.
#### Christian Paul Cruzate Contreras.
#### Montoya Toribio Jenny Antonella.
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
library(pacman)
p_load("rela", "psych", "ade4","GGally","PerformanceAnalytics")
library(readxl)
BD_Billetes <- read_excel("BD-Billetes.xlsx")
#variable_rpta
#################
#BILLETES FALSOS#
#################
datos=BD_Billetes[c(1:100),c(2:7)]
datos
datos=as.data.frame(datos)#siempre llevarlo a un DataFrame
################################################# 
# Análisis Descriptivo y Análisis de Correlación#
#################################################
library(psych)
describe(datos)
cor(datos)

corr.test(datos)
windows()
#MAPA DE CALOR##SUGERENCIA usar- 'windows()'
library(GGally)
ggcorr(datos)
#observacion:las variables estan correlacionadas entre 
#si por eso aplicamos analisis factorial
#####################################
# Prueba de Esfericidad de Bartlett##
#####################################
library(rela)
options(scipen=0)  # scipen = 999 Eliminan la notación científica
cortest.bartlett(cor(datos),n=nrow(datos))
#pvalor es menor a 0.05,se rechaza la hipotesis nula H0.
# La matriz de correlación difirente a la matriz Identidad.
###########################################
# Indicador Kaiser-Meyer-Olkinn KMO y MSA #
###########################################
KMO(datos)
##como 0.56 es  superior a 0.5 podemos trabajar con todas las varibles 
#######################################################
#ANALISIS FACTORIAL SIN ROTACION CON FUNCION PRINCIPAL#
#######################################################
library(psych)
facto.sin.rota <- principal(r=datos,#r=matriz de correlacion
                            nfactors=6,#¿CUANTOS FACTORES?
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
ggplot(eig.val) + aes(x=seq(1:6),y=facto.sin.rota$values) +
  geom_point(color="red") +
  scale_x_continuous(breaks=seq(1:6)) +
  labs(title="Scree-Plot",
       x= "Componentes",
       y= "Autovalor") +
  geom_col(fill="white",
           col="black") +
  geom_line() + 
  geom_hline(yintercept = 1,lty=2,color="blue") +
  geom_text(aes(label=round(facto.sin.rota$values,2) ), vjust=1.5,color="black") +
  theme_bw()
#por el criterio de la media ,es sufienciente  explicar la mayor cantidad 
#de varibilidad con 2 factores
######################################
###corriendo los datos con 2 factores#
######################################
library(psych)
facto.sin.rota1 <- principal(r=datos,#r=matriz de correlacion
                             nfactors=2,#¿CUANTOS FACTORES?
                             covar = F,
                             #lo estandariza la funcion 'principal'
                             rotate="none")
#¿rotamos o no rotamos?
facto.sin.rota1$loadings 
#observacion:no es necesario rotar los factores, ya que la diferencia es bastante
#comunalidad
facto.sin.rota1$communality
#observacion,los factores comunes explican la mayor cantidad de variabilidad 
#especificidad
facto.sin.rota$uniquenesses
#observacion:la factores unicos explican poco la variblidad total 

##########################################
#¿que varaibles pertenecen a cada factor?#
##########################################
library(psych)
fa.diagram(facto.sin.rota1)
###########
#factor 1:#
###########
#pertenecen las variables
#left,right,length
######### 
#factor2#
#########
#pertenecen las variables
#top,bottom,diagonal
##############
###############################################################
###############################################################
###############################################################
###############################################################
########
library(readxl)
BD_Billetes <- read_excel("BD-Billetes.xlsx")
#####################
#BILLETES VERDADEROS#
#####################
########
datos2=BD_Billetes[c(101:200),c(2:7)]
datos2=as.data.frame(datos2)
datos2$Diagonal=NULL 
################################################# 
# Análisis Descriptivo y Análisis de Correlación#
#################################################
library(psych)
describe(datos2)
cor(datos2)

corr.test(datos2)
windows()
#MAPA DE CALOR##SUGERENCIA usar- 'windows()'
library(GGally)
ggcorr(datos2)
#observacion:las variables estan correlacionadas entre 
#si por eso aplicamos analisis factorial
#####################################
# Prueba de Esfericidad de Bartlett##
#####################################
library(rela)
options(scipen=0)  # scipen = 999 Eliminan la notación científica
cortest.bartlett(cor(datos2),n=nrow(datos2))
#pvalor es menor a 0.05,se rechaza la hipotesis nula H0.
# La matriz de correlación difirente a la matriz Identidad.
###########################################
# Indicador Kaiser-Meyer-Olkinn KMO y MSA #
###########################################
KMO(datos2)
##como 0.53 es  superior a 0.5 podemos trabajar con todas las varibles,
#BORRANDO LA VARIABLE DIAGONAL
#######################################################
#ANALISIS FACTORIAL SIN ROTACION CON FUNCION PRINCIPAL#
#######################################################
library(psych)
facto.sin.rota2 <- principal(r=datos2,#r=matriz de correlacion
                            nfactors=5,#¿CUANTOS FACTORES?
                            covar = F,#¿QUIERES TRABAJAR CON LA MATRIZ VARIANCIA Y COVARIANCIA?RPTA:NO,yo quiero trbjar con los datos estandarizados
                            rotate="none")#no  quiero rotar
str(facto.sin.rota2)

# Autovalores
facto.sin.rota2$values
#####################################################
#ES NECESARIO LLEVARLO A DATAFRAME A LOS AUTOVALORES#
#####################################################
eig.val=as.data.frame(facto.sin.rota2$values)#para llevarlo a un screp plot 
#es necesario convertir los autovalores en un dataframe.
#
######################## 
###criterio screpp plot#insumo autovalores en dataframe
########################
library(ggplot2)
ggplot(eig.val) + aes(x=seq(1:5),y=facto.sin.rota2$values) +
  geom_point(color="red") +
  scale_x_continuous(breaks=seq(1:5)) +
  labs(title="Scree-Plot",
       x= "Componentes",
       y= "Autovalor") +
  geom_col(fill="white",
           col="black") +
  geom_line() + 
  geom_hline(yintercept = 1,lty=2,color="blue") +
  geom_text(aes(label=round(facto.sin.rota2$values,2) ), vjust=1.5,color="black") +
  theme_bw()
#por el criterio de la media ,es sufienciente  explicar la mayor cantidad 
#de varibilidad con 2 factores
######################################
###corriendo los datos con 2 factores#
######################################
library(psych)
facto.sin.rota3 <- principal(r=datos2,#r=matriz de correlacion
                             nfactors=2,#¿CUANTOS FACTORES?
                             covar = F,
                             #lo estandariza la funcion 'principal'
                             rotate="none")
#¿rotamos o no rotamos?
facto.sin.rota3$loadings 
#rotamos ya que los factores de la variable 'bottom','right' estan demasiado cerca
######### 
#rotamos#
#########
library(psych)
facto.con.rota <- principal(r=datos2,
                            nfactors=2,
                            rotate="varimax")#matriz de correlacion
#comunalidad
facto.con.rota$loadings
#observacion:los factores de cada variable estan distanciados.
facto.con.rota$communality
#observacion,los factores comunes explican la mayor cantidad de variabilidad 
#especificidad
facto.con.rota$uniquenesses
#observacion:la factores unicos explican poco la variblidad total 

##########################################
#¿que varaibles pertenecen a cada factor?#
##########################################
windows()
library(psych)
fa.diagram(facto.con.rota)
############
#verdaderos#
############
#factor1:
#left,right,length
#fator 2:
#bottom,top
#
#
#################
#billetes_falsos#
#################
###########
#factor 1:#
###########
#pertenecen las variables
#left,right,length
######### 
#factor2#
#########
#pertenecen las variables
#top,bottom,diagonal
#conclusion:en los billetes verdaderos no nos fijamos en la diagonal 
facto.con.rota$loadings#billetes_verdaderos
facto.sin.rota1$loadings#billetes_falsos
