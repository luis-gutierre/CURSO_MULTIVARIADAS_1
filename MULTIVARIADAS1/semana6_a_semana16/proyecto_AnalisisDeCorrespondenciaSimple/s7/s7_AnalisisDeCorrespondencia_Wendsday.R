#################################
#TODO SE TRABAJO CON PERFIL FILA#OJO#
#################################
#
#
#https://bookdown.org/jsalinas/tecnicas_multivariadas/
#5#
#contribucion_absoluta:variable->factor   #explica
#contribucion_realtivo:factor->variable
#
rm(list=ls()) #limpiar el environment
graphics.off()#limpiar las graficas
#
#otras opciones
options(scipen = 999) #eliminar notacion cientifica
options(digits = 3)   #numero de decimales
#---------------------------------------------------------
library(pacman)
p_load(MASS, ca, anacor,FactoMineR,vegan,
       gplots,vcd,graphics,factoextra,foreign)
#####################
#1. Ingreso de Datos#
#####################
#Antes de analizar el analisis de correspondencia se
#ingresan los datos, se calculan las frecuencias y se 
#realizan los graficos de perfiles.
#
#                  opinion
#renta       bueno malo  regular
#bajo         75    40     35
#medio        60    50     70 
#alto         20    40     30
#muy alto     15    40     25
#
#encuestado   renta   opinion
#         1    medio   malo
#         .    
#         .
#         n   alto     bueno     
#filas
datos.acs <- matrix(c(75,40,35,
                      60,50,70,
                      20,40,30,
                      15,40,25),nrow=4,byrow=T) #ncol = 3
#¿la informacion es por fila?
#si porque'byrow=T', si hubiera puesto 'byrow=F' la informacion
#seria por columna.
#'nrow' hace referencia al numero de filas que deseamos obtener ,
#la columna lo pone por defecto, en este ejemplo el numero de columna
#por defecto es 3.
datos.acs
# Asignación de nombres a las filas y columnas de la tabla
dimnames(datos.acs)<-list(renta=c("Bajo", "Medio", "Alto", "Muy Alto")
                          ,opinion=c("Bueno","Malo","Regular"))

str(datos.acs) 
datos.acs
datos.acs[2,1]

addmargins(datos.acs)
#-----------------------------------------------------1:11:38
#                   #PRIMER PASO#             
######################################################
#representando la tabla de contigencia en una grafica#trabajando-perfil fila
######################################################
#--------------------------------------------------------
#Primera forma- Balloonplots
#--------------------------------------------------------
library(gplots)
####################################################### 
#1# #Convertir los datos en una tabla,antes era una lista.#
####################################################### 
dt <- as.table(datos.acs)

dt
str(dt)
############################################ 
#2# Para graficarlo con % fila (perfiles fila)#
############################################ 
dt <- prop.table(dt,margin=1)#perfil fila
dt
#margin=1 perfil fila
#margin=2 perfil columna
#si solo pongo -> 'prop.table(dt)' como resultado botara -> frecuencia relativa
#
#sum(dt[1,c(1,3)])
#sum(dt[3,])
#
balloonplot(t(dt), #siempre se pone la transpuesta sea fila o columna.
            main ="Gráfico Opinión Renta", 
            xlab ="Opinión", 
            ylab="Renta",
            label = F, cum.margins=F, 
            label.lines=F, show.margins = FALSE)#show.margins = TRUE ,'cirulo + porcentaje'
#cuanto mas cercano este a 1 el ciculo sera mas grande.
#--------------------------------------------------------------
# Segunda forma - Mosaicos
#--------------------------------------------------------------
library(graphics)
mosaicplot(t(dt), shade = F, 
           main ="Gráfico Opinión Renta", 
           xlab ="Opinión", 
           ylab="Renta",
           las=2)
##                   #SEGUNDO PASO#  
#HACER UNO DE LA PRUEBA CHI-CUDRADO,PARA VER SI PUEDO REALIZAR
#ANALISIS DE CORREPONDENCIA.
#H0:LOS NIVELES DE LA VARIABLES SON INDEPENDIENTES
#H0:LOS NIVELES DE LA VARIABLES SON DEPENDIENTES
#
#H0:La opinion sobre el sistema sanitario es independiente del nivel de ingresos de 
#los contribuyentes.
#H1:...dependiente
prueba <- chisq.test(datos.acs)
prueba
#p-value = 0.0000004
#Rechazando la H0,es decir los niveles de la variables son dependientes.
#----------------------------------
prueba$observed#valores observados#frecuencia absoluta
prueba$expected#valores esperados,se puede hacer# sum(prueba$expected[,3])
#porque es una matrix 'class(prueba$expected)' 
#-------------------------------------
#¿como salio el valor esperado de 'bajo y bueno'=51?
#usamos -> prueba$expected
#si asumimos que es independiente
#P(renta baja Y opinion bueno)=P(renta baja)*P(opinion bueno)
#(150/500)*(170/500)
#pero,como son 500 encuestados:
#500*(150/500)*(170/500)=51

#-----------------------------------
# Frecuencia Relativa (fij)
prop.table(datos.acs) 
#
# Perfiles Fila
prop.table(datos.acs, 1) 

# Perfiles Columna
prop.table(datos.acs, 2) 
#-----------------------------------
# Tabla con el paquete gmodels y función CrossTable()
library(gmodels)
CrossTable(datos.acs,
           prop.t=F,  # Frecuencia Relativa
           prop.r=F,  # Perfil Fila
           prop.c=F,  # Perfil Columna
           prop.chisq=T)
#-----------------------------------
#chi-cuadrado   TOTAL
prueba
#
##################################### 
# Analisis de Correspondencia Simple#
#  con el paquete FactoMiner        #
#####################################
#
library(FactoMineR) 
#El 100% de la inercia total,se puede descomponer en 
#min(numero de filas,numero de columnas)-1
#componentes o dimensiones.
#
#########################################
res.ca <- CA(datos.acs,ncp=2,graph=TRUE)#ncp=2,es decir 2 componenetes
#########################################
#
#datos.acs:MATRIX

res.ca

#--------------------------------------------------------------
# Scree Plot de los Autovalores
res.ca$eig#autovalor#FactorMineR#MEquedoConEsto

eig.val <- get_eigenvalue(res.ca)#MeQuedoConEstoPorLasCoordenadas#Insumo.
eig.val#autovalor#factoextra

fviz_screeplot(res.ca)#grafica de screp plot con los autovalores.

# fviz_screeplot(fit) + geom_hline(yintercept=33.33, linetype=2, color="red")

fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 100))
#grafica de screp plot con los autovalores y su respectivo porcentaje.
#--------------------------------------------------------------
# Interpretación de los Indicadores del ACS

summary(res.ca,nb.dec = 3, ncp = 2)
# El 100% de la Inercia Total, se puede descomponer en
# min(número de filas,número de columnas) -1 componentes o
# dimensiones
#------------------------------------------------------------------------
#
######## 
#BIBLOT# con 2 componentes.
########
#-----------------------------------------------------------------------
                # Primera forma#FactoMineR
###############################################
# Primera forma - usando plot.CA de FactoMineR#
###############################################
#plot.CA(res.ca) # Mapa Simétrico#1
#
#-----------------------------------------------------------------------
plot.CA(res.ca, axes = c(1,2), col.row = "yellow3", col.col = "red")#2
#me quedo,porque aveces se tendra que explicar con mas de 2 componentes. 
#-----------------------------------------------------------------------
#
#plot.CA(res.ca,mass=c(T,T))#3
colors()#colores
                # Segunda forma#factoextra
######################################################
# Segunda forma - usando fviz_ca_biplot de factoextra#
######################################################
fviz_ca_biplot(res.ca, repel = T)#1
fviz_ca_biplot(res.ca, repel = T) + theme_minimal()#2
fviz_ca_biplot(res.ca, repel = T) + theme_light()#3
fviz_ca_biplot(res.ca, repel = T) + theme_void()#4
fviz_ca_biplot(res.ca, repel = T) + theme_test()#5
#--------------------------------------------------------------
# Coordenadas de las Dimensiones para filas y columnas
#insumo para usar en otro modelo(regresion).
#filas
row <- get_ca_row(res.ca)
row$coord#contribucion absoluta
row$coord#contribucion relativa
#columnas
col <- get_ca_col(res.ca)
###################################
#con summary tambien sale lo mismo#completo
###################################
summary(res.ca)
#---------------------------------------------------------------
# Gráficos de las contribuciones absolutas de las filas 
# y columnas a cada dimensión
head(row$contrib)#numerico
head(col$contrib)#numerico
#------
#filas
#------
fviz_contrib(res.ca, choice = "row", axes = 1)#axes=1(dimension1)#bajo,muy alto
fviz_contrib(res.ca, choice = "row", axes = 2)#axes=2(dimension2)#medio
#¿alto?#
#sera explicado con la contribucion relativa(cos2)
#--------
#columnas
#--------
fviz_contrib(res.ca, choice = "col", axes = 1)#axes=1(dimension1)#bueno
fviz_contrib(res.ca, choice = "col", axes = 2)#axes=2(dimension2)#regular
#¿malo?
#sera explicado con la contribucion relativa(cos2)
#
#----------------------------------------------------------------------------------
#
# Gráficos de las contribuciones relativas de cada dimensión
head(row$cos2)#numerico#filas
head(col$cos2)#numerico#columnas
#recodar que tanto las contribuciones absolutas y contribuciones relativas suman 1.
#------
#filas
#------
#¿alto?
fviz_cos2(res.ca, choice = "row", axes = 1)#axes=1(componente1 o dimension1)
fviz_cos2(res.ca, choice = "row", axes = 2)#axes=2(componente2 o dimension2)
#vemos que es mejor explicada por el componente 1 la categoria o nivel 'alto'.
#----------
#columnas
#----------
#¿malo?
fviz_cos2(res.ca, choice = "col", axes = 1)#axes=1(componente1 o dimension1)
fviz_cos2(res.ca, choice = "col", axes = 2)#axes=2(componente2 o dimension2)
#vemos que es mejor explicada por el componente 1 la categoria o nivel 'malo'.
#------------------------------------------------------------------------------
#significancia de la asociacion entre fila y columnas
eig <- factoextra::get_eigenvalue(res.ca)
eig

trace <- sum(eig[,1])  ; trace#0.0801,inercia total que quiero explicar
#-----------------------
#AHORA--Correlacion entre filas y columnas,si es mayor 0.2
#-----------------------
cor.coef <- sqrt(trace)
cor.coef
# Como regla, un valor por encima de 0.2 indica 
# una correlación que puede ser considerada importante
# (Bendixen 1995, 576; Healey 2013, 289-290).
# ChiCuadrado = Traza * (Total de Tabla)
#---------------------
trace*500#chi cuadrado total
prueba#chi_cuadrado total#X-squared = 40
40/500#inercia_total
trace*500/500#inercia total
#----------------------------------------------------------------
################################
# 11. ACS de una base de datos #
################################
library(foreign)
datos <- read.spss("Riesgo_morosidad.sav", 
                   use.value.labels = T,  
                   to.data.frame=TRUE)
attach(datos)   

table(nrodepen)
table(dpto)
#vriables que me interesan en este caso es 'dpto' y 'nrodepen'
addmargins(table(dpto,nrodepen))
#priemro lo llevo a una tabla,luego lo convierto en una matrix
datos.acs.b <- as.matrix(table(dpto,nrodepen))
datos.acs.b
############################
#todo lo que viene es igual#
############################
library(FactoMineR) 
res.ca.b <- CA(datos.acs.b,ncp=2,graph=FALSE)
res.ca.b$eig#autovalores#0.10762638+0.00281088=0.11043726#inercia total
library(factoextra)
eig.val <- get_eigenvalue(res.ca.b)
eig.val#autovalores
#eig.val[,1]
#   Dim.1      Dim.2      Dim.3      Dim.4      Dim.5 
#0.10762638 0.00281088 0.00075654 0.00012526 0.00000279 
sum(eig.val[c(1,2),1])#inercia total#0.10762638+0.00281088 =0.11043726#inercia total
fviz_screeplot(res.ca.b)
summary(res.ca.b,nb.dec = 3, ncp = 2)#PIURA,COLUMNA 0 ,estan cerca al origen.
res.ca.b$row$inertia
sum(res.ca.b$row$inertia)#inercia total con 5 componentes#0.111#summary(res.ca.b,nb.dec = 3, ncp = 2)
sum(res.ca.b$eig[,1])#inercia total con 5 componentes#0.111
#inercia total#0.111,si utilizaria 5 componentes y siguiendo la regla
#min(filas,columnas)-1=5,pero por buenas practicas son 2 componentes ademas
#si me fijo en la variblidad retenida 
#con dos componentes retengo 99.2% de variabilidad total.
#res.ca.b$row$inertia
#[1] 0.029097 0.037206 0.037576 0.004783 0.002249 0.000411
#----------------------------------------------------------
#
#----------------BIPLOT------------
fviz_ca_biplot(res.ca.b, repel = T)
#
#---------------------------------------------------------
############################### 
#ACS con puntos suplementarios#ingresando en columna'ideal'
###############################
#             E1	E2	E3	E4	E5	E6	E7	E8	E9	Ideal
# Precios	    16	17	18	19	16	45	15	19	18	45
# Variedad	   8	15	18	17	27	20	 2	14	53	53
# Rapidez	    20	20	23	21	29	20	18	19	25	29
# Información	11	13	12	17	20	16	15	10	44	44
# Trato	      28	25	25	22	30	26	24	22	26	30
# Condiciones	21	21	20	24	27	22	18	21	24	27
# Acceso	    21	21	21	23	26	15	16	18	21	26

datos_s.acs <- matrix(c(16,17,18,19,16,45,15,19,18,45,
                        8,15,18,17,27,20, 2,14,53,53,
                        20,20,23,21,29,20,18,19,25,29,
                        11,13,12,17,20,16,15,10,44,44,
                        28,25,25,22,30,26,24,22,26,30,
                        21,21,20,24,27,22,18,21,24,27,
                        21,21,21,23,26,15,16,18,21,26),
                      nrow=7,byrow=T)

datos_s.acs

# Asignación de nombres a las filas y columnas de la tabla
dimnames(datos_s.acs)<-list(Atributos=c("Precios", "Variedad", "Rapidez", 
                                        "Información","Trato","Condiciones","Acceso")
                            ,Empresa=c("Empresa 1","Empresa 2","Empresa 3",
                                       "Empresa 4","Empresa 5","Empresa 6",
                                       "Empresa 7","Empresa 8","Empresa 9",
                                       "Ideal"))
datos_s.acs
addmargins(datos_s.acs)
#----------------------------------------------------------------------------------------
# Prueba de Independencia Chi-Cuadrado  
chisq.test(datos_s.acs[,-10])#no se toma en cuenta la empresa 'ideal'
#grados libertad = (filas-1)*(columnas-1)=48
(gl=(7-1)*(9-1))
#--------------------------------------------------------------
# ACS con el paquete FactoMiner   
library(FactoMineR) 
res.ca.s <- CA(datos_s.acs,
               ncp=2,
               graph=FALSE,
               col.sup = 10)#la columna suplmentario es 'ideal'
#va hacer un analisis de correspondecia simple sin la empresa 'ideal'
#luego va simular cuales serian sus coordenadas en la dimension que yo he decidido 
#retener.
#--------------------------------------------------------------
# Scree Plot de los Autovalores
get_eigenvalue(res.ca.s)#autovalores#91.1 % de la inercia total=0.052028+0.023084=0.0751
autovaloes=get_eigenvalue(res.ca.s)
sum(autovaloes[,1])#0.0824,inercia total con 6 componentes
fviz_screeplot(res.ca.s, addlabels = TRUE, ylim = c(0, 80))#graficando los autovalores
#--------------------------------------------------------------
# Interpretación de los Indicadores del ACS
summary(res.ca.s,nb.dec = 3, ncp = 2)#nb.dec=3 decimales#ncp = 2,componentes
sum(res.ca.s$row$inertia)#0.0824,inercia total
sum(res.ca.s$col$inertia)#0.0824,inercia total
#--------------------------------------------------------------
# Biplot filas, columnas y columna suplementaria
fviz_ca_biplot(res.ca.s, repel = T) + theme_light()
#observamos que la empresa 9 esta mas cerca a la empresa ideal,la empresa 9
#tiene como atributos por la grafica (informacion,variedad),tendria que corroborar
#con los indicadores.
#la empresa 1 es lo opuesto esta muy lejano a la empresa ideal,pero tendria que corroborar
#con los indicadores.
#-------------------------------------------------------------
#---------------------------------------------------------
############################### 
#ACS con puntos suplementarios#ingresando en fila'ideal'
###############################
############################### 
#ACS con puntos suplementarios#ingresando en fila'ideal'
###############################
datos<- matrix(c(30,	30,	155,
                 30,	130,	30,
                 80,	30,	30,
                 80,	30,	5,
                 80,130,155 )#fila ideal
               ,ncol=3,byrow=T,#si ingrese fila 'ideal',en fx de columna(ncol=3)
               dimnames = list(c('A','B','C','D','Ideal'),
                               c('Segmento1','Segmento2','Segmento3')))
datos
# ACS con FactoMineR

# Prueba de Independencia
chisq.test(datos[-5,])
# Los segmentos son dependientes de las marcas (Pvalor<alfa)

# ACS (2 dimensiones)
library(FactoMineR)
datos.acs.s <-CA(X=datos,ncp=2,row.sup=5,graph=T)

summary(datos.acs.s)
