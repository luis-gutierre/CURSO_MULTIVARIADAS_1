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
res.ca <- CA(datos.acs,ncp=2,graph=TRUE)
#datos.acs:MATRIX

res.ca

#--------------------------------------------------------------
# Scree Plot de los Autovalores
res.ca$eig#autovalor#FactorMineR#MEquedoConEsto

eig.val <- get_eigenvalue(res.ca)
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
#------------------------------------------------------------
