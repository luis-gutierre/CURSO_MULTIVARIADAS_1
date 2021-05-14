#####################
#ejercicio_del_excel#
#####################
#
###########################################
#VAMOS A JUGAR CON LAS FILAS#OH SII!!! :')#
###########################################
#
#####################
#1. Ingreso de Datos#
#####################
#Antes de analizar el analisis de correspondencia se
#ingresan los datos, se calculan las frecuencias y se 
#realizan los graficos de perfiles.
#
rm(list=ls()) #limpiar el environment
graphics.off()#limpiar las graficas
#
library(pacman)
p_load(MASS, ca, anacor,FactoMineR,vegan,
       gplots,vcd,graphics,factoextra,foreign)
#
#frecuencia-absoluta#datos-observados
#
#
#  segmento1 segmento2 segmento3
#A  30	      30	       155
#B  30	      130       	30
#C  80	      30         	30
#D  80	      30         	5
#filas
datos.acs <- matrix(c(30,	30,	155,
                      30,	130,	30,
                      80,	30,	30,
                      80,	30,	5),nrow=4,byrow=T) #ncol = 3
#
#¿la informacion es por fila?
#si porque'byrow=T', si hubiera puesto 'byrow=F' la informacion
#seria por columna.
#'nrow' hace referencia al numero de filas que deseamos obtener ,
#la columna lo pone por defecto, en este ejemplo el numero de columna
#por defecto es 3.
datos.acs
# Asignación de nombres a las filas y columnas de la tabla
dimnames(datos.acs)<-list(marca=c("A", "B", "C", "D")
                          ,segmentos=c("segmento1","segmento2","segmento3"))

#datos.acs#es una lista
datos.acs
addmargins(datos.acs)
#
#frecuencias absolutas
#tengo los totales tanto por fila 
#y tambien los totales por columna
#
#----------------------------------------------------- 
#                   #PRIMER PASO#             
######################################################
#representando la tabla de contigencia en una grafica#trabajando-perfil fila#datos estan en una tabla
######################################################
#--------------------------------------------------------
#Primera forma- Balloonplots
#--------------------------------------------------------
library(gplots)
#
#######
#TABLA#
#######
#
    ####################################################### 
#1# #Convertir los datos en una tabla,antes era una lista.#
    ####################################################### 
#TABLA_DE_FRECUENCIAS_ABSOLUTAS
dt <- as.table(datos.acs)

dt
tabla_frecuencias_absolutas=addmargins(dt)#aqui decidimos si trabajamos con perfil fila o perfil columna
tabla_frecuencias_absolutas
#               segmentos
#marca segmento1 segmento2 segmento3 Sum
#A          30        30       155   215
#B          30       130        30   190
#C          80        30        30   140
#D          80        30         5   115
#Sum       220       220       220   660
#
perfil_filas_media=tabla_frecuencias_absolutas[,4]/tabla_frecuencias_absolutas[5,4]
perfil_filas_media

#PERFIL_FILAS_MEDIAS
#
#A         B         C         D       Sum 
#0.3257576 0.2878788 0.2121212 0.1742424 1.0000000
#
############################
#TRABAJAMOS CON PERFIL FILA#
############################
str(dt)
  ############################################# 
#2# Para graficarlo con % fila (perfiles fila)#
  ############################################# 
#TABLA_DE PERFIL_FILA
perfil_filas<- prop.table(dt,margin=1)#perfil fila
perfil_filas
#
#                  segmentos
#marca  segmento1  segmento2  segmento3
#A      0.13953488 0.13953488 0.72093023
#B      0.15789474 0.68421053 0.15789474
#C      0.57142857 0.21428571 0.21428571
#D      0.69565217 0.26086957 0.04347826
#
#REPRESENTANDO EN UNA GRAFICA
balloonplot(t(perfil_filas), #siempre se pone la transpuesta sea fila o columna.
            main ="Gráfico Opinión Marca-Segmento(perfil-fila)", 
            xlab ="Segmento", 
            ylab="Marca",
            label = F, cum.margins=F, 
            label.lines=F, show.margins = FALSE)
#----------------------------------------------------- 
#                   #SEGUNDO PASO#             
######################################################
#
#HACER LA PRUEBA CHI-CUDRADO,PARA VER SI PUEDO REALIZAR
#ANALISIS DE CORREPONDENCIA.
#H0:LOS NIVELES DE LA VARIABLES SON INDEPENDIENTES
#H0:LOS NIVELES DE LA VARIABLES SON DEPENDIENTES
#
#H0:La opinion sobre las marcas es independiente del nivel de segmentos de 
#los contribuyentes.
#H1:...dependiente
prueba_Chi_Cuadrado=chisq.test(datos.acs)
prueba_Chi_Cuadrado
362.41/660#inercia total
#Pearson's Chi-squared test
#data:  datos.acs
#X-squared = 362.41, df = 6, p-value < 2.2e-16
#CONCLUSION:
#Se rechaza la H0 ,la opinion sobre las marcas depende de los segmentos.
#
#
prueba_Chi_Cuadrado$observed#valores observados#frecuencia absoluta
prueba_Chi_Cuadrado$expected#valores esperados
#¿como salio el valor esperado de 'marca A y segmento1'=71.66667?
#usamos -> prueba$expected
addmargins(prueba_Chi_Cuadrado$expected)
#                segmentos
#marca segmento1 segmento2 segmento3 Sum
#A    71.66667  71.66667  71.66667 215
#B    63.33333  63.33333  63.33333 190
#C    46.66667  46.66667  46.66667 140
#D    38.33333  38.33333  38.33333 115
#Sum 220.00000 220.00000 220.00000 660
#
#si asumimos que es independiente
#P(marca A Y segmento1)=P(marca A)*P(segmento1)
#(215/660)*(220/660)
#pero,como son 500 encuestados:
#660*(215/660)*(220/660)=71.66667
#-----------------------------------
# Frecuencia Relativa (fij)
prop.table(datos.acs) 
# Perfiles Fila
prop.table(datos.acs, 1) 
# Tabla con el paquete gmodels y función CrossTable()
library(gmodels)
CrossTable(datos.acs,
           prop.t=F,  # Frecuencia Relativa
           prop.r=F,  # Perfil Fila
           prop.c=F,  # Perfil Columna
           prop.chisq=T)
##################################### 
# Analisis de Correspondencia Simple#
#  con el paquete FactoMiner        #
#####################################
#
library(FactoMineR) 
#El 100% de la inercia total,se puede descomponer en 
#min(numero de filas,numero de columnas)-1=MIN(4,3) -1=3-1=2
#componentes o dimensiones.
res.ca <- CA(datos.acs,ncp=2,graph=TRUE)
res.ca#ATRIBUTOS
res.ca$eig#autovalores
res.ca$col
res.ca$row
res.ca$call
#       eigenvalue variance.percent cumulative.variance.percent
#Dim.1  0.3404987         62.00909                    62.00909
#Dim.2  0.2086122         37.99091                   100.00000
res.ca$eig[,1]
sum(res.ca$eig[,1])#inercia total ,tanto matrix fila como matrix columna 
#explicaran la misma variabilidad total 0.549111.
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 100))#grafica de screp plot con los autovalores.
summary(res.ca,nb.dec = 3, ncp = 2)
prueba_Chi_Cuadrado#X-squared = 362.41
362.41/660#inercia total
# PAQUETE ANACOR
#--------------
library(anacor)
fit2 <- anacor(datos.acs,ndim=2)
str(fit2)

summary(fit2)
plot(fit2,plot.type="jointplot")  # Grafico Biplot
plot(fit2)
fit2$row.scores  # coordenadas fila

# Autovectores
#-------------
fit2$left.singvec#left:filas
fit2$right.singvec#right:columnas