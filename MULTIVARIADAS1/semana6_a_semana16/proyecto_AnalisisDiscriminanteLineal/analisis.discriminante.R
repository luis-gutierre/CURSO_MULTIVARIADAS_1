#https://bookdown.org/jsalinas/tecnicas_multivariadas/ad.html
options(scipen=999)      # Eliminar la notación científica
options(digits = 3)      # Número de decimales
library(pacman)
p_load(MASS, dplyr, ggplot2, klaR, psych, gains, caret, Boruta, gmodels,vegan, MLmetrics, vcd, Epi, InformationValue,ROCit)
#
Patrimonio   <- c(1.3,3.7,5,5.9,7.1,4,7.9,5.1,5.2,9.8,9,12,6.3,8.7,11.1,9.9)
Deuda        <- c(4.1,6.9,3,6.5,5.4,2.7,7.6,3.8,1,4.2,4.8,2,5.2,1.1,4.1,1.6)
Grupo        <- c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0)

datosd <- data.frame(Patrimonio,Deuda,Grupo)
str(datosd)
#
datosd$Grupo <- factor(datosd$Grupo,levels=c(0,1),
                       labels=c("NoFallido","Fallido"))
getwd()
# No Fallido: no moroso (0)
# Fallido: deudor (1)

#write.csv(datosd,"data fallidos.txt",row.names = F)
contrasts(datosd$Grupo)
#Fallido
#NoFallido       0     fracaso    #VAMOS ASUMIR   HAY UUNA REGLA PRA LA HIPOTESIS
#Fallido         1     exito

#EL_RSTUDIO:probalidad(fallido)
str(datosd)

prop.table(table(datosd$Grupo)) # Data Balanceada
#NoFallido   Fallido 
#0.5       0.5 

#GRAFICA DE PUNTOS DE LA VARIABLE PATRIMONIO Y LA VARIABLE DEUDA
library(ggplot2)
ggplot(datosd) + aes(x=Patrimonio,y=Deuda,color=Grupo) + 
  geom_point()  + 
  scale_x_continuous(breaks=seq(0,12,2)) +
  scale_y_continuous(breaks=seq(0,8,1)) + 
  theme_bw() +
  theme(panel.grid = element_blank())

#
#
#
#------------------------------------------------------------------

############
#PATRIMONIO#
############

tapply(datosd$Patrimonio,datosd$Grupo,mean)

C1 <- (5+9)/2  ; C1

datosd$clase.pred1 <- ifelse(datosd$Patrimonio<C1,1,0)
datosd$clase.pred1 <- factor(datosd$clase.pred1,levels = c(0,1),
                             labels = c("NoFallido","Fallido"))
#matriz de confusion
table(Clase_Real=datosd$Grupo,
      Clase_Predicha=datosd$clase.pred1)

table(Clase_Predicha=datosd$clase.pred1,
      Clase_Real=datosd$Grupo)


datosd$Grupo
cbind(datosd$Grupo,datosd$clase.pred1)
mean(datosd$Grupo==datosd$clase.pred1) # Accuracy: porcentaje de acierto

mean(datosd$Grupo!=datosd$clase.pred1) # Tasa de error del modelo

ggplot(datosd) + aes(x=Patrimonio,fill=Grupo) + 
  geom_histogram(alpha = 0.25, position="identity") +
  theme_bw() + 
  geom_vline(xintercept = C1, linetype = "longdash") +
  scale_x_continuous(breaks = seq(0,13,1))
#------------------------------------------------------------------

#######
#DEUDA#
#######


tapply(datosd$Deuda,datosd$Grupo,mean)

C2 <- (3+5)/2  ; C2 #4

datosd$clase.pred2 <- ifelse(datosd$Deuda>C2,1,0)
datosd$clase.pred2 <- factor(datosd$clase.pred2,levels=c(0,1),
                             labels=c("NoFallido","Fallido"))

table(Clase_Real=datosd$Grupo,
      Clase_Predicha=datosd$clase.pred2)

cbind(datosd$Grupo,datosd$clase.pred2)
mean(datosd$Grupo==datosd$clase.pred2) # Accuracy

mean(datosd$Grupo!=datosd$clase.pred2) # Tasa de error

ggplot(datosd) + aes(x=Deuda,fill=Grupo) + 
  geom_histogram(alpha = 0.25, position="identity") +
  theme_bw() + 
  geom_vline(xintercept = C2, linetype = "longdash") +
  scale_x_continuous(breaks = seq(0,8,1))

########################################################### 
#3.Clasificacion con dos grupos y dos variable predictoras#
###########################################################

library(MASS)
modelo <- lda(Grupo ~ Patrimonio + Deuda,datosd)

str(modelo)

modelo

modelo$means

# Usando el modelo
datosd$D <- -0.422*datosd$Patrimonio + 0.380*datosd$Deuda 

mean(datosd$D)

tapply(datosd$D,datosd$Grupo,mean)

C3 <- (-2.66-0.21)/2  ; C3

datosd$clase.pred3 <- ifelse(datosd$D>C3,1,0)
datosd$clase.pred3 <- factor(datosd$clase.pred3,levels=c(0,1),
                             labels=c("NoFallido","Fallido"))

table(Clase_Real=datosd$Grupo,
      Clase_Predicha=datosd$clase.pred3)
##tabla _ poderosa # tabla de confusion

tabla=table(Clase_Real=datosd$Grupo,
            Clase_Predicha=datosd$clase.pred3)

addmargins(tabla)
#               Clase_Predicha
#Clase_Real  NoFallido Fallido Sum
#NoFallido         7       1   8
#Fallido           0       8   8
#Sum               7       9  16

mean(datosd$Grupo==datosd$clase.pred3)# Accuracy: porcentaje de acierto

mean(datosd$Grupo!=datosd$clase.pred3)# Tasa de error del modelo

ggplot(datosd) + aes(x=D,fill=Grupo) + 
  geom_histogram(alpha = 0.25, position="identity") +
  theme_bw() + 
  geom_vline(xintercept = C3, linetype = "longdash") +
  scale_x_continuous(breaks = seq(-5,5,1))

#NOTA.-
#datosd3$D= -0.42249*datosd$Patrimonio + 0.38022*datosd$Deuda
#C3=-1.4365
#ifelse(datosd$D>c3,1,0)
#D-c3>0
#-0.42249*Patrimonio +0.38022$Deuda + 1.43465>0
#------------------------------------------------------------------
#forma1
datosd$dif <- datosd$D - C3            #paso a paso
datosd$dif
# [1]  2.4444  2.4956  0.4650  1.4152  0.4908  0.7730  0.9892  0.7268 -0.3794 -1.1046 -0.5390 -2.8690  0.7524 -1.8184 -1.6912
#[16] -2.1348

#forma2
predicciones <- predict(modelo)        #forma rapida
predicciones
str(predicciones)

#1   2.4462261
#2   2.4968690
#3   0.4647612
#4   1.4152977
#5   0.4900625
#6   0.7731863
#7   0.9885588
#8   0.7266901
#9  -0.3801825
#10 -1.1069328
#11 -0.5408057
#12 -2.8729048
#13  0.7520115
#14 -1.8208819
#15 -1.6941946
#16 -2.1377609

##################################
#forma1 y forma2 son equivalentes#
##################################


#------------------------------------------------------------------

# Igual a la diferencia de D y C3  #ifelse(datosd$D-C3>0,1,0)  
#AYUDA VISUALIZAR SI SON POSITIVOS LA ETIQUETA SERA fallido
#AYUDA VISUALIZAR SI SON NEGATIVOS LA ETIQUETA SERA nofallido
predicciones$x
#1   2.4462261
#2   2.4968690
#3   0.4647612
#4   1.4152977
#5   0.4900625
#.
#.
#16  -2.1377609

cbind(predicciones$x,datosd$dif)   #hecho con el modelo  "predicciones$x" , hecho paso a paso  "datosd$dif"
plot(modelo)
#-----------------------------------------
# Otra forma #HITOGRAMA
ldahist(data = predicciones$x, g = datosd$Grupo)
#-----------------------------------------
#GRAFICA DE BARRITAS
ggplot(datosd) + aes(x=dif,fill=Grupo) + 
  geom_histogram(alpha = 0.25, position="identity") +
  theme_bw() + 
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks = seq(-5,5,1))
#-----------------------------------------
#-----------------------------------------
#-----------------------------------------
#-----------------------------------------

############
#IMPORTANTE#
############

##########################################################
#el 0 siempre va ser la condicion normal                 #
#el 1 el cambio es el que se escapa de ese grupo normal  #
##########################################################

#EJEMPLO_1
#A una persona a cual yo le preste dinero termino 
#siendo morosa

#EJEMPLO_2
#A un cliente a cual yo le suscribe dandole una buena promocion
#termino fugando de la empresa 


#lo que ami me interesa de esas 2 probalidades
#es la probalidad de fallidos





predicciones$posterior               #SE ELIGE EL MAYOR 
#NoFallido      Fallido
#1  0.002486811 0.9975131890         #AL PRIMER INDIVIDUO LO VA A PREDECIR COMO FALLIDO
#2  0.002197226 0.9978027741
#3  0.242530318 0.7574696815
#4  0.030234699 0.9697653007
#5  0.231323058 0.7686769420
#6  0.130717952 0.8692820481
#7  0.081482154 0.9185178455
#8  0.144217853 0.8557821467
#9  0.717400615 0.2825993848        #AL NOVENO INDIVIDUO LO VA PREDECIR COMO NOFALLIDO
#10 0.937756867 0.0622431333
#11 0.790045099 0.2099549005
#12 0.999124468 0.0008755318
#13 0.136727709 0.8632722914
#14 0.988591388 0.0114086116
#15 0.984502783 0.0154972169
#16 0.994719274 0.0052807262
datosd$proba <- predicciones$posterior[,2]   #ELIGO LA SEGUNDA COLUMNA PORQUE ES MI EXITO (FALLIDO)


predicciones$class # Punto de corte por defecto= 0.5 (umbral)
#[1] Fallido   Fallido   Fallido   Fallido   Fallido   Fallido   Fallido   Fallido   NoFallido NoFallido NoFallido NoFallido
#[13] Fallido   NoFallido NoFallido NoFallido

cbind(predicciones$posterior,predicciones$class)
#   NoFallido      Fallido  
#1  0.002486811 0.9975131890 2
#2  0.002197226 0.9978027741 2
#3  0.242530318 0.7574696815 2
#4  0.030234699 0.9697653007 2
#5  0.231323058 0.7686769420 2
#6  0.130717952 0.8692820481 2
#7  0.081482154 0.9185178455 2
#8  0.144217853 0.8557821467 2
#9  0.717400615 0.2825993848 1
#10 0.937756867 0.0622431333 1
#11 0.790045099 0.2099549005 1
#12 0.999124468 0.0008755318 1
#13 0.136727709 0.8632722914 2
#14 0.988591388 0.0114086116 1
#15 0.984502783 0.0154972169 1
#16 0.994719274 0.0052807262 1

#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
#Con función de densidad
ggplot(datosd) + aes(x=proba,color= Grupo,fill=Grupo) + 
  geom_density(alpha = 0.25) + theme_bw() + 
  geom_vline(xintercept = 0.5, linetype = "longdash") + theme_bw()

#Con histograma
ggplot(datosd) + aes(x=proba,color= Grupo,fill=Grupo) + 
  geom_histogram(alpha = 0.25) + theme_bw() + 
  geom_vline(xintercept = 0.5, linetype = "longdash") + theme_bw()

cbind(GrupoReal=datosd$Grupo,predicciones$posterior,GrupoPredicho=predicciones$class)
#GrupoReal   NoFallido      Fallido GrupoPredicho
#1          2 0.002486811 0.9975131890             2
#2          2 0.002197226 0.9978027741             2
#3          2 0.242530318 0.7574696815             2
#4          2 0.030234699 0.9697653007             2
#5          2 0.231323058 0.7686769420             2
#6          2 0.130717952 0.8692820481             2
#7          2 0.081482154 0.9185178455             2
#8          2 0.144217853 0.8557821467             2
#9          1 0.717400615 0.2825993848             1
#10         1 0.937756867 0.0622431333             1
#11         1 0.790045099 0.2099549005             1
#12         1 0.999124468 0.0008755318             1
#13         1 0.136727709 0.8632722914             2
#14         1 0.988591388 0.0114086116             1
#15         1 0.984502783 0.0154972169             1
#16         1 0.994719274 0.0052807262             1

#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
#GRAFICA DE PUNTOS DE LA VARIABLE PATRIMONIO Y LA VARIABLE DEUDA
library(ggplot2)
ggplot(datosd) + aes(x=Patrimonio,y=Deuda,color=Grupo) + 
  geom_point()  + 
  scale_x_continuous(breaks=seq(0,12,2)) +
  scale_y_continuous(breaks=seq(0,8,1)) + 
  theme_bw() +
  theme(panel.grid = element_blank())


#Usar solo para 2 variables
library(klaR)

# Análisis discriminante lineal
partimat(Grupo ~ Deuda + Patrimonio, data = datosd, method = "lda")

# Análisis discriminante cuadratico
partimat(Grupo ~ Deuda + Patrimonio, data = datosd, method = "qda")

#PREDICIENDO NUEVOS INDIVIDUOS 

Patrimonio <- c(10.1,9.7)
Deuda      <- c(6.8,2.2)

nuevos.casos <- data.frame(Patrimonio,Deuda)
#     Patrimonio Deuda
#1       10.1   6.8
#2        9.7   2.2
nuevas.predicciones <- predict(modelo,nuevos.casos)
nuevas.predicciones$posterior

nuevas.predicciones$class
nuevos.casos 
#           Patrimonio Deuda
#1              10.1   6.8
#2              9.7   2.2
#
#cliente1       10.1   6.8
#cliente2       9.7   2.2

#---------------------------------------------------------
 
#modelo <- lda(Grupo ~ Patrimonio + Deuda,datosd)

#Group means:
#               Patrimonio Deuda
#NoFallido          9     3
#Fallido            5     5

 
#---------------------------------------------------------

#Usando el Criterio de la Distancia de Mahalanobis
#Caso general
nuevos.casos <- data.frame(Patrimonio=1,Deuda=4)
nuevos.casos

cov_nf <- by(datosd[,c(1,2)],datosd$Grupo,cov)[[1]]
cov_nf # matriz varianza-covarianza de los no fallidos

cov_f  <- by(datosd[,c(1,2)],datosd$Grupo,cov)[[2]]
cov_f  # matriz varianza-covarianza de los fallidos

cov <- (7*cov_nf + 7*cov_f)/(14) #14 es el total de aciertos
cov                              #7 y 7 se reparte de forma proporcional

library(dplyr)
datosd %>%  select(Patrimonio,Deuda,Grupo) %>% 
  group_by(Grupo) %>%
  summarise_if(is.numeric,mean) %>% as.data.frame-> centroides
centroides
#       Grupo  Patrimonio Deuda
#1   NoFallido        9     3
#2   Fallido          5     5         #9+5=14


#Distancia del individuo a los No Fallidos
centroides_nf <- as.matrix(centroides[1,-1]); centroides_nf
#Patrimonio Deuda
#       9     3
solve(cov)
#            Patrimonio       Deuda
#Patrimonio  0.22436797 -0.06890388
#Deuda      -0.06890388  0.32804348

#------------------------------------------------------------

#nuevos.casos
#Patrimonio Deuda
#       1     4

#centroides_nf
#Patrimonio Deuda
#       9     3

as.matrix(nuevos.casos-centroides_nf)%*%solve(cov)%*%
  t(as.matrix(nuevos.casos-centroides_nf))
#15.79006

# De Otra Forma
mahalanobis(nuevos.casos, centroides_nf, cov)  
#15.79006     # esla distncia del individuo al grupo de los NoFallidos
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
#47:00

#Distancia del individuo a los Fallidos

centroides_f <- as.matrix(centroides[2,-1]); centroides_f

as.matrix(nuevos.casos-centroides_f)%*%solve(cov)%*%
  t(as.matrix(nuevos.casos-centroides_f))

# Otra Forma 
mahalanobis(nuevos.casos, centroides_f, cov)

#------------------------------------------------------------

#7.2.2 Ejemplo 2. Ejemplo Suscripción
#Este ejemplo fue tomado del libro de Uriel.

#Uriel,E. y Aldas, J. (2002). Análisis Multivariante Aplicado. 
#Aplicaciones al marketing, investigación de mercados, economía, 
#dirección de empresas y turismo. Ediciones Paraninfo

#Ingreso de Datos

#La compañía de cable edita y promociona una revista de cine (de edición mensual) 
#a un grupo (442) de sus suscriptores durante 6 meses. Al cabo de dicho periodo 
#le ofrece la posibilidad de suscribirse a dicha revista. 
#De los 442 clientes a los que se ofreció la promoción, 
#se suscribieron a la revista 329 y no se suscribieron 113.


#p_load(psych, MASS, klaR, gains, caret, Boruta, gmodels,vegan,
#       MLmetrics, vcd, Epi, InformationValue, ROCit)

#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
library(foreign)
datosd <- read.spss("suscripcion-discriminante.sav",
                    use.value.labels=TRUE, 
                    max.value.labels=TRUE,
                    to.data.frame=TRUE)
attr(datosd,"variable.labels") <- NULL # para quitar las variables names
str(datosd)

datosd$Suscripcion <- factor(datosd$Suscripcion)
datosd$Suscripcion <- factor(datosd$Suscripcion,
                             levels=c(0,1),
                             labels=c("No","Si"))
#h0:"0" PERSONAS QUE NO SE SUSCRIBIERON      (NORMAL)
#h1:"1" PERSONAS QUE SI SE SUSCRIBIERON       (RARO)
contrasts(datosd$Suscripcion)
#    Si
#No  0    
#Si  1    representa q si suscriban (exito)  "que yo RECHAZE la hiportesis planteada"

#H0: no suscribieron
#H1: si se suscribieron

#---------------------------------------------------------------------------------------

#1 es lo que se estudia(alterna),si afirmamos el 1 ,rechazamos la H0 ES DECIR EL  0. 

# 1.  Actual ()                    0    vs Fuga ()                       1#             
# 2.  Aprobado ()                  0    vs Desaprobado () + Éxito        1
# 3.  Con virus ()                 1    vs Sin Virus ()                  0#
# 4.  Moroso ()                    1    vs No Moroso ()                  0
# 5.  Fraude ()                    1    vs No Fraude ()                  0
# 6.  No tiene cáncer ()           0    vs Si tiene cáncer ()            1
# 7.  Está embarazada ()           1    vs No está embarazada ()         0#
# 8.  Culpable ()                  1    vs Inocente ()                   0#
# 9.  Termina universidad ()       0    vs Abandona universidad ()       1#
# 10. Segunda vuelta Candidato A () vs Candidato B ()                    indistinto

#---------------------------------------------------------------------------------------






str(datosd)
attach(datosd)

datosd$Suscripcion <- relevel(datosd$Suscripcion, ref="No") # El 0 ES LA SITUACION NORMAL(si dessamos cambiar)
contrasts(datosd$Suscripcion)
#  Si
#No  0
#Si  1   #vamos a estudiar a los que si se suscribieron

#Explorando las variables predictoras
library(funModeling)       # Pablo Casas Udemy

###################################################### 
################IMPORTANTE############################
######################################################
#names(datosd)     #TODOS LOS NOMBRES QUE HAY EN MIS "datosd"
#"Educacion"      "Edad"           "Tvdiario"       "Organizaciones" "Hijos"          "Suscripcion" 


#--------------------------------------------------------------
# Reduciendo el código

#la fx "setdiff" quitale el target "Suscripcion"(NO LO VA ELIMINAR), lo guarda en el objeto predictores

#predictores <- setdiff(names(datosd), "Suscripcion")
#predictores
#target=datosd$Suscripcion
#"Educacion"      "Edad"           "Tvdiario"       "Organizaciones" "Hijos"  


#FORMAS DE CREAR UN MODELO

#lda(Suscripcion~.,datosd)                       #clasica Y ~ .

#lda(datosd[,predictores],datosd[,Suscripcion])  #otra forma (nivel Dios)
#lda(datosd[,predictores],datosd[,target])        # 
##############################################################################
#PARA TRABAJAR EN CONSULTARIA                                                #
#                                                                            #
#predictores <- setdiff(names(datosd), "Suscripcion")                        #
#predictores                                                                 #
#target=datosd$Suscripcion                                                   #  
#                                                                            #
##############################################################################
#BUENA COMBIANACION
predictores <- setdiff(names(datosd), "Suscripcion")
predictores

#datosd$Suscripcion=factor(datosd$Suscripcion,levels=c("No","Si"),
#                          labels=c(0,1))
target=datosd$Suscripcion

class(target)#factor

library(MASS)
lda(datosd[,predictores],datosd[,target])

#--------------------------------------------------------------
#predictores
#"Educacion"      "Edad"           "Tvdiario"       "Organizaciones" "Hijos"



# Boxplot por cada variable predictora vs target
plotar(datosd, target = "Suscripcion", input = predictores,
       plot_type = "boxplot")



#DISCRETIZACION
#cuando la v.predictora es numerica ------>categorica

# Gráfico de Barras Apilado en proporción por cada variable predictora vs target
cross_plot(datosd, 
           input=predictores, 
           target="Suscripcion",
           plot_type = "percentual") #both  #quantity

#----------------------------------------------------------------------------------------


# Media por cada variable predictora vs. target
tapply(Educacion,Suscripcion,mean)

#otra forma

library(dplyr)
datosd %>% group_by(Suscripcion) %>% summarize(mean(Educacion))

tapply(Edad,Suscripcion,mean)
tapply(Tvdiario,Suscripcion,mean)
tapply(Organizaciones,Suscripcion,mean)
tapply(Hijos,Suscripcion,mean)

#Si las diferencias de las medias son muy lejanas mi lambda de wilks va ser significativo,en otras 
#palabras entra en mi modelo la variable (se puede realizar la prueba de t,para corroborar lo dicho)

########################
#Selección de variables#
########################

########### 
#criterio1#
###########

# Criterio de lambda de Wilks para selección de variables
library(klaR)
greedy.wilks(Suscripcion ~ .,data=datosd)
#Suscripcion ~ Edad + Educacion + Tvdiario     #QUEDATE CON 3 variables predictoras.


########### 
#criterio2#
###########

#seleccion de variables con la libreria boruta
library(Boruta)#esta basado en  radom forest ,varaibles sombras(variables fictisias)
set.seed(123)
boruta=Boruta(Suscripcion~.,data=datosd,doTrace=2)
boruta
#4 attributes confirmed important: Edad, Educacion, Hijos, Tvdiario;
#boruta me dice trabajar con 4 variables predictoras

#CONCLUSION: greedy.wilks :Suscripcion ~ Edad + Educacion + Tvdiario      (3 variables predictoras)
#            boruta       :Suscripcion ~ Edad, Educacion, Hijos, Tvdiario (4 variables predictoras)

#grafico de boruta (4 variables predictoras)
plot(boruta,cex.axis=0.5)




#División de la Muestra y Modelamiento






#--------------------------------------------
# Distribución de individuos en el target
prop.table(table(datosd$Suscripcion))       #lo q viene acontinuacion mantendran la misma proporcion que el target.
#       No        Si 
#0.2556561 0.7443439 
#--------------------------------------------


#----------------------------------------------------------------------------------------------------------------
#################################
#PARTICION-ENBTRENAMIENTO-TESTEO#
#################################
#----------------------------------------------------------------------------------------------------------------

################
#LIBRERIA_CARET#_PREPOCESAMIENTO_PODEROSO
################

# Selección de muestra de entrenamiento (train, 80%) y de 
#evalucion (test, 20%)
library(ggplot2)
library(caret)
########################################################################
#RNGkind(sample.kind = "Rounding")#ALGORITMO DE SELECCION datos ANTIGUO#
########################################################################

########################
#FX createDataPartition#

#divide mis datos en 80%  y 20% proporcional al target
set.seed(123) 
index <- createDataPartition(datosd$Suscripcion, 
                             p=0.8, 
                             list=FALSE)
head(index)#obteniendo los primeras filas,representa el indice
#        Resample1
#[1,]         3
#[2,]         4
#[3,]         6
#[4,]         7
#[5,]         8
#[6,]         9
tail(index)#obteniendo los ultimas filas,representa el indice
#         Resample1
#[350,]       437
#[351,]       438
#[352,]       439
#[353,]       440
#[354,]       441
#[355,]       442
training <- datosd[ index, ]  # 355 datos,
testing  <- datosd[-index, ]  # 87 datos

# Verificando la estructura de los datos particionados
prop.table(table(datosd$Suscripcion))
#No        Si 
#0.2556561 0.7443439 
prop.table(table(training$Suscripcion)) #en la data de entrenamiento
#      No       Si 
#0.256338 0.743662
prop.table(table(testing$Suscripcion))
#No        Si 
#0.2528736 0.7471264 


#VEMOS QUE NUESTRA DATA DE TESTEO Y DATA DE ENTRENAMIENTO ES REPRESENTATIVA CON RESPECTO AL TARGET
#GRACIAS Fx "createDataPartition"(REPRESENTATIVA EN EL TARGET)

#OJO# ----puede OCURRIR
#CON RESPECTO A LA VARIABLE PREDICTORA(CON TODOS MIS DATOS),
#digamos variable predictora "genero" 70% mujeres y 30% hombres
#y en el training 50% mujeres 50 %hombres    testing  30% mujeres y 70 %hombres.
 


#------------------------------------------------------------------------------------------------------
#Estimación de la Función Discriminante Lineal
#------------------------------------------------------------------------------------------------------

#########################
#pruebalo en el training#
#########################

#considerando la seleccion de variables  por el criterio "greedy.wilks"(hacia adelante)
library(MASS)
modelo.training <- lda(Suscripcion ~ Educacion + Edad + Tvdiario,
                       training,
                       prior=c(1,1)/2)#la probalidad de cometer error tipo 1 ,error tipo 2  es lo mismo.
                                      #recordar el ejemplo de embarazo.
str(modelo.training)

modelo.training
#Coefficients of linear discriminants:
#            LD1
#Educacion 0.39207895
#Edad      0.06560276
#Tvdiario  0.35678042

#----------------------------44:45.

D <- 0.3921*training$Educacion + 0.0656*training$Edad + 0.3568*training$Tvdiario #355 resultados,razon porq son datos de entrenamiento
head(D)
mean(D)#media de los 355 datos de entrenamiento  #8.766376
tapply(D,training$Suscripcion,mean)
#   No     Si 
# 7.57   9.18    #me doy cuenta que los que si suscribieron son los que tienen mayores coeficientes(9.18)
# ">"


C <- (7.57 + 9.18)/2  ; C
#8.375

#puedo deducir que la REGLA DEBE SER   > 8.375 ,1(si), 0(no)


# Regla de Decisión:
# 0.3921*Educacion + 0.0656*Edad + 0.3568*Tvdiario > 8.38 ,"Si"
# 0.3921*Educacion + 0.0656Edad + 0.3568Tvdiario - 8.38 > 0, "Si"

CLASE <- ifelse(D>C,1,0)
CLASE <- factor(CLASE,levels=c(0,1),
                labels=c("No","Si"))


# Ejemplo:
# Educacion= 11, Edad=20, TVdiario= 1
#0.3921*11 + 0.0656*20 + 0.3568*1 - 8.38 
#-2.3981   esta persona no se va a suscribir -2.3981< C(8.375)

###############
#FORMA #RAPIDA#
###############

# valor de la función discriminante menos el punto de corte
modelo.values <- predict(modelo.training) 
modelo.values$x
modelo.values$posterior
modelo.values$class

#############################################
#modelo.values$class   es equivalente  CLASE#
#############################################

dim(modelo.values$x)
#[1] 355   1
#------------------------------------------------------------------------------------------------------
######
#NOTA###LO QUE VIENE SE  EVALUA EN EL TESTING  #INICIO
######
#------------------------------------------------------------------------------------------------------
# Negativo -> No, Positivo -> Si #referencia es el "cero" 
ldahist(data = modelo.values$x, g=training$Suscripcion)

modelo.values$posterior   # predicción de la probabilidad
modelo.values$class       # predicción de la clase

table(modelo.values$class )
# No  Si 
#130 225 


#------------------------------------------------------------------------------------------------------
#FIN
#------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------
#                                    INICIO#RELLENO
#------------------------------------------------------------------------------------------------------


#INSUMO : LE DOY MI  MODELO DE ENTRENAMIENTO "modelo.training"

#Clasificación y probabilidades de las observaciones
#Probabilidad predicha

#testing
#    Educacion Edad Tvdiario Organizaciones Hijos Suscripcion
#       12      18        3        0          0          No

(modelo.training)

proba.pred  <- predict(modelo.training,testing[,-6])$posterior

# estoy quitando la columna suscripcion
head(proba.pred,10)
#    No         Si
#1  0.9071412 0.09285879
#2  0.9397477 0.06025226
#5  0.8168487 0.18315134
#14 0.8706004 0.12939959
#15 0.9266447 0.07335531
#23 0.6187388 0.38126119
#27 0.6320252 0.36797478
#31 0.9698794 0.03012062
#32 0.9477822 0.05221784
#40 0.4656158 0.53438422

ldahist(data = modelo.values$x[,1], g=training$Suscripcion)
proba.pred[,2]
#            No         Si
#1   0.90714121 0.09285879
#2   0.93974774 0.06025226
#5   0.81684866 0.18315134
#.
#.
#.

testing$proba.pred <- proba.pred[,2]#me importa "si"
head(testing$proba.pred,10)
#0.09285879 0.06025226 0.18315134 0.12939959 0.07335531 0.38126119 0.36797478 0.03012062
#[9] 0.05221784 0.53438422


#Clase predicha (punto de corte, umbral es 0.5)

#entrenamiento 355
#testeo        87

# se aumenta una columna
testing$clase.pred <- predict(modelo.training,testing[,-6])$class
head(testing$clase.pred,10)
#No No No No No No No No No Si


# Almacenamiento de datos con clase y probabilidad predecida
# write.csv(testing,"testing-suscripcion-adl-scores.csv")

#grafica de barras
ggplot(testing) + aes(x = proba.pred, 
                      color=Suscripcion, 
                      fill=Suscripcion) +
  geom_histogram(alpha = 0.25)

#grafica de densidad
ggplot(testing) + aes(x = proba.pred, 
                      color=Suscripcion, 
                      fill = Suscripcion) + 
  geom_density(alpha = 0.25)

#------------------------------------------------------------------------------------------------------
#                                   FIN #RELLENO
#------------------------------------------------------------------------------------------------------
#Modelamiento con el paquete caret

# Relación de modelos en caret
library(caret) #Classification and Regression Training
names(getModelInfo())#TIENES PARA HACER 238(ALGORITMOS) TEMAS DE TESIS.(COMPARAR CON LOS ALGORITMOS ESTUDIADOS)

#TUNEAR  : ES PROBAR VALORES (EL VALOR OPTIMO)

# Relación de hiperparámetros a ajustar de un modelo
modelLookup(model="rpart")

modelLookup(model="xgbTree")
modelLookup(model='lda')

#------------------------------------------------------------------------------------------------------
#                                   EMPEZEMOS
#------------------------------------------------------------------------------------------------------
#############################################
# Aplicando el modelo con VALIDACION CRUZADA#(EN LOS DATOS DE ENTRENAMIENTO)
#############################################
RNGkind(sample.kind = "Rounding") 
set.seed(123)
ctrl <- trainControl(method="cv",number=10)
library(caret)
# Contruye 11 modelos: 10 para validación cruzada y un modelo para todos los datos
modelo_lda <- train(Suscripcion ~ Educacion + Edad + Tvdiario,#YA PASO POR FILTRO SELECCION DE VARIABLES(GRID WILKS ,BORUTA)
                    data = training, #DATA ENTRENAMIENTO
                    method = "lda",#ALGORTIMO ANALISIS DISCRIMINTE LIENAL
                    trControl = ctrl,#METODO VALIDACION CRUZADA CON 10 CAPAS
                    tuneLength = 5,#AL AZAR 5 VALORES (O HIPERPARAMETROS)
                    metric="Accuracy")

modelo_lda  
#Linear Discriminant Analysis 


#355 samples
#3 predictor
#2 classes: 'No', 'Si' 

#No pre-processing
#Resampling: Cross-Validated (10 fold) 
#Summary of sample sizes: 320, 319, 320, 320, 319, 320, ... 
#Resampling results:
  
#  Accuracy   Kappa    
#0.8427263  0.5690254

# Accuracy promedio(10 capas)=0.8427263




####################
#validacion cruzada#
####################

modelo_lda$resample#10 accuracy# para cada  capa(10)
#    Accuracy     Kappa Resample
#1  0.9142857 0.7671840   Fold01
#2  0.9166667 0.7500000   Fold02
#3  0.8571429 0.5803357   Fold03
#4  0.8285714 0.5512821   Fold04
#5  0.8888889 0.6521739   Fold05
#6  0.8285714 0.5512821   Fold06
#7  0.6756757 0.1777778   Fold07
#8  0.8000000 0.4567627   Fold08
#9  0.8285714 0.5512821   Fold09
#10 0.8888889 0.6521739   Fold10



modelo_lda#1 accuracy : es el promedio de los  10 accuracy

#Resampling: Cross-Validated (10 fold) 
#Summary of sample sizes: 320, 319, 320, 320, 319, 320, ... 
#Resampling(remuestreo) results:
  
#  Accuracy   Kappa    
#0.8427263  0.5690254



modelo_lda$finalModel#esto para mis valores "predichos"
#
#LD1
#Educacion 0.39207895
#Edad      0.06560276
#Tvdiario  0.35678042



modelo.training
#Coefficients of linear discriminants:
#            LD1
#Educacion 0.39207895
#Edad      0.06560276
#Tvdiario  0.35678042




#D <- 0.3921*training$Educacion + 0.0656*training$Edad + 0.3568*training$Tvdiario #355 resultados,razon porq son datos de entrenamiento

#CONCLUSION : modelo_lda$finalModel Y modelo.training ES EL MISMO.


#
#


# Se probo con el 90% de la data de entrenamiento 
355*0.9
modelo_lda$finalModel
#              LD1
#Educacion 0.39207895
#Edad      0.06560276
#Tvdiario  0.35678042


# Importancia de las variables
varImp(modelo_lda)
#         Importance
#Edad          100.00      #esta variable es mas importante
#Educacion      38.59
#Tvdiario        0.00

modelo_lda$resample
#    Accuracy     Kappa Resample
#1  0.9142857 0.7671840   Fold01
#2  0.9166667 0.7500000   Fold02
#3  0.8571429 0.5803357   Fold03
#4  0.8285714 0.5512821   Fold04
#5  0.8888889 0.6521739   Fold05
#6  0.8285714 0.5512821   Fold06
#7  0.6756757 0.1777778   Fold07
#8  0.8000000 0.4567627   Fold08
#9  0.8285714 0.5512821   Fold09
#10 0.8888889 0.6521739   Fold10

plot(varImp(modelo_lda))  #¿que varaible es mas importante?#grafica#edad


# Gráfico de puntos para los valores de accuracy
dotplot(modelo_lda$resample$Accuracy)#DBERIA SALIR 10 PUNTOS PERO SOLO SE OBSERVA 7 PUNTOS  ES POR MOTIVO DE LA ESCALA que empieza 0.7 a 0.90

# Diagrama de cajas
bwplot(modelo_lda$resample$Accuracy)#hay un punto(1 accuracy atipico 0.6756757)

summary(modelo_lda$resample$Accuracy)
#Min.   1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.6757  0.8286  0.8429  0.8427  0.8889  0.9167 


# También se puede analizar el Kappa
summary(modelo_lda$resample$Kappa)

#10_ACCURACY 
ggplot() + aes(modelo_lda$resample$Accuracy)+ geom_density()+
  theme_bw() + xlim(0,1)
#elotucurtica

###################
###########testing#
###################

###################################################
#CLASIFICACION Y PROBALIDADES DE LAS OBSERVACIONES#
###################################################
names(datosd)

library(MASS)
modelo=lda(Suscripcion~Educacion+Edad+Tvdiario,training,prior=c(1,1)/2)#estas variables se selecionaron con el criterio grid.wilks(paso ahacia adelante)
str(modelo)
modelo
prob.pred<- predict(modelo,testing[,-6])$posterior#no me uses la 6ta columa xq es el target
head(prob.pred,10)
#      No         Si
#1  0.9071412 0.09285879
#2  0.9397477 0.06025226
#5  0.8168487 0.18315134
#14 0.8706004 0.12939959
#15 0.9266447 0.07335531
#23 0.6187388 0.38126119
#27 0.6320252 0.36797478
#31 0.9698794 0.03012062
#32 0.9477822 0.05221784
#40 0.4656158 0.53438422



testing$prob.pred=prob.pred[,2]#87 datos testeo #selecciona la columna "2" xq esla que estudiare
head(testing$proba.pred,10)
#[1] 0.09285879 0.06025226 0.18315134 0.12939959 0.07335531 0.38126119 0.36797478 0.03012062 0.05221784 0.53438422

###############################################
#clase predicha (punto de corte,umbral es 0.5)#
###############################################

#si la probalidad es mayor a 0.5 lo pone como exito ("si")

testing$clase.pred=predict(modelo,testing[,-6])$class
head(testing$clase.pred,10)
#[1] No No No No No No No No No Si


getwd()
#alamacenamiento de datos con clase y probalidad predecida
write.csv(testing,"testing=suscripcion-adl-scores.csv")

#grafico de histograma
ggplot(testing) + aes(x=proba.pred,
                      color=Suscripcion,
                      fill=Suscripcion)+
  geom_histogram(alpha=0.25)+
  geom_vline(xintercept = 0.5,linetype="longdash")
#grafico de densidad
ggplot(testing) + aes(x=proba.pred,
                      color=Suscripcion,
                      fill=Suscripcion)+
  geom_density(alpha=0.25)+
  geom_vline(xintercept = 0.5,linetype="longdash")


#------------------------------------------------------------------------------------------------------------
 
########################################
#Indicadores para Evaluación de Modelos#de la matriz de confusion salen los indicadores
########################################

#trabajando con la data de testing #87 DATOS
#INSUMO:
#objetivo es el "ACCURACY"


#------------------------------------------------------------------------------------------------------------
#1. Tabla de Clasificación / Matriz de Confusión

library(gmodels)
CrossTable(x = testing$Suscripcion, 
           y = testing$clase.pred,
           prop.t=FALSE, 
           prop.c=FALSE, 
           prop.r=FALSE,
           prop.chisq = FALSE)

# Otra Forma
addmargins(table(Clase_Real=testing$Suscripcion,
                 Clase_Predicha=testing$clase.pred))
#           Clase_Predicha
#Clase_Real   No   Si  Sum
#No          19    3    22
#Si          17   48    65
#Sum         36   51    87

prop.table(table(Clase_Real=testing$Suscripcion,
                 Clase_Predicha=testing$clase.pred),1)
#               Clase_Predicha
#Clase_Real        No        Si
#No           0.8636364 0.1363636
#Si           0.2615385 0.7384615
prop.table(table(Clase_Real=testing$Suscripcion,
                 Clase_Predicha=testing$clase.pred),2)

#                                     Clase_Predicha
#Clase_Real         No                                          Si
#No           0.52777778(VALOR PREDICTIVO -)               0.05882353
#Si           0.47222222                                   0.94117647(VALOR PREDICTIVO +) 

vcd::mosaic(testing$clase.pred~testing$Suscripcion)

#############
#MANUALMENTE#CALCULAR EL ACCURACY
#############

# Calcular el accuracy#¿donde coincide la clase real con la clase predicha?
accuracy <- mean(testing$Suscripcion==testing$clase.pred)
accuracy
#0.7701149


# Calcular el error de mala clasificación (Tasa de error)
error <- mean(testing$Suscripcion!=testing$clase.pred)
error
#0.2298851
#----------------------------------------------------------------------------------------------------------------------------
#EJEMPLO X
#Clases desbalanceadas     
#98% ACTUAL   2% FUGA   "ACCURACY BASE"  es  98%
#en este caso ya no sirve fijarse del accuracy
#
#ES DECIR OBSERVAREMOS OTROS INDICADORES
#
# ACTUAL --> especificidad-->90%
# FUGA   --> sensibilidad -->85%
#
#
#Clases balanceadas    
#
#
#El accuracy solo es bueno cuando mi data es balanceada
#
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------

#########
#FORMA_1#
#########

# Usando el paquete caret
library(caret)
cm <- caret::confusionMatrix(testing$clase.pred,#clase predicha
                             testing$Suscripcion,#clase real
                             positive="Si")
cm
#                 Reference
#Prediction     No       Si
#No            19        17
#Si             3        48

#Accuracy    : 0.7701                         #parece que no hemos mejorado casi nada ya que nuestro "accuracy base" fue 0.7443439
#Sensitivity : 0.7385  
#Pos Pred Value : 0.9412 (valor predictivo positivo)
#Neg Pred Value : 0.5278 (valor predictivo negativo)
#Specificity : 0.8636 


cm$table
#          Reference
#Prediction No   Si
#No        19    17
#Si         3    48


###########################################################
#en mi datos origianles  #datosd #antes de particionar
table(datosd$Suscripcion)
# No  Si 
#113 329 
prop.table(table(datosd$Suscripcion))
#       No        Si 
#0.2556561    0.7443439(ACCURACY BASE,el objetivo con los modelos es superar este "accuracy base")                                   
############################################################
#tengo que hallar un modelo que supere el ACCURACY 0.7443439 como regla basica




cm$byClass["Sensitivity"] 
#Sensitivity 
#0.7384615 
cm$byClass["Specificity"] 
#Specificity 
#0.8636364 
cm$overall["Accuracy"]
# Accuracy 
#0.7701149 
precision <- cm$byClass['Pos Pred Value']  ; precision #VALOR PREDICTIVO POSITIVO O PRECISION
#Pos Pred Value 
#0.9411765


# Sensibilidad
recall <- cm$byClass['Sensitivity']  ; recall
#Sensitivity 
#0.7384615


# F1 score             = PRECISION + SENSIBILIDAD
# De Forma Manual
f_measure <- 2*((precision*recall)/(precision+recall));f_measure
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------

#########
#FORMA_2#
#########

library(MLmetrics)#MAS COMPLETO QUE EL ANTERIOR      "F1 SCORE"
Precision(testing$Suscripcion,testing$clase.pred,positive="Si")
#[1] 0.9411765
Recall(testing$Suscripcion,testing$clase.pred,positive="Si")
#[1] 0.7384615
F1_Score(testing$Suscripcion,testing$clase.pred,positive="Si")
#[1] 0.8275862

#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
#2. Estadístico de Kappa

# Tabla de Clasificación
addmargins(table(Clase_Real=testing$Suscripcion,
                 Clase_Predicha=testing$clase.pred))


# Accuracy o Probabilidad observada
pr_o <- (19+48)/87 ; pr_o


# Probabilidad esperada
pr_e <- (22/87)*(36/87) + (65/87)*(51/87) ; pr_e

k <- (pr_o - pr_e)/(1 - pr_e) ; k

# Estadístico de Kappa
k <- cm$overall['Kappa'] ; k



#3. Estadístico Kolmogorov - Smirnov

library(InformationValue)

ks_stat(testing$Suscripcion,testing$proba.pred, returnKSTable = T)


ks_stat(testing$Suscripcion,testing$proba.pred)
# Graficando el estadístico K-S 
ks_plot(testing$Suscripcion,testing$proba.pred)


library(ROCit)
ROCit_obj <- rocit(score=testing$proba.pred,
                   class=testing$Suscripcion)
ksplot(ROCit_obj,legend=T,values=T)

ksplot1 <- ksplot(ROCit_obj,legend=T,values=T)

ksplot1$`KS Cutoff`  # punto de corte óptimo con el mayor K-S
ksplot1$`KS stat`    # K-S al punto de corte óptimo

#--------------------------------------------------------------------------------------------------------------

###################################
#4. Curva ROC y Área bajo la Curva#EL BOJETIVO ES ENCONTRAR PUNTO DE CORTE
###################################

#--------------------------------------------------------------------------------------------------------------

#########
#forma_1#
#########

# 1. Usando el paquete caTools
library(caTools)
AUC <- colAUC(testing$proba.pred,#SIEMPRE EN ESTE ORDEN  1)PROBALIDAD
              testing$Suscripcion,#CLASE REAL
              plotROC = TRUE)
abline(0, 1,col="red",lty=1)

#graphics.off()
AUC 
#               [,1]
#No vs. Si   0.9160839


#########
#forma_2#USANDO PAQUETE pROC
#########

# 2. Usando el paquete pROC
library(pROC)

# Área bajo la curva
roc <- roc(testing$Suscripcion,testing$proba.pred)
roc
#Area under the curve: 0.9161



roc$thresholds # puntos de corte que ha probado el modelo
#-Inf 0.02934815 0.03754238 0.04859099 0.05623505 0.06680378 0.08310705 0.09587810 0.11172383 0.12697492 0.13858613 0.16205501
#[13] 0.17974434 0.18412521 0.19331389 0.20938170 0.21812969 0.25312337 0.28906205 0.30061728 0.32189997 0.33429208 0.35154578 0.37461799
#[25] 0.38148034 0.38177183 0.38242765 0.39278195 0.40970928 0.43206569 0.45391230 0.46631018 0.47852093 0.50292277 0.52178970 0.52340714
#[37] 0.52855094 0.53369311 0.54903423 0.57063182 0.58252411 0.58844052 0.59498110 0.63127346 0.66234175 0.66378928 0.67396101 0.68409784
#[49] 0.69553870 0.70667991 0.71405657 0.72374252 0.72779884 0.73359519 0.74306043 0.74840597 0.75508908 0.76385084 0.76756348 0.76888426
#[61] 0.77315884 0.78209160 0.79904893 0.82315215 0.83847047 0.84463261 0.85140830 0.85888724 0.86439120 0.86996032 0.87656182 0.88421835
#[73] 0.88946870 0.89504257 0.91074907 0.92281232 0.92844075 0.93811816 0.95007372 0.96039396 0.96700352 0.97653153 0.98502161        Inf





# Sensitividad para cada punto de corte
roc$sensitivities 
#[1] 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 0.98461538 0.98461538 0.98461538
#[13] 0.98461538 0.96923077 0.96923077 0.96923077 0.96923077 0.96923077 0.96923077 0.95384615 0.93846154 0.90769231 0.89230769 0.89230769
#[25] 0.87692308 0.86153846 0.84615385 0.84615385 0.83076923 0.81538462 0.80000000 0.78461538 0.75384615 0.73846154 0.72307692 0.70769231
#[37] 0.69230769 0.67692308 0.66153846 0.63076923 0.61538462 0.61538462 0.60000000 0.60000000 0.58461538 0.56923077 0.55384615 0.53846154
#[49] 0.52307692 0.50769231 0.49230769 0.47692308 0.46153846 0.44615385 0.43076923 0.43076923 0.41538462 0.40000000 0.38461538 0.36923077
#[61] 0.35384615 0.33846154 0.32307692 0.30769231 0.29230769 0.27692308 0.26153846 0.24615385 0.23076923 0.21538462 0.20000000 0.18461538
#[73] 0.16923077 0.15384615 0.13846154 0.12307692 0.10769231 0.09230769 0.07692308 0.06153846 0.04615385 0.03076923 0.01538462 0.00000000




# Especifidad para cada punto de corte
roc$specificities
# [1] 0.00000000 0.04545455 0.09090909 0.13636364 0.18181818 0.22727273 0.27272727 0.31818182 0.36363636 0.36363636 0.40909091 0.45454545
#[13] 0.50000000 0.50000000 0.54545455 0.59090909 0.63636364 0.72727273 0.77272727 0.77272727 0.77272727 0.77272727 0.77272727 0.81818182
#[25] 0.81818182 0.81818182 0.81818182 0.86363636 0.86363636 0.86363636 0.86363636 0.86363636 0.86363636 0.86363636 0.86363636 0.86363636
#[37] 0.86363636 0.86363636 0.86363636 0.86363636 0.86363636 0.90909091 0.90909091 0.95454545 0.95454545 0.95454545 0.95454545 0.95454545
#[49] 0.95454545 0.95454545 0.95454545 0.95454545 0.95454545 0.95454545 0.95454545 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000
#[61] 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000
#[73] 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000


#no se puede agarrar los extremos ya que si escogo el 1 de sensiblidad no hay nada en especificidad,por otro lado
#no se puede agarrar 0 de sensibilidad porque hay 1 de especificidad,el objetivo es tener tanto alto en especificidad y alto en 
#sensiblidad

#¿COMO LOGRARLO?

#################################################################################
#################################################################################

              #INICIO RELLENO#
# 2da Forma
areaROC <- auc(roc(testing$Suscripcion,testing$proba.pred)) 
areaROC
#Area under the curve: 0.9161


# 3era forma
roc$auc
#Area under the curve: 0.9161

              #FIN RELLENO#

#################################################################################
#################################################################################


#SEGUIMOS #Usando paquete pROC

#Area under the curve: 0.9161
plot.roc(testing$Suscripcion,testing$proba.pred, #CLASE REAL  #CON LA PROBALIDAD
         main = paste('Área bajo la curva =',round(areaROC,4)),
         xlab="1 - Especificidad", #POR DEFECTO ME SALE EN INGLES SINO LO PONGO 
         ylab="Sensibilidad", 
         legacy.axes=TRUE,
         col="blue",
         max.auc.polygon=TRUE,
         auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.auc=TRUE,#MUESTRAME EL AREA BAJO LA CURVA
         print.thres=TRUE)#MUESTRAME EL PUNTO DE CORTE OPTIMO¿CON QUE CRITERIO?#0.289 (PUNTO DE CORTE OPTIMO)/SENSEIBLIDAD#0.773/ESPECIFICIDAD#0.969




puntos.corte <- data.frame(prob=roc$thresholds,#punto de corte que ha probado el modelo
                           sen=roc$sensitivities,#Sensitividad para cada punto de corte
                           esp=roc$specificities,#Especificidad para cada punto de corte
                           s_e=roc$sensitivities+roc$specificities)#RPTA A LA PREGUNTA ¿COMO LOGRARLO?
                           #rpta:OBTENIENDO LA MAYOR SUMA ENTRE ESPECIFICIDAD Y SENSIBILIDAD
head(puntos.corte)
#  prob sen        esp      s_e
#1       -Inf   1 0.00000000 1.000000
#2 0.02934815   1 0.04545455 1.045455
#3 0.03754238   1 0.09090909 1.090909
#4 0.04859099   1 0.13636364 1.136364
#5 0.05623505   1 0.18181818 1.181818
#6 0.06680378   1 0.22727273 1.227273




# Punto de corte óptimo (mayor sensibilidad y especificidad)
coords(roc, "best",ret=c("threshold","specificity", "sensitivity","accuracy"))
#"best" se refiere roc$sensitivities+roc$specificities
# LA MAYOR SUMA ENTRE ESPECIFICIDAD Y SENSIBILIDAD

#           threshold specificity sensitivity  accuracy
#threshold  0.289062   0.7727273   0.9692308 0.9195402



coords(roc, "best")
#  threshold specificity sensitivity
#1  0.289062   0.7727273   0.9692308

#grafica mostrando el punto de corte optimo con su respectiva sensibilidad y especificidad(p.corte:0.289062,sensibilidad:0.7727273,especificidad:0.9692308)
plot(roc,print.thres=T)  # punto de corte, especificidad, sensibilidad






# Graficando la Sensibilidad y Especificidad

ggplot(puntos.corte, aes(x=prob)) + 
  geom_line(aes(y=sen, color="Sensibilidad")) +
  geom_line(aes(y=esp, color="Especificidad")) + 
  labs(title ="Sensibilidad vs Especificidad", 
       x="Probabilidad") +
  scale_color_discrete(name="Indicador") +
  geom_vline(aes(xintercept=0.2891), # punto de corte obtenido en coords
             color="black", linetype="dashed", size=0.5) + 
  theme_bw() + theme(line = element_blank())

#izquierda a derecha
#se puede observar que a manera que aumenta la especificidad disminuye la sensibilida.

#derecha a izquierda
#se puede observar que a manera que aumenta la sensiblidad disminuye la espercificidad.

#entonces  la solucion no son los extremos, buscamos un  punto de corte intermedio,que el algoritmo
#en este caso se baso en el criterio de la myor suma entre sensibilidad y especificidad.




 

#############################################
#IDENTIFICAME EL PUNTO DE CORTE OPTIMO(FILA)#
#############################################


# Gráficando la Sensibilidad más la Especificidad
puntos.corte$suma <- puntos.corte$esp+puntos.corte$sen
optimo <- which.max(puntos.corte$suma)
optimo 
#[1] 19


# indica en orden en que se encuentra el punto optimo
puntos.corte[optimo,]
#       prob       sen       esp      s_e     suma
#19  0.289062 0.9692308 0.7727273 1.741958 1.741958  (EN LA FILA 19 ESTA EL MEJOR PUNTO DE CORTE)


                              #############
                              #COMPARACION#
##################################################################################################
#COMPARACION          #PUNTO DE CORTE 0.5        #PUNTO DE CORTE 0.289
##################################################################################################

cm$byClass["Sensitivity"] 
#Sensitivity 
#0.7384615 
cm$byClass["Specificity"] 
#Specificity 
#0.8636364 
cm$overall["Accuracy"]
# Accuracy 
#0.7701149 


#FIJARME EN LA DATA "testing"

#se puede observar
#library(rio)
#export(testing,"testing.txt")
#getwd()
#Educacion	Edad	Tvdiario	Organizaciones	Hijos	Suscripcion	  proba.pred      clase.pred	 
#12	        18	    3	          0          	0	      No	    0.0928587869290025	    No	      
#12	        19	    2	          0	        0       	No	   	0.060252261680849	    	No	      
#12       	20	    4	          0	        1       	Si	   	0.183151339543247	      No	      
#13	        21	    2	          1	        1	       	No	   	0.129399592625024	    	No	     
#12       	21     	2	          6	        0	       	No	   	0.0733553069516133	    No	      
#16	        22	    1	          6	        0	       	Si	   	0.38126118998227		    No	   cambiara a "SI" (PUNTO DE CORTE 0.289)   
#15	        22    	2	          0	        0	       	No	   	0.367974783916528	    	No	   cambiara a "SI"        
#12       	23    	0	          0	        1	       	No	   	0.0301206219050944	    No	      
#12       	23	    1	          4	        1	       	No	   	0.0522178394128248	    No	      
#15	        23	    3	          3        	0	       	Si	   	0.534384216014738		    Si	      
#.
#.
#.

#el nuevo punto de corte es 0.289 y ya no es 0.5 la nueva  "clase predicha"  (clase.pred)

#
##################################################################################################

testing$clase.pred2 <- ifelse(testing$proba.pred>0.289,"Si","No")
testing$clase.pred2 <- as.factor(testing$clase.pred2)
#[1] No No No No No Si Si No No Si No Si No Si ...
str(testing$clase.pred2)
#Factor w/ 2 levels "No","Si": 1 1 1 1 1 2 2 1 1 2 ...

# Usando el paquete caret
library(caret)
cm2 <- caret::confusionMatrix(testing$clase.pred2,#clase predicha
                             testing$Suscripcion,#clase real
                             positive="Si")
cm2

#            Reference
#Prediction  No   Si
#No          17   2
#Si          5    63
#################################0.289

#Sensitivity  
#0.9692       

#Specificity 
#0.7727

#Accuracy 
#0.9195


#################################0.5
cm
#Sensitivity 
#0.7384615 

#Specificity 
#0.8636364 

# Accuracy 
#0.7701149 


##################################################
##################################################FIN !!!!!!!!!!!!!!!!!!!!!!!!!!
##################################################


#5. Coeficiente de Gini
# Calculando manualmente
library(caTools)
AUC <- colAUC(testing$proba.pred,
              testing$Suscripcio,
              plotROC=T)
abline(0,1,col="red",lty=2)

#6. Log Loss

# Usando el paquete MLmetrics
library(MLmetrics)
# Transformar la variable CHURN a numérica
real <- as.numeric(testing$Suscripcion)
head(real)

# Recodificar los 1 y 2 como 0 y 1 respectivamente
real <- ifelse(real==2,1,0)
head(real)

LogLoss(testing$proba.pred,real)

#Predicción para Nuevos Individuos
#Para datos de manera individual

#Educacion Edad Tvdiario 12 18 3
nuevo1 <- data.frame(Educacion=12,Edad=18,Tvdiario=3)
predict(modelo.training,nuevo1)$posterior


predict(modelo.training,nuevo1)$class
nuevo2<-data.frame(Educacion=16,Edad=22,Tvdiario=1)
predict(modelo.training,nuevo2)$posterior

predict(modelo.training,nuevo2)$class

#Para un conjunto de datos


library(foreign)
datosn <- read.spss("suscripcion-nuevos-discriminante.sav",
                    use.value.labels=TRUE, 
                    max.value.labels=TRUE,
                    to.data.frame=TRUE)
attr(datosn,"variable.labels") <- NULL
str(datosn)

clase    <- predict(modelo.training,datosn)$class
proba    <- predict(modelo.training,datosn)$posterior
datonsdp <- cbind(datosn,clase,proba)


























