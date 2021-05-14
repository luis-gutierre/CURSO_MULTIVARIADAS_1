data.estudiante <- read.delim("data.estudiante.txt")
datos=data.estudiante

datos$internet  = as.factor(datos$internet)#pasar a cualitativa 
datos$sex       = as.factor(datos$sex)
datos$address   = as.factor(datos$address)
datos$activities = as.factor(datos$activities)
datos$romantic = as.factor(datos$romantic)
datos$rendimiento = as.factor(datos$rendimiento)
#
datos$age=as.numeric(datos$age)
datos$studytime=as.numeric(datos$studytime)
datos$freetime=as.numeric(datos$freetime)
datos$absences=as.numeric(datos$absences)
#
str(datos)
#
attach(datos) #para reconocimientos de los nombres de las 
#

#MODELO LOGISTICO1
# PRIMERA PROPUESTA
modelo_logistico1 = glm(rendimiento~ ., family = "binomial",data=datos)

summary(modelo_logistico1)
#
#sexM ,activitiesyes
#
#MODELO LOGISTICO2
modelo_logistico2 = glm(rendimiento~age+address+studytime
                        +romantic+freetime+absences+internet , family = "binomial",data=datos)
summary(modelo_logistico2)
#
#romanticyes
#
#MODELO LOGISTICO3
modelo_logistico3 = glm(rendimiento~age+address+studytime
                        +freetime+absences+internet , family = "binomial",data=datos)
summary(modelo_logistico3)

#verosimilitudes
modelo_logistico0 = glm(rendimiento~1, family = "binomial",data=datos)
summary(modelo_logistico0 )

#
library(lmtest)
TestRV = lrtest(modelo_logistico0 ,modelo_logistico3)
TestRV
#Model 1: rendimiento ~ 1
#Model 2: rendimiento ~ age + address + studytime + freetime + absences + 
#  internet
#Df  LogLik Df Chisq Pr(>Chisq)    
#1   1 -398.38                        
#2   7 -358.16  6 80.45  2.884e-15 ***


#Se rechaza la H0,  a un nivel de de significancia de 0.05 se puede afimar
#que el modelo_logistico3 es mejor que el modelo_logistico0,por lo tanto 
# modelo_logistico3 se ajusta  a lkos datos.

# ----------- #
# Pseudo - R² #
# ----------- #
( TestRV  = lrtest ( modelo_logistico0, modelo_logistico3 ))
# LogLik del modelo 1 (modelo_nulo) = 
# LogLik del modelo 2 (modelo_logistico) = 
TestRV $ LogLik
#-398.3804 -358.1554

# Pseudo R2 = 1- logLik (modelo_logistico2) / logLik (modelo_nulo)
1 - TestRV $ LogLik [ 2 ] / TestRV $ LogLik [ 1 ] 
#0.1009713

###############
# Predicciones#
###############

predicciones = predict(modelo_logistico3, type=c("response"))
comparacion  = data.frame(OBS  = rendimiento,
                          PRED = as.factor(round(predicciones,0))) 

comparacion
#486   1    1
#487   0    1
#488   0    1
#489   0    1
#490   0    0
#491   0    1
#492   0    1
#493   0    1
#494   0    0
#495   0    1
#496   1    1
#497   1    1
#498   0    1
#499   1    1
#500   1    1
table(comparacion)
#      PRED
#OBS   0   1
#0    42 155
#1    34 418






#-------------------#
#matriz de confusion#
#-------------------#

#matriz de confusion
library(caret)
confusionMatrix(comparacion$PRED, comparacion$OBS, positive = "1")
#          Reference
#Prediction   0   1
#0           42  34
#1          155 418

#Sensitivity : 0.9248     #identifica los aprobados        
#Specificity : 0.2132     #identifica a los desaprobados

(42+418)/(42+418+34+155)
############
#curva roc #1 criterio de la suma  mayor de la sensibilidad + especificidad
############
library(pROC)
rocobj = roc( comparacion$OBS,predicciones, ci = TRUE)
plot(rocobj)
plot.roc(rocobj,
         legacy.axes = F, 
         print.thres = "best",
         print.auc   = TRUE,
         auc.polygon = F,
         max.auc.polygon = F, 
         col  = "blue", 
         grid = T )

############
#curva roc #2  criterio de la suma  mayor de la sensibilidad + especificidad
############
library(Epi)
ROC(form = rendimiento~age+address+studytime
    +freetime+absences+internet  ,  plot = "ROC")

# Para futuras predicciones, se puede utilizar el punto de corte sugerido por la curva ROC
data.frame(datos$rendimiento, predicciones, predicciones>=0.687)
table(datos$rendimiento, predicciones>=0.687)
#
#    FALSE TRUE
#0   134   63
#1   140  312
#
prop.table(table(datos$rendimiento))
(134+312)/(134+312+63+140)
