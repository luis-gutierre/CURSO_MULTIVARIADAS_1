#####################################
#     CUARTA PRÁCTICA CALIFICADA    # 
#   ANÁLISIS DISCRIMINANTE LINEAL   #
#     Mg. Jesús Salinas Flores      # 
#     jsalinas@lamolina.edu.pe      #
#####################################

# Para limpiar el workspace, por si hubiera algún dataset 
# o información cargada
rm(list = ls())
graphics.off()

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Otras opciones
options(scipen=999)      # Eliminar la notación científica
options(digits = 3)      # Número de decimales

# Paquetes
library(pacman)
p_load(psych, MASS, klaR, gains, caret, Boruta, gmodels,vegan,
       MLmetrics, vcd, Epi, InformationValue, ROCit)

########################
# Descripción del caso #
########################

# Se utilizará el Wisconsin Breast Cancer Diagnostic 
# dataset de la UCI Machine Learning Repository en 
# http://archive.ics.uci.edu/ml. 
# Los datos fueron donados por investigadores de la 
# Universidad deWisconsin e incluye las mediciones a 
# partir de imágenes digitalizadas de una masa mamaria. 
# Los valores representan las características de los
# núcleos celulares presentes en la imagen digital.
#
# Los datos del cáncer de mama incluyen 569 ejemplos de
# biopsias de cáncer, cada uno con 32 características. 
# Una característica es un número de identificación, otro
# es el diagnóstico de cáncer y 30 son mediciones 
# numéricas de laboratorio. 
# El diagnóstico (TARGET) se codifica como:
# "M" para indicar maligno o 
# "B" para indicar benigno.
#
# Las otras 30 mediciones numéricas comprenden la media, 
# el error estándar y el peor (es decir, mayor) para 10 
# características diferentes de los núcleos de células 
# digitalizadas.
# Éstas incluyen:
# • Radio
# • Textura
# • Perímetro
# • Zona
# • Suavidad
# • Compacidad
# • Concavidad
# • Puntos cóncavos
# • Simetría
# • Dimensión fractal

# Basándose en estos nombres, todas las características 
# parecen estar relacionadas con la forma y el tamaño de
# los núcleos de la célula.
# A menos que sea un oncólogo, es poco probable que sepa 
# cómo se relaciona cada uno con benignas o malignas. 


# Lectura de datos
wbcd <- read.csv("wisc_bc_data.csv", 
                 stringsAsFactors = T)
datosd=data.frame(wbcd)
str(datosd)

# No considerar el campo id
datosd$id <- NULL

dim(datosd)
# [1] 569  31

levels(datosd$diagnosis) <- c("Benigno","Maligno")

contrasts(datosd$diagnosis)
#          Maligno
#Benigno       0
#Maligno       1

table(datosd$diagnosis)
#Benigno Maligno 
#357     212 


prop.table(table(datosd$diagnosis))
#Benigno   Maligno 
#0.6274165 0.3725835                 #se oberva que esta desbalanceada


# Para cada una de las preguntas comente los resultados 
# obtenidos. Use donde sea necesario set.seed(2021)

#--------------------------------------------------------------------------------------------------------
# 1. Realice el análisis exploratorio bivariado de las 
#    variables predictoras con la variable target.
#    Indique qué variables serían las más importantes. 
#    Justifique numéricamente y gráficamente su respuesta
#    (4 puntos)


#Explorando las variables predictoras
datosd$diagnosis <- factor(datosd$diagnosis)

set.seed(2021)
library(funModeling)
names(datosd)
predictores <- setdiff(names(datosd), "diagnosis")
predictores

target=datosd$diagnosis
# Gráfico de Barras Apilado en proporción por cada variable predictora vs target
cross_plot(datosd, 
           input=predictores, 
           target="diagnosis",
           plot_type = "percentual") #both  #quantity


#conclusion:
#de los 30 graficos  que son "diagnosis"(maligno  o benigno)   VS las 30 variables  (PAR EN PAR)
#explicaremos algunos de ellos:

#diagnosis vs points_se
#A manera que aumenta los "points_se" tambien aumenta la posibilidad de tener celulas "malignas"

#diagnosis vs concavity_se
#se observa que en el rango [0.000 ; 0.00774]  el indicador dice que no hay  presencia celulas malignas
#pero a manera que aumenta la "concativy_se" aumenta la probalidad de tener celular malignas.


#--------------------------------------------------------------------------------------------------------
# 2. Aplique el criterio de lambda de Wilks para la selección
#    de variables. (2 puntos) 


########### 
#criterio1#
###########
library(klaR)
greedy.wilks(diagnosis ~ .,data=datosd)

#Las variables que aportan mas al trabajo son 16 y  estas son;

#points_worst + radius_worst + texture_worst + area_worst + 
#smoothness_se + symmetry_worst + compactness_se + radius_se + 
#  dimension_worst + compactness_mean + points_mean + concavity_worst + 
#  concavity_se + area_se + points_se + concavity_mean



#--------------------------------------------------------------------------------------------------------
# 3. Particione la data 80% (train) y 20% (test) usando el 
#    paquete caret() (2 puntos)

set.seed(2021)
library(caret)
index <- createDataPartition(datosd$diagnosis, 
                             p=0.8, 
                             list=FALSE)

training <- datosd[ index, ]  #456 observaciones
testing  <- datosd[-index, ]  #113 observaciones

prop.table(table(datosd$diagnosis))
#Benigno Maligno 
#0.627   0.373 

prop.table(table(training$diagnosis))
#Benigno Maligno 
#0.627   0.373 

prop.table(table(testing$diagnosis))
#Benigno Maligno 
#0.628   0.372 

#interpretacion : 
#podemos observar que tanto para la data entrenamiento y la data de testeo
#mantiene la proporcion de la varaible "diagnosis"


#--------------------------------------------------------------------------------------------------------
# 4. Con las variables seleccionadas en (2), realice un análisis 
#    discriminante lineal en el train usando validación cruzada
#    con v=10. Presente cuál sería la regla de 
#    decisión para clasificar a una persona si el diagnóstico es
#    maligno o benigno. (4 puntos)

library(MASS)
modelo.training <- lda(diagnosis ~ points_worst + radius_worst + texture_worst + area_worst + 
                         smoothness_se + symmetry_worst + compactness_se + radius_se + 
                         dimension_worst + compactness_mean + points_mean + concavity_worst + 
                         concavity_se + area_se + points_se + concavity_mean,
                       training,
                       prior=c(1,1)/2)#la probalidad de cometer error tipo 1 ,error tipo 2  es lo mismo.
#recordar el ejemplo de embarazo.
str(modelo.training)

modelo.training
################### 
#REGLA DE DECISION#
################### 
D=2.96215*training$points_worst+ 0.56252*training$radius_worst+0.05125*training$texture_worst-0.00317*training$area_worst+87.38438*training$smoothness_se+ 4.07189*training$symmetry_worst-1.54246*training$compactness_se+2.49008*training$radius_se+17.58345*training$dimension_worst-15.88810*training$compactness_mean+14.12065*training$points_mean+2.30082*training$concavity_worst-17.73688*training$concavity_se-0.01003*training$area_se+44.08393*training$points_se+4.25017*training$concavity_mean
D
tapply(D,training$diagnosis,mean)
#Benigno Maligno 
#10.4    14.2
C <- (10.4   + 14.2)/2  ; C
# 12.3

#D=2.96215*points_worst+0.56252*radius_worst+0.05125*texture_worst +...+4.25017*concavity_mean 
# D>C , "maligno" , "benigno"








#--------------------------------------------------------------------------------------------------------
# 5. Comente sobre la estabilidad del modelo. Justifique su 
#    respuesta numérica y gráficamente (4 puntos)


set.seed(2021)
ctrl <- trainControl(method="cv",number=10)
library(caret)
# Contruye 11 modelos: 10 para validación cruzada y un modelo para todos los datos
modelo_lda <- train(diagnosis ~ points_worst + radius_worst + texture_worst + area_worst + 
                      smoothness_se + symmetry_worst + compactness_se + radius_se + 
                      dimension_worst + compactness_mean + points_mean + concavity_worst + 
                      concavity_se + area_se + points_se + concavity_mean,#YA PASO POR FILTRO SELECCION DE VARIABLES(GRID WILKS)
                    data = training, #DATA ENTRENAMIENTO
                    method = "lda",#ALGORTIMO ANALISIS DISCRIMINTE LIENAL
                    trControl = ctrl,#METODO VALIDACION CRUZADA CON 10 CAPAS
                    tuneLength = 5,#AL AZAR 5 VALORES (O HIPERPARAMETROS)
                    metric="Accuracy")

modelo_lda

modelo_lda$resample#10 accuracy# para cada  capa(10)
#Accuracy Kappa Resample
#1     0.933 0.853   Fold01
#2     0.956 0.903   Fold02
#3     0.978 0.953   Fold03
#4     0.978 0.952   Fold04
#5     1.000 1.000   Fold05
#6     0.957 0.904   Fold06
#7     0.913 0.809   Fold07
#8     0.978 0.953   Fold08
#9     0.889 0.749   Fold09
#10    1.000 1.000   Fold10

#INTERPRETACION:
#podemos observar  que los accuracy en de cada capa estan en un rango de [0.889-1] , es decir el rango es pequeño.




modelo_lda#1 accuracy : es el promedio de los  10 accuracy

#Accuracy  Kappa
#0.958     0.908
#INTERPRETACION:
#podemos observar que el accuracy promedio es 0.958  haciendo una comparacion con los accuracy de las 10 capas ,nuestro 
#acccuracy promedio pertenece a ese rango y lo mejor q esta muy cercano a los accurary de las 10 capas,podemos decir que nuestro modelo
#es estable.

# Diagrama de cajas 
bwplot(modelo_lda$resample$Accuracy)
#interpretacion:
#no hay ningun accuracy atipico todos estan en el rango intercuartil lo cual es bueno y confirmamos lo dicho anteriormente.

#--------------------------------------------------------------------------------------------------------

# 6. En la data de evaluación o test halle los siguientes
#    indicadores (4 puntos)
#    6.1.Accuracy
#    6.2 Sensibilidad
#    6.3 Especificidad
#    6.4 Curva ROC y área bajo la curva

#clases#
testing$clase.pred <- predict(modelo.training,testing[,-1])$class
#probalidades#
testing$proba.pred  <- predict(modelo.training,testing[,-1])$posterior  
testing$proba.pred <- proba.pred[,2]


#matrix de confusion#
library(caret)
cm <- caret::confusionMatrix(testing$clase.pred,#clase predicha
                             testing$diagnosis,#clase real
                             positive="Maligno")
cm
#             Reference
#Prediction Benigno Maligno
#Benigno      71       2
#Maligno       0      40

#Accuracy : 0.982          nuestro modelo  muy exacto , pues explica aun 0.982 
#Sensitivity : 0.952      identifica muy bien  a los pacientes que tienen celulas malignas           
#Specificity : 1.000      idetifica muy bien a los pacientes que tienen celulas benignas

#curva roc#
library(pROC)

# Área bajo la curva
roc <- roc(testing$diagnosis,testing$proba.pred)
roc
#1
areaROC <- auc(roc(testing$diagnosis,testing$proba.pred)) 
areaROC
#1
#el area es 1 

#Area under the curve: 1
plot.roc(testing$diagnosis,testing$proba.pred, #CLASE REAL  #CON LA PROBALIDAD
         main = paste('Área bajo la curva =',round(areaROC,4)),
         xlab="1 - Especificidad", 
         ylab="Sensibilidad", 
         legacy.axes=TRUE,
         col="blue",
         max.auc.polygon=TRUE,
         auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.auc=TRUE,#MUESTRAME EL AREA BAJO LA CURVA
         print.thres=TRUE)

# el area bajo la curva es 1 , en nuestra grafica nos dice que el mejor punto de corte es 0.280 donde podriamos
#explicar  al 100 porciento los paciente que tienen celulas malignas y los paciente que tienen celulas benignas.