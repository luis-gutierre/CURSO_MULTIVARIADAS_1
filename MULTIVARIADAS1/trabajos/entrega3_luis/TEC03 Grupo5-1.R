######################################################
#                                                    #
#             TRABAJO EN CLASE N 03                   #ingresando 2 columnas ideales
#                                                    #
######################################################


# Cambiar el directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Limpiar workspace y gráficos
rm(list=ls())
graphics.off()
# Eliminar la notación científica. Establecer numero de digitos
options(scipen=999)      
options(digits = 7)
# Carga de paquetes
library(pacman)
p_load(MASS, ca, anacor,FactoMineR,vegan,
       gplots,vcd,graphics,factoextra,foreign)


# Tabla de contigencia
#---------------------
datos_s.acs <- matrix(c(16,17,18,19,16,45,15,19,18,45,31,
                        8,15,18,17,27,20, 2,14,53,53,40,
                        20,20,23,21,29,20,18,19,25,29,29,
                        11,13,12,17,20,16,15,10,44,44,20,
                        28,25,25,22,30,26,24,22,26,30,30,
                        21,21,20,24,27,22,18,21,24,27,27,
                        21,21,21,23,26,15,16,18,21,26,26),
                      nrow=7,byrow=T)

# Asignación de nombres a las filas y columnas de la tabla
dimnames(datos_s.acs)<-list(Atributos=c("Precios", "Variedad", "Rapidez", 
                                        "Información","Trato","Condiciones","Acceso")
                            ,Empresa=c("Empresa 1","Empresa 2","Empresa 3",
                                       "Empresa 4","Empresa 5","Empresa 6",
                                       "Empresa 7","Empresa 8","Empresa 9",
                                       "Ideal","E5S"))#columnas ideales 'Ideal' y 'E5S'

datos_s.acs


# Prueba de Independencia Chi-Cuadrado  
chisq.test(datos_s.acs[,-c(10:11)]) # Se trabaja con todas las columnas menos la suplementaria
# Se Rechaza Ho

# ACS con el paquete FactoMiner   
#------------------------------
library(FactoMineR) 
res.ca.s <- CA(datos_s.acs,
               ncp=2,
               graph=F,
               col.sup = c(10,11))

# Scree Plot de los Autovalores
#------------------------------
get_eigenvalue(res.ca.s) # Con 2 dimensiones Se explica el 91% de la variabilidad

# Interpretación de los Indicadores del ACS
#------------------------------------------
summary(res.ca.s,nb.dec = 3, ncp = 2) 


# En el analisis de ACS
#----------------------
#                Iner*1000    Dim.1    ctr   cos2    Dim.2    ctr   cos2 
# Empresa 5   |     3.089 | -0.067  1.155  0.195 |  0.119  8.170  0.611

#   El 19.5 % de la inercia de la empresa 5 es explicada por la dimension 1
#   El 61.1 % de la inercia de la empresa 5 es explicada por la dimension 2

#   El 80,6% de la inercia de la empresa 5 es explicada por las dimensiones


# Con la variable sumplementaria  E5simulada
#-------------------------------------------
#                 Dim.1   cos2    Dim.2   cos2  
#  E5S         | -0.121  0.334 | -0.095  0.205 |

#   El 33.4 % de la inercia de la empresa 5 es explicada por la dimension 1
#   El 20.5 % de la inercia de la empresa 5 es explicada por la dimension 2

#   El 53.9 % de la inercia de la empresa 5 es explicada por las dimensiones


# Biplot filas, columnas y columna suplementaria
#-----------------------------------------------
fviz_ca_biplot(res.ca.s, repel = T) + theme_light()


# Conclusiones
#-------------
# Al aumentar el valor en los precios y en la variedad graficamente la empres 5 se posiciona
# mas cerca de la empresa ideal, pero la inercia explicada por los dos componentes dismunyo en 
# 26.7% y ahora se encuentra mas cerca del origen

# Se propone cambiar los valores de otros atributos.
(sum(res.ca.s$row$inertia)-(0.007329734))*100
                 
 