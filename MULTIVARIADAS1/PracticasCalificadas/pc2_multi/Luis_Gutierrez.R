
#
# Tabla de contigencia
#---------------------
datos_s.acs <- matrix(c(56,13,4,1,56,4,11,
                        15,43,21,18,20,9,19,
                        13,15,24,22,6,20,42,
                        6,19,7,5,8,33,26,
                        33,4,50,45,4,1,0),
                      nrow=5,byrow=T)
#
# Asignación de nombres a las filas y columnas de la tabla
dimnames(datos_s.acs)<-list(area=c("recepcion", "medicina", "topico", 
                                        "odontologia","psicologia")
                            ,atributos=c("amabilidad","eficiencia","confiabilidad","confidenciabilidad",
                                       "rapidez","tecnologia","higine")) 

datos_s.acs
#chi-cuadrado
chisq.test(datos_s.acs)
#
library(FactoMineR) 
res.ca.s <- CA(datos_s.acs,
               ncp=2,
               graph=F)           
res.ca.s
#
library(factoextra)
get_eigenvalue(res.ca.s)
#1. Encuentre los perfiles fila del área de PSICOLOGÍA. Interprete uno de ellos. (1
#punto)
#
summary(res.ca.s,nb.dec = 3, ncp = 2) 
#                     Iner*1000     Dim.1     ctr    cos2     Dim.2     ctr    cos2  
#psicologia         |   198.234 |   0.422  12.625   0.182 |  -0.884  57.121   0.803 |
#interprtetacion_:
#Esta alejada del origen porque su inercia total es  198.234, es explicada por la dimension2 al 57.121%.
#
#2.Encuentre los perfiles columna para el atributo CONFIABILIDAD. Interprete uno de
#ellos (1 punto)

#                     Iner*1000     Dim.1     ctr    cos2     Dim.2     ctr    cos2
#confiabilidad      |    87.094 |   0.508  14.177   0.466 |  -0.543  16.665   0.533 |
#interprtetacion_:
#Esta alejada del origen porque su inercia total es  87.094, es explicada por la dimension2 al 16.665%.
#
#3.Indique si se justifica realizar un análisis de correspondencia simple. Presente la
#prueba de hipótesis correspondiente. (2 puntos)
#
# Prueba de Independencia Chi-Cuadrado 
#chi-cuadrado
chisq.test(datos_s.acs)
#
# p-value < 2.2e-16
#interprtetacion_:
#se rechaza la h0,es decir las categorias de la variable 'area' depende de las categorias de la variaable 'atributos'
#
#4.Realice el análisis de correspondencia simple. Justifique el número de
#dimensiones o componentes a retener (2 puntos) 
#
#
library(factoextra)
get_eigenvalue(res.ca.s)
#
#eigenvalue variance.percent cumulative.variance.percent
#Dim.1 0.28646960        44.362664                    44.36266
#Dim.2 0.27856201        43.138095                    87.50076
#Dim.3 0.05945767         9.207611                    96.70837
#Dim.4 0.02125553         3.291630                   100.00000
#
#interprtetacion_:
#con 2  componenete retengo la mayor inercia total al 87.50076%  
#
#5.Interprete la contribución absoluta del área de MEDICINA con la Dimensión 1. (2
#puntos)
summary(res.ca.s,nb.dec = 3, ncp = 2) 
#
#                      Iner*1000     Dim.1     ctr    cos2     Dim.2     ctr    cos2 
#medicina           |    49.155 |   0.029   0.062   0.004 |   0.119   1.096   0.062 |
#
#interprtetacion_:
#la categoria 'medicina'  explica  la dimension1 al 0.062%.
#
#6.Interprete la contribución relativa del atributo AMABILIDAD con la Dimensión 2. (2
#                                                                                 puntos)
#
#                      Iner*1000     Dim.1     ctr    cos2     Dim.2     ctr    cos2  
#amabilidad         |    82.801 |  -0.520  17.269   0.597 |  -0.392  10.096   0.340 |
#interpretaacion:
#la dimension2 explica la categoria 'amabilidad' al 0.340%.
#
#7.Interprete la inercia del área de TÓPICO (1 punto)
#
#                    Iner*1000     Dim.1     ctr    cos2     Dim.2     ctr    cos2 
#topico             |    62.761 |   0.390  11.186   0.511 |   0.256   4.973   0.221 |
#interprtetacion_:
#la inercia total que representa la categoria 'topico' es  62.761% podemos observar que es la  segunda
#categoria de la varaible 'area' 
#que estara cerca al origen.
#
#8.Interprete la inercia del atributo HIGIENE (1 punto)
#
#                      Iner*1000     Dim.1     ctr    cos2     Dim.2     ctr    cos2  
#higine             |    81.390 |   0.238   2.868   0.101 |   0.632  20.896   0.715 |
#interpretacion:
#la inercia total que representa la categoria 'higine' es  81.390% podemos observar que es la  segunda
#categoria de la varaible 'atributos'  que estara mas cercano al origen.
#
#9.Realice las conclusiones finales del estudio presentando las correspondencias
#entre las áreas y atributos. Presente el gráfico correspondiente. (4 puntos)
#
#
fviz_ca_biplot(res.ca.s, repel = T) + theme_light()
#
#interpretacion:
summary(res.ca.s)
#
#                    c1	   c2
#recepcion	       ca(-)	
#medicina		              cr(+)
#topico	           cr(+)	
#odontologia	          	ca(+)
#psicologia	             	ca(-)
#amabilidad	       ca(-)	
#eficiencia		            cr(+)
#confiabilidad		        ca(-)
#confidenciabilidad		    ca(-)
#rapidez	         ca(-)	
#tecnologia	            	ca(+)
#higine		                ca(+)
#
#interpretacion:
#
#############
#componente1#
#############
#
#la categoria 'recepcion' esta asociado a la categoria 'amabilidad' y tambien a la categoria 'rapidez'
#
#la categoria 'topico' se contrapone a la categoria 'recepcion' y ademas no esta asociada a ninguna
#categoria de la variable 'atributos'
#
#############
#componente2#
#############
#
#la categoria 'medicina' y la categoria 'odontologia' esta asociado a las categorias 'eficiencia','tecnologia' y 
#'higine'
#
#la categoria 'psicologia' esta asociado a las categorias 'confiabilidad' y 'confidenciabilidad'
#
#la categoria 'psicologia' se contrapone a las categorias 'odontologia' y 'medicina'
#
#
#10.¿Cuál o cuáles serían las áreas del centro médico que estaría(n) más próxima al
#IDEAL y que características tendría esta área? (4 puntos)
#
#IDEAL ;  lo mas altos votos 56,43,42,33,50
#
datos_s.acs1 <- matrix(c(56,13,4,1,56,4,11,56,#ingresando columana ideal
                        15,43,21,18,20,9,19,43,
                        13,15,24,22,6,20,42,42,
                        6,19,7,5,8,33,26,33,
                        33,4,50,45,4,1,0,50),
                      nrow=5,byrow=T)
#
# Asignación de nombres a las filas y columnas de la tabla
dimnames(datos_s.acs1)<-list(area=c("recepcion", "medicina", "topico", 
                                   "odontologia","psicologia")
                            ,atributos=c("amabilidad","eficiencia","confiabilidad","confidenciabilidad",
                                         "rapidez","tecnologia","higine","ideal"))
library(FactoMineR) 
res.ca.s1 <- CA(datos_s.acs1,
               ncp=2,
               graph=T,
               col.sup = 8)
#
summary(res.ca.s1)
#IDEAL
#
#Supplementary column
#                      Dim.1   cos2    Dim.2   cos2  
#ideal              | -0.070  0.376 | -0.068  0.356
#
#interpretacion : esta mas asociada a la dimension1 o componente1, ya que contribucion relativa es 0.376,entonces
#esta mas asociada a la categoria 'recepcion' y tambien a las categorias 'amabilidad' y 'rapidez'.