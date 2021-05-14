library(readxl)
coag<- read_excel("datosc.exportado.xlsx")
coag=as.data.frame(coag)
coag
str(coag)
#variables:
divertid<-coag$divertid
presupu=coag$presupu
aprovech=coag$aprovech
buenacom=coag$buenacom
noimport=coag$noimport
ahorro=coag$ahorro


grp=coag$grp
library(agricolae)

#----------------------------------------------------------------------------------

###########
#divertido#
###########

anva1<-aov(divertid~grp,coag)
compd1<-duncan.test(anva1,"grp",group=FALSE)
compd1

#$means
#divertid       std r Min Max  Q25 Q50 Q75
#1 5.666667 1.0000000 9   4   7 5.00 6.0   6
#2 1.666667 0.5163978 6   1   2 1.25 2.0   2
#3 3.500000 0.5477226 6   3   4 3.00 3.5   4

#$comparison
#difference pvalue signif.       LCL        UCL
#1 - 2   4.000000  0e+00     ***  3.069142  4.9308577          #siginificativo  *** 
#1 - 3   2.166667  1e-04     ***  1.279474  3.0538591          #significativo   ***
#2 - 3  -1.833333  4e-04     *** -2.720526 -0.9461409          #significativo   ***
#--------------------------------------------------------------------------------

#############
#presupuesto#
#############

anva<-aov(presupu~grp,coag)
compd<-duncan.test(anva,"grp",group=FALSE)
compd

#$means
#presupu       std r Min Max  Q25 Q50 Q75
#1 3.666667 0.8660254 9   2   5 3.00   4   4
#2 3.000000 0.6324555 6   2   4 3.00   3   3
#3 5.500000 1.3784049 6   3   7 5.25   6   6

#$comparison
#difference pvalue signif.        LCL        UCL
#1 - 2  0.6666667 0.2301         -0.4609352  1.7942686     # no significativo 
#1 - 3 -1.8333333 0.0031      ** -2.9609352 -0.7057314     #significativo      **
#2 - 3 -2.5000000 0.0003     *** -3.6830994 -1.3169006     #significativo      ***

#--------------------------------------------------------------------------------

##########
#aprovech#
##########

anva2<-aov(aprovech~grp,coag)
compd2<-duncan.test(anva2,"grp",group=FALSE)
compd2

#$means
#aprovech       std r Min Max  Q25 Q50 Q75
#1 6.000000 1.0000000 9   4   7 6.00 6.0   7
#2 1.833333 0.7527727 6   1   3 1.25 2.0   2
#3 3.333333 0.8164966 6   2   4 3.00 3.5   4

#$comparison
#difference pvalue signif.       LCL        UCL
#1 - 2   4.166667 0.0000     ***  3.102190  5.2311431            #significativo ***
#1 - 3   2.666667 0.0000     ***  1.652123  3.6812100            #significativo ***
#2 - 3  -1.500000 0.0061      ** -2.514543 -0.4854566            #significativo **

#--------------------------------------------------------------------------------

############
#buena comp#
############

anva3<-aov(buenacom~grp,coag)
compd3<-duncan.test(anva3,"grp",group=FALSE)
compd3

#$means
#buenacom       std r Min Max Q25 Q50 Q75
#1 3.222222 0.8333333 9   2   4   3 3.0   4
#2 3.500000 1.0488088 6   2   5   3 3.5   4
#3 6.000000 0.6324555 6   5   7   6 6.0   6

#$comparison
#difference pvalue signif.       LCL        UCL
#1 - 2 -0.2777778 0.5565         -1.251723  0.6961673            #no significativo
#1 - 3 -2.7777778 0.0000     *** -3.799658 -1.7558978            #significativo ***
#2 - 3 -2.5000000 0.0000     *** -3.473945 -1.5260549            #significativo ***

#---------------------------------------------------------------------------------

###########
#no import#
###########

anva4<-aov(noimport~grp,coag)
compd4<-duncan.test(anva4,"grp",group=FALSE)
compd4

#$means
#noimport       std r Min Max  Q25 Q50 Q75
#1      2.0 0.8660254 9   1   3 1.00 2.0   3
#2      5.5 1.0488088 6   4   7 5.00 5.5   6
#3      3.5 0.8366600 6   2   4 3.25 4.0   4

#$comparison
#difference pvalue signif.        LCL        UCL
#1 - 2       -3.5 0.0000     *** -4.5953372 -2.4046628            #significativo ***
#1 - 3       -1.5 0.0074      ** -2.5439565 -0.4560435            #significativo **
#2 - 3        2.0 0.0008     ***  0.9560435  3.0439565            #significativo ***

#---------------------------------------------------------------------------------

########
#ahorro#
########

anva5<-aov(ahorro~grp,coag)
compd5<-duncan.test(anva5,"grp",group=FALSE)
compd5

#$means
#ahorro       std r Min Max Q25 Q50 Q75
#1 4.000000 0.7071068 9   3   5   4 4.0   4
#2 3.333333 0.8164966 6   2   4   3 3.5   4
#3 6.000000 1.5491933 6   3   7   6 6.5   7

#$comparison
#difference pvalue signif.        LCL        UCL
#1 - 2  0.6666667 0.2527         -0.5185293  1.8518626            #no significativo 
#1 - 3 -2.0000000 0.0023      ** -3.1851959 -0.8148041            #significativo **
#2 - 3 -2.6666667 0.0002     *** -3.9101947 -1.4231386            #significativo ***
