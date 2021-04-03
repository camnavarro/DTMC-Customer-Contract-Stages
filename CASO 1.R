#CASO 1 PROCESOS ESTOCASTICOS
#PAULA VALENCIA 
#LAUR VIVIANA VARGAS
#JUAN DIEGO HERNANDEZ
#CAMILO NAVARRO

install.packages("markovchain")
library(markovchain)
library(MASS)


#Se declaran los estados de la variable
Estados<- c("A", "B", "C", "W", "L")

#Llenamos la matriz de frecuencias de saltos, obtenida con excel
Frec_Nov<-matrix(c(905, 575, 0, 0, 375, 
                    0, 386, 427, 0, 306, 
                    0, 0, 207, 157, 269, 
                    0,0,0,1,0,
                    0,0,0,0,1), 
                  nrow=5, byrow=TRUE)

#Se calcula la matriz de transicion
Trans_Nov<-Frec_Nov/rowSums(Frec_Nov)

#Se crea la DTMC
mcNoviembre<-new("markovchain", states=Estados, byrow=TRUE, 
                 transitionMatrix=Trans_Nov, name="Noviembre")

#Hcemos lo mismo para Diciembre y Enero

Frec_Dec<-matrix(c(10111, 6088, 0, 0, 3912, 
                    0, 4004, 4607, 0, 3048, 
                    0, 0, 1964, 1567, 3040, 
                    0,0,0,1,0,
                    0,0,0,0,1), 
                  nrow=5, byrow=TRUE)

Trans_Dec<-Frec_Dec/rowSums(Frec_Dec)

mcDiciembre<-new("markovchain", states=Estados, byrow=TRUE, 
                 transitionMatrix=Trans_Dec, name="Diciembre")



Frec_Ene<-matrix(c(3988, 2300, 0, 0, 1554, 
                   0, 1533, 1731, 0, 1179, 
                   0, 0, 738, 610, 1121, 
                   0,0,0,1,0,
                   0,0,0,0,1), 
                 nrow=5, byrow=TRUE)

Trans_Ene<-Frec_Ene/rowSums(Frec_Ene)


mcEnero<-new("markovchain", states=Estados, byrow=TRUE, 
                 transitionMatrix=Trans_Ene, name="Enero")

#Vemos un resumen de las tres DTCM obtenidas y las imprimimos
show(mcNoviembre)
summary(mcNoviembre)

show(mcDiciembre)
summary(mcDiciembre)

show(mcEnero)
summary(mcEnero)


#Sacamos las probabilidades de absorcion para cada mes
absNov<-absorptionProbabilities(mcNoviembre)
absDic<-absorptionProbabilities(mcDiciembre)
absEne<-absorptionProbabilities(mcEnero)


#Sacamos los tiempos esperados de absorcion para cada mes
tNov<-meanAbsorptionTime(mcNoviembre)
tDic<-meanAbsorptionTime(mcDiciembre)
tEne<-meanAbsorptionTime(mcEnero)


#Graficamos las DTMC
PlotNov<-as(mcNoviembre, "markovchain")
plot(PlotNov)

PlotDec<-as(mcDiciembre, "markovchain")
plot(PlotDec)

PlotEne<-as(mcEnero, "markovchain")
plot(PlotEne)

#END


