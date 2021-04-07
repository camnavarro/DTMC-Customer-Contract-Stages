#CASO 1 PROCESOS ESTOCASTICOS
#PAULA VALENCIA 
#LAUR VIVIANA VARGAS
#JUAN DIEGO HERNANDEZ
#CAMILO NAVARRO

install.packages("markovchain")
library(markovchain)
library(MASS)

#Se leen los datos de los archivos .csv de cada mes
DatosNoviembre<- read.csv("/Users/camilonavarro/Downloads/Nov-Grupo.csv", sep=",", dec=".")
DatosDiciembre<- read.csv("/Users/camilonavarro/Downloads/Dic-Grupo.csv", sep=",", dec=".")
DatosEnero<- read.csv("/Users/camilonavarro/Downloads/Ene-Grupo.csv", sep=",", dec=".")

#Se declaran las "secuencias iniciales", estas aun no han sido divididas por cliente
Seq_Nov_Inicial<-DatosNoviembre[,2]
Seq_Dic_Inicial<-DatosDiciembre[,2]
Seq_Ene_Inicial<-DatosEnero[,2]

#Se separa la secuencia por cliente
Seq_Nov <- split(DatosNoviembre$Secuencia, DatosNoviembre$Cliente, drop=FALSE)
Seq_Dic <- split(DatosDiciembre$Secuencia, DatosDiciembre$Cliente, drop=FALSE)
Seq_Ene <- split(DatosEnero$Secuencia, DatosEnero$Cliente, drop=FALSE)

#Se crean las matrices de frecuencia
Frec_Nov<-createSequenceMatrix(Seq_Nov)
Frec_Dic<-createSequenceMatrix(Seq_Dic)
Frec_Ene<-createSequenceMatrix(Seq_Ene)

Bar_Nov<-barplot(Frec_Nov, main="FRECUENCIA NOVIEMBRE", col=c("antiquewhite4", "antiquewhite3", "antiquewhite"))
Bar_Dic<-barplot(Frec_Dic, main="FRECUENCIA DICIEMBRE", col=c("antiquewhite4", "antiquewhite3", "antiquewhite"))
Bar_Ene<-barplot(Frec_Ene, main="FRECUENCIA ENERO", col=c("antiquewhite4", "antiquewhite3", "antiquewhite"))

#Se declaran los estados de la variable
Estados<- c("A", "B", "C", "W", "L")

#Se crean las matrices de transicion y las DTCM

#NOVIEMBRE
Trans_Nov<-Frec_Nov/rowSums(Frec_Nov)
Trans_Nov[4,4]<-1
Trans_Nov[5,5]<-1
Trans_Nov[4,1]<-0
Trans_Nov[4,2]<-0
Trans_Nov[4,3]<-0
Trans_Nov[4,5]<-0
Trans_Nov[5,1]<-0
Trans_Nov[5,2]<-0
Trans_Nov[5,3]<-0
Trans_Nov[5,4]<-0

mcNoviembre<-new("markovchain", states=Estados, byrow=TRUE, 
                 transitionMatrix=Trans_Nov, name="Noviembre")

#DICIEMBRE
Trans_Dic<-Frec_Dic/rowSums(Frec_Dic)
Trans_Dic[4,4]<-1
Trans_Dic[5,5]<-1
Trans_Dic[4,1]<-0
Trans_Dic[4,2]<-0
Trans_Dic[4,3]<-0
Trans_Dic[4,5]<-0
Trans_Dic[5,1]<-0
Trans_Dic[5,2]<-0
Trans_Dic[5,3]<-0
Trans_Dic[5,4]<-0
mcDiciembre<-new("markovchain", states=Estados, byrow=TRUE, 
                 transitionMatrix=Trans_Dic, name="Diciembre")

#ENERO
Trans_Ene<-Frec_Ene/rowSums(Frec_Ene)
Trans_Ene[4,4]<-1
Trans_Ene[5,5]<-1
Trans_Ene[4,1]<-0
Trans_Ene[4,2]<-0
Trans_Ene[4,3]<-0
Trans_Ene[4,5]<-0
Trans_Ene[5,1]<-0
Trans_Ene[5,2]<-0
Trans_Ene[5,3]<-0
Trans_Ene[5,4]<-0
mcNoviembre<-new("markovchain", states=Estados, byrow=TRUE, 
                 transitionMatrix=Trans_Dic, name="Enero")

#Imprimimos las cadenas y vemos su respectivo resumen
mcNoviembre
summary(mcNoviembre)

mcDiciembre
summary(mcNoviembre)

mcEnero
summary(mcEnero)


#Sacamos las probabilidades de absorcion para cada mes
absNov<-absorptionProbabilities(mcNoviembre)
absDic<-absorptionProbabilities(mcDiciembre)
absEne<-absorptionProbabilities(mcEnero)

absNov
absDic
absEne


#Sacamos los tiempos esperados de absorcion para cada mes
tNov<-meanAbsorptionTime(mcNoviembre)
tNtDic<-meanAbsorptionTime(mcDiciembre)
tEne<-meanAbsorptionTime(mcEnero)

tNov
tDic
tEne

#Graficamos las DTMC
PlotNov<-as(mcNoviembre, "markovchain")
plot(PlotNov)

PlotDec<-as(mcDiciembre, "markovchain")
plot(PlotDec)

PlotEne<-as(mcEnero, "markovchain")
plot(PlotEne)

assessStationarity()
assessOrder()
verifyHomogeneity(mcNoviembre, verbose=TRUE)
verifyMarkovProperty(mcNoviembre, verbose=TRUE)

#END


