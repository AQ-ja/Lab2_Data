# Loading
library("readxl")
library(ggplot2)

#--------------------------------------------------
# Analisis Exploratorio
#--------------------------------------------------

# xls files
data_impor <- read_excel("Importacion.xlsx")
View(data_impor)


#Clasifiacion de las variables
#Fecha = 
#Superior = 
# Regular = 
# Diesel  =
# Total_Gasolinas = 
# Total_Importaciones = 

hist(x = data_impor$Superior)
hist(x = data_impor$Regular)
hist(x = data_impor$Diesel)



# Mes con el cual se tiene mayor importacion de Gasolina
gas<-data_impor[order(-data_impor$Superior),]
super<- head(gas,10)
super<-super[,1:2]
View(super)


#Mes en el cual se tiene mayor importacion de Regular 
reg<-data_impor[order(-data_impor$Regular),]
regu<- head(reg,10)
regu<-regu[,1:2]
View(regu)

#Mes en el que se tiene mayor importacion de Disel 
diesel<-data_impor[order(-data_impor$Diesel),]
dies<- head(diesel, 10)
dies<-dies[,1:2]
View(dies)




def<-data.frame(Tipo=c("Gasolina","Regular","Diesel"), Media=c(mean(data_impor$Superior), mean(data_impor$Regular), mean(data_impor$Diesel)))

grafdef<-ggplot(data=def, aes(x=Tipo, y=Media, fill=Tipo)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=as.integer(Promedio)), vjust=1.6, color="blue",
            position = position_dodge(0.9), size=3.5)+
  labs(title="Promedio de importaciones por tipo de gasolina", y="Promedio")+
  theme(legend.position="none")



grafsup<-ggplot(data_impor, aes(x = format(data_impor$Fecha, format='%Y'), y = data_impor$Superior, fill =format(data_impor$Fecha, format='%Y') )) +
  geom_bar(stat = "identity",position=position_dodge())+labs(title="Importaciones por año de gasolina Superior", y="Importacion" , x='Año')+
  theme(legend.position="none")


#----------------------------------------------
# Series de tiempo y modelos 
#----------------------------------------------

#Librerias necesarias
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)

impor <- read_excel("Importacion.xlsx")

super<-impor[c('Fecha','Superior')]
regular<-impor[c('Fecha','Regular')]
diesel<-impor[c('Fecha','Diesel')]

# Para el combustible Superior 
super_ts <- ts( super$`Superior`, start = c(2001,1),frequency = 12)

start(super_ts)
end(super_ts)
frequency(super_ts)
plot(super_ts)

abline(reg=lm(super_ts~time(super_ts)), col=c("red"))
plot(aggregate(super_ts,FUN=mean))
dec.Super<-decompose(super_ts)
plot(dec.Super)
plot(dec.Super$seasonal)


#Para el combustible Regular 

regular_ts <- ts( regular$`Regular`, start = c(2001,1),frequency = 12)

start(regular_ts)
end(regular_ts)
frequency(regular_ts)
plot(regular_ts)

abline(reg=lm(regular_ts~time(regular_ts)), col=c("red"))
plot(aggregate(regular_ts,FUN=mean))
dec.Regul<-decompose(regular_ts)
plot(dec.Regul)
plot(dec.Regul$seasonal)


# Para el combustible Diesel

diesel_ts <- ts( diesel$`Diesel`, start = c(2001,1),frequency = 12)

start(diesel_ts)
end(diesel_ts)
frequency(diesel_ts)
plot(diesel_ts)

abline(reg=lm(diesel_ts~time(diesel_ts)), col=c("red"))
plot(aggregate(diesel_ts,FUN=mean))
dec.Diesel<-decompose(diesel_ts)
plot(dec.Diesel)
plot(dec.Diesel$seasonal)






