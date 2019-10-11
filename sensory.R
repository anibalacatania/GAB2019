library(SensoMineR)
library(readxl)
library(missMDA)
library(lmerTest)
library(predictmeans)
library(car)
####################Datos Sensoriales######################

#importo los datos a través de import Dataset (from Excel)

data<-datos17

data<-as.data.frame(data) # lo transformo en data frame
str(data) #para observar la estructura de los datos

#configuramos producto, panelista y sesion como factor
data$sesion<-as.factor (data$sesion)
data$producto<-as.factor(data$producto)
data$panelista<-as.factor(data$panelista)

table(data$producto,data$panelista) # tabla que resume las muestras que evaluaron los distintos panelistas
nlevels(data$panelista)
######## en caso de que hayan datos perdidos puedo usar  el paquete missMDA#########


da.imp=imputeFAMD(data, ncp = 3)
data<-da.imp$completeObs



### Análisis multivariado para chequear que no se produzcan errores de tipo 1

da.a=as.matrix(data [,-c(1:3)])
da.man<-manova(da.a~(panelista+sesion+producto)^2, data=data)
a<-summary(da.man, test="Wilks")
output <- capture.output(a)
as.data.frame(output)


### Boxplot
boxplot(Intensidad.Color~producto,data=data,col="lightgray",main="Intensidad de color")
boxplot(Matiz.Amarillo~producto,data=data,col="lightgray",main="Matiz amarillo")
boxplot(Matiz.Verde~producto,data=data,col="lightgray",main="Matiz Verde")
boxplot(Intensidad.Aromatica~producto,data=data,col="lightgray",main="Intensidad Aromatica")
boxplot(Linalol~producto,data=data,col="lightgray",main="Linalol")
boxplot(Fruta.Fresca~producto,data=data,col="lightgray",main="Fruta Fresca")
boxplot(Fruta.Seca~producto,data=data,col="lightgray",main="Fruta Seca")
boxplot(Oxidado~producto,data=data,col="lightgray",main="Oxidado")
 



#grafico de interacciones
#graphinter(data,col.p=3,col.j=1,firstvar=4,lastvar=11,numr=1,numc=1)
graphinter(data[,c("panelista", "sesion", "producto", "Fruta.Seca")],col.p=3,col.j=1,firstvar=4,lastvar=4,numr=1,numc=1)






#cuadro de interacciones

res.panelperf <- panelperf(data,firstvar=4,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion",random=FALSE)

res.panelperf$p.value
coltable(res.panelperf$p.value[order(res.panelperf$p.value[,1]),],col.lower="gray",cex=0.8)

# contribución de los panelistas a la interacción
results=interact(data[,c("panelista", "sesion", "producto", "Fruta.Seca")],col.p=3,col.j=1,firstvar=4)



#cuadro de interacciones de los panelistas

#poder de Discrimination
res.paneliperf <- paneliperf(data,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion", formul.j="~producto+sesion",col.j=1,firstvar=4,synthesis=TRUE)
names(res.paneliperf)
res.magicsort <- magicsort(res.paneliperf$prob.ind,method="median")
round(res.magicsort,3)
coltable(round(res.magicsort,2),level.lower=0.05,level.upper=0.06, col.lower="gainsboro",col.upper="gray",cex = 0.8)

#Acuerdo de los panelistas
res.paneliperf <- paneliperf(data,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion", formul.j="~producto+sesion",col.j=1,firstvar=4,synthesis=TRUE)
names(res.paneliperf)
res.magicsort <- magicsort(res.paneliperf$agree.ind,method="median")
round(res.magicsort,3)
coltable(round(res.magicsort,2),level.lower=-0.001,level.upper=0.80, col.lower="gainsboro",col.upper="gray",cex = 0.8)


#Repetibilidad
res.paneliperf <- paneliperf(data,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion", formul.j="~producto+sesion",col.j=1,firstvar=4,synthesis=TRUE)
names(res.paneliperf)
res.magicsort <- magicsort(res.paneliperf$res.ind,method="median")
round(res.magicsort,3)
coltable(round(res.magicsort,2),level.lower=0.001,level.upper=1.96, col.lower="gainsboro",col.upper="gray",cex = 0.8)




### PCA con elipses
res.panellipse <- panellipse(data,col.p=3,col.j=1,firstvar=4,level.search.desc=1,alpha = 0.1,nbchoix = 50)
coltable(res.panellipse$hotelling, main.title =  "P-values for the Hotelling T2 tests")

panellipse.session <- panellipse.session(data,col.p=3,col.j=1,col.s=2,firstvar=4,level.search.desc=1)

###modelo mixto

variable<-data$Intensidad.Color


mixedmodel <- lmer(variable~producto
                  + (1 | panelista) 
                  + (1 | sesion)
                  + (1 | panelista:sesion)
                  + (1 | panelista:producto)
                  + (1 | sesion:producto), data=data)
anova(mixedmodel)
rand(mixedmodel)

### checking assumptions for mixed models
qqnorm(resid(mixedmodel))
qqline(resid(mixedmodel))


### resid vs predicted
#plot(data$Intensidad.Color, resid(mixedmodel), xlab="Measurement", ylab="Residuals")
#abline(0,0)




predictmeans(mixedmodel, "producto", adj="BH",pairwise=FALSE,Df=5,barplot=TRUE)
#predictmeans(fm, "nitro:Variety", pair=TRUE, atvar="Variety", adj="BH")


#Fixed models
options(contrasts=c("contr.sum","contr.sum"))
model <- aov(variable~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion,data=data)
library(agricolae)
res.LSD <- LSD.test(model,"producto", p.adj="bonferroni")

names(res.LSD)
bar.group(res.LSD$groups,ylim=c(0,10),density=4,border="black",cex.names=0.7)

# normality assumption for fixed models
qqnorm(variable)
qqline(variable)

plot(data$Intensidad.Color, resid(model), xlab="Measurement", ylab="Residuals")
abline(0,0)


leveneTest(variable~producto*panelista,data=data)

