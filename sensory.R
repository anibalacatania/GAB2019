
####################Datos Sensoriales######################

#importo los datos a través de import Dataset (from Excel)

datos17<-as.data.frame(datos17) # lo transformo en data frame
str(datos17) #para observar la estructura de los datos

#configuramos producto, panelista y sesion como factor
datos17$sesion<-as.factor (datos17$sesion)
datos17$producto<-as.factor(datos17$producto)
datos17$panelista<-as.factor(datos17$panelista)

table(datos17$producto,datos17$panelista) # tabla que resume las muestras que evaluaron los distintos panelistas

######## en caso de que hayan datos perdidos puedo usar  el paquete missMDA#########

library(missMDA)
da.imp=imputeFAMD(datos17, ncp = 3)
data17<-da.imp$completeObs



### Análisis multivariado para chequear que no se produzcan errores de tipo 1

da.a=as.matrix(datos17 [,-c(1:3)])
da.man<-manova(da.a~(panelista+sesion+producto)^2, data=datos17)
a<-summary(da.man, test="Wilks")
output <- capture.output(a)
as.data.frame(output)






library(SensoMineR)
#caudro de interacciones

res.panelperf <- panelperf(data17,firstvar=4,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion",random=FALSE)

res.panelperf$p.value
coltable(res.panelperf$p.value[order(res.panelperf$p.value[,1]),],col.lower="gray",cex=0.8)



#cuadro de interacciones de los panelistas

#poder de Discrimination
res.paneliperf <- paneliperf(datos17,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion", formul.j="~producto+sesion",col.j=1,firstvar=4,synthesis=TRUE)
names(res.paneliperf)
res.magicsort <- magicsort(res.paneliperf$prob.ind,method="median")
round(res.magicsort,3)
coltable(round(res.magicsort,2),level.lower=0.05,level.upper=0.06, col.lower="gainsboro",col.upper="gray",cex = 0.8)

#Acuerdo de los panelistas
res.paneliperf <- paneliperf(datos17,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion", formul.j="~producto+sesion",col.j=1,firstvar=4,synthesis=TRUE)
names(res.paneliperf)
res.magicsort <- magicsort(res.paneliperf$agree.ind,method="median")
round(res.magicsort,3)
coltable(round(res.magicsort,2),level.lower=-0.001,level.upper=0.80, col.lower="gainsboro",col.upper="gray",cex = 0.8)


#Repetibilidad
res.paneliperf <- paneliperf(datos17,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion", formul.j="~producto+sesion",col.j=1,firstvar=4,synthesis=TRUE)
names(res.paneliperf)
res.magicsort <- magicsort(res.paneliperf$res.ind,method="median")
round(res.magicsort,3)
coltable(round(res.magicsort,2),level.lower=0.001,level.upper=1.96, col.lower="gainsboro",col.upper="gray",cex = 0.8)




### PCA con elipses
res.panellipse <- panellipse(datos17,col.p=3,col.j=1,firstvar=4,level.search.desc=1,alpha = 0.1)
coltable(res.panellipse$hotelling, main.title =  "P-values for the Hotelling T2 tests")

panellipse.session <- panellipse.session(datos17,col.p=3,col.j=1,col.s=2,firstvar=4,level.search.desc=1)

###modelo mixto
library(lmerTest)
mixedmodel <- lmer(Intensidad.Color~producto
                  + (1 | panelista) 
                  + (1 | sesion)
                  + (1 | panelista:sesion)
                  + (1 | panelista:producto)
                  + (1 | sesion:producto), data=datos17)
anova(mixedmodel)
rand(mixedmodel)


qqnorm(resid(mixedmodel))
qqline(resid(mixedmodel))


mixedmodel <- lmer(Fruta.Fresca~producto+sesion+producto:sesion
                       + (1|panelista)+(1|producto:panelista)+(1|panelista:sesion),data=data17)
anova(mixedmodel)


anova(mixedmodel)





library(predictmeans)


predictmeans(mixedmodel, "producto", adj="BH",pairwise=FALSE,Df=5,barplot=TRUE)
#predictmeans(fm, "nitro:Variety", pair=TRUE, atvar="Variety", adj="BH")


#graficos de barra
options(contrasts=c("contr.sum","contr.sum"))
model <- aov(Matiz.Verde~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion,data=datos17)
library(agricolae)
res.LSD <- LSD.test(model,"producto", p.adj="bonferroni")

names(res.LSD)
bar.group(res.LSD$groups,ylim=c(0,10),density=4,border="black",cex.names=0.7)

# normality assumption
qqnorm(datos17$Fruta.Fresca)
qqline(datos17$Fruta.Fresca)

residuals <- resid(model)
Model <- aov(datos17$Matiz.Amarillo~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion,data=datos17)
Model
plot(datos17$Matiz.Amarillo, residuals, xlab="Measurement", ylab="Residuals")
abline(0,0)


