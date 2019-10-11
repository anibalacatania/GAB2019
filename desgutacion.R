library(SensoMineR)
library(readxl)
library(predictmeans)
library(car)
library(agricolae)
library(lmerTest)

####################Datos Sensoriales######################

#importo los datos a través de import Dataset (from Excel)

data<-degustacion

data<-as.data.frame(data) # lo transformo en data frame
str(data) #para observar la estructura de los datos

#configuramos producto, panelista y sesion como factor

data$producto<-as.factor(data$producto)
data$panelista<-as.factor(data$panelista)

table(data$producto,data$panelista) # tabla que resume las muestras que evaluaron los distintos panelistas
nlevels(data$panelista)



### Análisis multivariado para chequear que no se produzcan errores de tipo 1

da.a=as.matrix(data [,-c(1:2)])
da.man<-manova(da.a~(panelista+producto), data=data)
a<-summary(da.man, test="Wilks")
output <- capture.output(a)
as.data.frame(output)


### Boxplot
boxplot(Floral~producto,data=data,col="lightgray",main="Floral")
boxplot(Cítricos~producto,data=data,col="lightgray",main="Cítricos")
boxplot(Fruta.roja~producto,data=data,col="lightgray",main="Fruta roja")
boxplot(madera~producto,data=data,col="lightgray",main="madera")
boxplot(Astringencia~producto,data=data,col="lightgray",main="Astringencia")










#cuadro de interacciones

res.panelperf <- panelperf(data,firstvar=3,formul="~producto+panelista",random=TRUE)

res.panelperf$p.value
coltable(res.panelperf$p.value[order(res.panelperf$p.value[,1]),],col.lower="gray",cex=0.8)




#####################################Modelo fijo

variable<-data$Floral

model <- aov(variable~producto+panelista,data=data)
summary(model)

res.LSD <- LSD.test(model,"producto", p.adj="bonferroni")

names(res.LSD)
bar.group(res.LSD$groups,ylim=c(0,10),density=4,border="black",cex.names=0.7)

# normality assumption for fixed models
qqnorm(variable)
qqline(variable)

plot(variable, resid(model), xlab="Measurement", ylab="Residuals")
abline(0,0)


leveneTest(variable~producto*panelista,data=data)



##############################modelo mixto#######################

mixedmodel <- lmer(variable~producto
                  + (1 | panelista) 
                    , data=data)
anova(mixedmodel)
rand(mixedmodel)

### checking assumptions for mixed models
qqnorm(resid(mixedmodel))
qqline(resid(mixedmodel))


### resid vs predicted
#plot(variable, resid(mixedmodel), xlab="Measurement", ylab="Residuals")
#abline(0,0)




predictmeans(mixedmodel, "producto", adj="BH",pairwise=FALSE,Df=5,barplot=TRUE)
#predictmeans(fm, "nitro:Variety", pair=TRUE, atvar="Variety", adj="BH")


