## Script: "biomasa2.r"                           /
## Sobre:  ajuste de modelos de biomasa           /
## y obtencion de predicciones (camino mas simple)/     
#-------------------------------------------------/  
# Este es un script que he preparado para         #
#  que Ud entienda y ejercite el ajuste de modelos#
#  de biomasa                                     #
#-------------------------------------------------# 
## Profesor: Christian Salas Eljatib
## E-mail: cseljatib AT gmail DOT com
## Web: https://eljatib.com
#======================================
library(datana)
data(biomass2)
df<-biomass2
head(df)
#?biomass2
mean(df$dap)
descstat(df$wtot)
str(df)
tapply(df$atot,df$spp,mean)
summary(biomass2)
tapply(df$atot,df$spp,length)

hist(df$dap)
hist(df$wtot)
plot(wtot~dap,data=df)


#modelo biomasa total
plot(wtot~I(dap^2),data=df)


m1<-lm(wtot~I(dap^2),data=df)
summary(m1)
help(biomass2)
dap <- 30:35
predict(m1, data.frame(dap))

11.816132 + 0.360998*30^2
-11.816132 + 0.360998*30^2
data.frame(dap)
##w=b0+b1*d^2h
m2<-lm(wtot~I(atot*dap^2),data=df)
summary(m2)


sigma.e.m1<-summary(m1)$sigma
100*sigma.e.m1/mean(df$wtot)

plot(wtot~dap,data=df)
plot(wfuste~dap,data=df)

#modelo biomasa fustal
m2<-lm(wfuste~I(dap^2),data=df)
summary(m2)

sigma.e.m2<-summary(m2)$sigma
100*sigma.e.m2/mean(df$wfuste)

plot(wtot~dap,data=df)
plot(wfuste~dap,data=df)
plot(wramas~dap,data=df)

#modelo biomasa ramas
m3<-lm(wramas~I(dap^2),data=df)
summary(m3)

sigma.e.m3<-summary(m3)$sigma
100*sigma.e.m3/mean(df$wramas)

dap<-5:40
dap

w.tot<-predict(m1,data.frame(dap))
w.fus<-predict(m2,data.frame(dap))
w.ram<-predict(m3,data.frame(dap))

plot(w.tot~dap,type="l",bty="l")
lines(dap,w.fus,col="blue")
lines(dap,w.ram,col="red")

##@@@@@@@@@@@@@@@@@@@@@@@@@
###en el caso de ajustar con 
# la alternativa "larga", de
# generar columnas, el procedimiento
#es como sigue
df$d2<-df$dap^2
m1b<-lm(wtot~d2,data=df)
m2b<-lm(wfuste~d2,data=df)
m3b<-lm(wramas~d2,data=df)
#compare los parametros estimados
# con los objetos m1, m2 y m3, respectivamente

#El grafico de comportamiento 
# esperado bajo el modelo en este
# caso se realiza como
dap<-5:40
d2<-dap^2

wb.tot<-predict(m1,data.frame(d2))
wb.fus<-predict(m2,data.frame(d2))
wb.ram<-predict(m3,data.frame(d2))

plot(wb.tot~dap,type="l",bty="l")
lines(dap,wb.fus,col="blue")
lines(dap,wb.ram,col="red")

#@@@@@@@@@@@@@@@@@
#eso es todo estimad@s alumn@s
#disfRuten!
#saludos
#C
#@@@@@@@@@@@@@@@@@