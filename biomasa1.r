## Script: "biomasa1.r"                                  /
## Sobre:  ajuste de modelos de biomasa                  /
## y obtencion de predicciones (camino largo)           /
#------------------------------------------------------/ 
##
## Profesor: Christian Salas Eljatib
## E-mail: cseljatib AT gmail DOT com
## Web: https://eljatib.com
#======================================
library(datana)
data(biomass2)
head(biomass2)
df <- biomass2

head(df)

tapply(df$atot, df$spp, length)
help(biomass2)
df$d2 <- df$dap^2h

plot(wtot~dap, data=df)
plot(wtot~d2, data=df)
## ajuste modelo de biomasa del fuste

m.fu <- lm(wfuste ~ d2, data=df)
summary(m.fu)
b0.hat.mfu <- coef(m.fu)[1]
b1.hat.mfu <- coef(m.fu)[2]

d.list <- 5:100
d.list

w.fu.hat <- b0.hat.mfu + b1.hat.mfu*d.list^2

##ajuste modelo de ramas
m.ra <- lm(wramas ~ d2, data=df)
summary(m.ra)
b0.hat.mra<- coef(m.ra)[1]
b1.hat.mra<- coef(m.ra)[2]
w.ra.hat <- b0.hat.mra+b1.hat.mra*d.list^2
#plot(d.list,w.ra.hat, type="l", las=1, col="blue")




###ajuste modelo para el follaje
m.fo <- lm(whojas~d2,data=df)
summary(m.fo)

b0.hat.mfo<-coef(m.fo)[1]
b1.hat.mfo<-coef(m.fo)[2]

w.fo.hat <- b0.hat.mfo + b1.hat.mfo*d.list^2
#plot(d.list,w.fo.hat, type="l", las=1, col="red")

head(data.frame(d.list,w.fu.hat,w.ra.hat,w.fo.hat))

####combinar en un solo grafico
plot(d.list,w.fu.hat, type = "l", las=1, xlab="Diametro (cm)", ylab="Biomasa (kg)")
lines(d.list,w.ra.hat, col="blue")
lines(d.list,w.fo.hat, col="green")


##ejercios Extras
###1. analisis descriptivo de los componentes de biomas, pero en terminos porcentuales
## por ejemplo, el del fuste seria
df$fuste.p<-100*df$wfuste/df$wtot
summary(df$fuste.p)
## Calcular lo anterior para cada componente
##2. Repetir lo anterior, pero por especie

#3. Ajuste los modelos del script, pero por especie.

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profe        ║
#╚═════════════════╝

mean(df$wfuste)
errorporcentaje <- 18.76/mean(df$wfuste)*100
errorporcentaje
help(biomass2)
