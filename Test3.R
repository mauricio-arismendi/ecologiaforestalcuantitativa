##Test 3

library(datana
        )

data(radiatapl2)
radiatapl2
summary(radiatapl2)
?radiatapl2

fc <- 10000/150 
fc

radiatapl2



dens <- summary.factor(radiatapl2$parce) * fc
sum(dens)

fc

#subset pa trabajar con parcela 1
radiatapl2
jaimito <- subset(radiatapl2, radiatapl2$parce == 1)
jaimito


radiatapl2
plot(radiatapl2$dap )

#sacamos los NA 
newJaimito <-  na.omit(jaimito)
newJaimito

#creo var x
jaimito$x <-log(1/(jaimito$dap+10))
jaimito$x
#creo var y
jaimito$h <-sqrt(jaimito$atot)
jaimito$h
#grafica solo pa ver
plot(newJaimito$x~newJaimito$h, data=newJaimito)



##sqrt(h)=(b0+b1*ln(1/x+10))

##4 Ajuste el modelo estadístico dado. 
##-parámetro "xxx" = "número XXX"
jaimito
newJaimito
mod <- lm(jaimito$h ~ jaimito$x, data = jaimito)
mod
summary(mod)
b0est<-coef(mod)[1]
b1est<-coef(mod)[2]
b0est
b1est
#parametro b0 = 0.2208
#parametro b1 = -1.0452 
#error =  0.1129 


##5 Asumiendo arbol dap 13 cm
##y empleando el modelo ajustado, calcular valor predicho para altura en m

h=(0.2207931 + -1.045165 * log(1/23))**2
h

#12.23532 metros


##6 Empleando el modelo ajustado predecir altura arboles de parcela a los q no se ha medido
#esa variable. Una vez q todos los arboles de la parcela tengan un val de altura, calcular 
## altura media (en m) de todos los arboles de la parcela

jaimito
newJaimito
library(dplyr)

##modelo dendrometrico: conservar las alturas medidas, y solo
# predecir a las observaciones no medidas.
#como logramos lo anterior?
jaimito$h.aju <- predict(mod, newdata = jaimito)
mod
summary(jaimito$h.aju)
jaimito$h.final <- jaimito$h.aju

#porcion de arboles donde no se midio la altura
jaimitona <- jaimito[is.na(jaimito$atot),]
jaimitona
jaimitona$h.final <- jaimitona$atot
dim(jaimitona)

#porcion de arboles donde si se midio la altura
jaimitosi <- jaimito[!is.na(jaimito$atot),]
jaimitosi
dim(jaimitosi)
jaimitosi$h.final <- jaimitosi$atot
jaimitosi

#finalmente tenemos el tree list o listado de arboles con todas las variables
# necesarias
trl <- rbind(jaimitona,jaimitosi)
dim(trl)

trl**2

mean(trl$h.aju**2) 
##################### 12.49

?is.na


##7 Usando el residual en unidades orignales de la variable (es decir ê=hi-^hi)
##donde hi es altura observada para la i-esima observacion y ^hi es la altura predicha x el modelo ajustado
## calcular el estadistico de prediccion, diferencia media absoluta (DAA) que esta def como:

## DAA = 1/n sum(abs(êi))

mod
summary(mod)

0.1129/8
jaimitoobservado <- jaimito[!is.na(jaimito$atot),]
jaimitoobservado

jaimitopredicho <- (jaimitoobservado$h.aju)**2
jaimitopredicho

e <- jaimitoobservado$atot - jaimito$h.final

nrow(jaimitoobservado)

daa <- (1/nrow(jaimitoobservado))* abs(sum((jaimitoobservado$atot) - sum(jaimitopredicho)))
sum(jaimitoobservado$atot)
sum(jaimitopredicho)
127.2-127.0981
0.1019*0.1
1/10
##8 Estimar biomasa aérea del bosque en ton/ha. con el sgte modelo ajustado:
0.01019-100
jaimitoobservado
jaimitopredicho

##donde wi, di, hi, es la biomasa en kg, el dap (en cm), h (en m) para el i-esimo arbol, respectivamente
a0 = 5.44174; a1=0.0257348;a2 = 0.031996 
a0
a1
a2
wi = a0 + a1*(jaimito$dap) + a2*(jaimito$h.aju**2)
wi
witotal <- exp(wi)

sum(witotal, na.rm = T)*fc

witha <- fc*sum(wi, na.rm = TRUE)   
witha
##me da 724.809 ton/ha 

jaimito$h.aju <- predict(mod, newdata = jaimito)
summary(jaimito$h.aju)
jaimito$h.final <- jaimito$h.aju

#porcion de arboles donde no se midio la altura
modelitow <- modelitow[is.na(jaimito$atot),]
jaimitona
jaimitona$h.final <- jaimitona$atot
dim(jaimitona)

#porcion de arboles donde si se midio la altura
jaimitosi <- jaimito[!is.na(jaimito$atot),]
dim(jaimitosi)
jaimitosi$h.final <- jaimitosi$atot
jaimitosi

#finalmente tenemos el tree list o listado de arboles con todas las variables
# necesarias
trl <- rbind(jaimitona,jaimitosi)
dim(trl)

trl**2

mean(trl$h.aju**2) 

