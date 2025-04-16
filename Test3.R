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
newJaimito$x <-log(1/(newJaimito$dap+10))
newJaimito$x
#creo var y
newJaimito$h <-sqrt(newJaimito$atot)
newJaimito$h
#grafica solo pa ver
plot(newJaimito$x~newJaimito$h, data=newJaimito)



##sqrt(h)=(b0+b1*ln(1/x+10))

##4 Ajuste el modelo estadístico dado. 
##-parámetro "xxx" = "número XXX"

mod <- lm(newJaimito$h ~ newJaimito$x, data = newJaimito)
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

jaimito$atot



##7 Usando el residual en unidades orignales de la variable (es decir ê=hi-^hi)
##donde hi es altura observada para la i-esima observacion y ^hi es la altura predicha x el modelo ajustado
## calcular el estadistico de prediccion, diferencia media absoluta (DAA) que esta def como:

## DAA = 1/n sum(abs(êi))

##8 Estimar biomasa aérea del bosque en ton/ha. con el sgte modelo ajustado:



##donde wi, di, hi, es la biomasa en kg, el dap (en cm), h (en m) para el i-esimo arbol, respectivamente
a0 = 5.44174; a1=0.0257348;a2 = 0.031996 
a0
a1
a2
wi = a0 + a1*(jaimito$dap) + a2*(jaimito$atot)
witotal <- exp(wi)
sum(witotal, na.rm = T)
5081.692*fc
witha <- fc*sum(witotal, na.rm = TRUE)   
witha  
##me da 338779.5 kg 


##h <- (b0 + b1*ln(1/x+10))**2


#sqrt(h)=(b0+b1*ln(1/x+10))
