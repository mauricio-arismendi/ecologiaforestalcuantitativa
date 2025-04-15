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


#creo var x
jaimito$x <-log(1/(jaimito$dap+10))
jaimito$x
#creo var y
jaimito$h <-sqrt(jaimito$atot)
jaimito$h

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


##4 Ajuste el modelo estadístico dado. 
##-parámetro "xxx" = "número XXX"


##5 Asumiendo arbol dap 13 cm
##y empleando el modelo ajustado, calcular valor predicho para altura en m

##6 Empleando el modelo ajustado predecir altura arboles de parcela a los q no se ha medido
#esa variable. Una vez q todos los arboles de la parcela tengan un val de altura, calcular 
## altura media (en m) de todos los arboles de la parcela

##7 Usando el residual en unidades orignales de la variable (es decir ê=hi-^hi)
##donde hi es altura observada para la i-esima observacion y ^hi es la altura predicha x el modelo ajustado
## calcular el estadistico de prediccion, diferencia media absoluta (DAA) que esta def como:

## DAA = 1/n sum(abs(êi))

##8 Estimar biomasa aérea del bosque en ton/ha. con el sgte modelo ajustado:



##donde wi, di, hi, es la biomasa en kg, el dap (en cm), h (en m) para el i-esimo arbol, respectivamente
a0 = 5.44174; a1=0.0257348;a2 = 0.031996 

wi = exp(a0 + a1*(jaimito$dap*10) + a2*(jaimito$atot))
sum(wi, na.rm = TRUE)   # Suma ignorando NA



a0

jaimito

##y <- (b0 + b1*ln(1/x+10))**2


#sqrt(h)=(b0+b1*ln(1/x+10))

##creando la variable Y necesaria para el modelo 3
jaimito$sq.h<-sqrt(jaimito$dap)

##creando la variable X necesaria para el modelo 3
jaimito$ln.d<- log(1/jaimito$dap+10)

plot(sq.h~ln.d, data=df)

descstat(df[,c("dap","exp.d","atot","ln.h")])
mod3<- lm(ln.h~exp.d, data=df)
summary(mod3)
b0.hat3<-coef(mod3)[1]
b1.hat3<-coef(mod3)[2]
b0.hat3
b1.hat3
h.ajumod3 <- exp(b0.hat3 + b1.hat3 * exp(-0.03*d.fake))


data <- jaimito

modelo_lin <- lm(sqrt(h) ~ I(log(1/(x + 10))), data = data)


modelo_nls <- nls(sqrt(h) ~ b0 + b1*log(1/(x + 10)),
                  data = data,
                  start = list(b0 = 1, b1 = 1))  # Valores iniciales


