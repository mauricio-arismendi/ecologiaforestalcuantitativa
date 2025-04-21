##Tareas

##Tarea sugerida:  altura2.r 
## 1. escriba (en una hoja) los parametros estimados de cada modelo.
## 2. revise la inferencia estadistica respecto a los coeficientes estimados.
## 3. compare ambos modelos, basado en el grafico de comportamiento y
##    los puntos anteriores.

##1
summary(mod1)
summary(mod2)
mod2
df$inv.d

##2
df
descstat(df[,c("atot", "dap","aju","e.aju","h.aju1","h.ajumod2", "e.aju1")])
df$h.ajumod2 <- h.ajumod2
h.ajumod2

##3.
descstat(df[,c("atot", "dap","aju","e.aju","h.aju1","h.ajumod2", "e.aju1")])
df$h.ajumod2 <- h.ajumod2
h.ajumod2

#modelo 1 varía más. (según cv)



#altura3.r
## 1. calcule los estadisticos de validacion para los modelos 1, 2 y 3

mean(h.ajumod1)
mean(h.ajumod2) 
mean(h.ajumod3)  
mean(df$atot)

# observando las medias aritméticas, se observa que el modelo que más se acerca 
# a la media observada es el modelo 2, PERO, hay que observar también
#otros estadísticos que brinden certeza, los que se harán en el ejercicio siguiente.

  ## 2. prepare un cuadro en una hoja a mano, y escriba los estadisticos de
##    validacion para cada modelo (cada fila un modelo).
#listoko

## 3. compare los modelos, basado en los estadisticos de
##    validacion calculados
## flojera, pero se observa en la gráfica que el modelo 3 es mejor. 



##altura4.r
##Tarea sugerida:
## 1. realice un grafico entre el error de prediccion y el valor observado
plot(e.aju~atot, data=df, las=1, col="gray")
## 2. realice un grafico entre el error de prediccion y el valor ajustado
plot(e.aju~aju, data=df, las=1, col="blue")
## 3. realice un grafico entre el error de prediccion y el diametro
plot(e.aju~dap, data=df, las=1, col="red")

##altura5.r

##Tarea sugerida:
## 1. prepare un cuadro en una hoja a mano, y escriba los estadisticos de
##    validacion para cada modelo (cada fila un modelo).

100*ad.mod1/mean.h
100*rmsd.mod1/mean.h
100*aad.mod1/mean.h
##modelo 2
100*ad.mod2/mean.h
100*rmsd.mod2/mean.h
100*aad.mod2/mean.h



## 3. compare ambos modelos, basado en los estadisticos de
##    validacion calculados.
#Mod 1 es mejor 


