library(datana)
data(radiatapl2)
help(radiatapl2)
radiatapl2
?radiatapl2

FE = 10000/150
FE

df <- subset(radiatapl2, parce %in% c(1, 3, 4))
df

df1 <- subset(radiatapl2, radiatapl2$parce == 1)
df1

nrow

df3 <- subset(radiatapl2, radiatapl2$parce == 3)
df3

df4 <- subset(radiatapl2, radiatapl2$parce == 4)
df4

#Pregunta 1

N1 <- FE*nrow(df1)
N3 <- FE*nrow(df3)
N4 <- FE*nrow(df4)

N1
N3
N4

#Pregunta 2

G1 <- FE*((pi*(sum(df1$dap))**2)/40000)
G3 <- FE*((pi*(sum(df3$dap))**2)/40000)
G4 <- FE*((pi*(sum(df4$dap))**2)/40000)

G1
G3
G4

#Pregunta 3

dmc1 <- sqrt((sum(df1$dap)**2)/nrow(df1))
dmc1
dmc3 <- sqrt((sum(df3$dap)**2)/nrow(df1))
dmc4 <- sqrt((sum(df4$dap)**2)/nrow(df1))
dmc3
dmc4

#Pregunta 4

#sacar na
newdf <- na.omit(df)
newdf
#var Y
Yi <- 1/(newdf$atot-1.3)
Yi

#var X
Xi <- 1/(newdf$dap**2)
Xi

newdf
mod<-lm(Yi ~ Xi, data = newdf )
mod
summary(mod)
b0est<-coef(mod)[1]
b1est<-coef(mod)[2]
b0est
b1est

#Pregunta 5

newdf$h.aju <- predict(mod, newdata=newdf)
newdf
summary(mod)
newdf$hi_hat <- (1/newdf$h.aju)+1.3
newdf$hi_hat

ei <- newdf$atot - newdf$hi_hat
ei
mean(ei)
abs_ei <- abs(ei)
abs_ei
mean(abs_ei)

sqrt((sum(ei)**2)/26)
rmse <- sqrt(mean(ei^2, na.rm = TRUE))
rmse
sum(ei**2)/26
mean(ei**2)

newdf
#Pregunta 6
na_rows <- is.na(df$atot)
na_rows

df$arboles_na <- df[na_rows,]
df$arboles_na

df$arboles_medidos <- df[!na_rows,]
df$arboles_medidos

arboles_na$Xi <- 1/(arboles_na$dap**2)
arboles_na

arboles_na$Yi_predicho <- predict(mod, newdata = arboles_na)
arboles_na

#atot predicha
arboles_na$atot_pred <- (1/arboles_na$Yi_predicho)+1.3
arboles_na

#creé una nueva columna pa poder meter la final
df$atot_pred <- NA
df
#le meto las alturas medidas a esa columna
if(nrow(arboles_medidos) >0){
  df$atot_pred[!na_rows] <- arboles_medidos$atot
}

#como va a estar vacío de alturas predichas, se las meto
  df$atot_pred[na_rows] <- arboles_na$atot_pred
df

mean(df$atot_pred)

#13.55 altura total (medidos + predichos)

#Pregunta 7
df1 <- subset(df, df$parce == 1)
df1

df3 <- subset(df, parce == 3)
df3

df4 <- subset(df, df$parce == 4)
df4
head(df1)
df1$vol1 <- 0.005 + 0.00003151*((df1$dap**2)*df1$atot_pred)
df3$vol3 = 0.005 + 0.00003151*((df3$dap**2)*df3$atot_pred)
df4$vol4 = 0.005 + 0.00003151*((df4$dap**2)*df4$atot_pred)
df1

volumen1 <- FE*sum(df1$vol1)
volmen3 <- FE*sum(df3$vol3)
volumen4 <- FE*sum(df4$vol4)

volumen1
volmen3
volumen4
########127.36 metros cubicos /ha
####279.85 metros cubicos /ha 

## 276.68 metros cubicos /ha