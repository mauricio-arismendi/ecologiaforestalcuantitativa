#*==================================================*#
#* Script: "varEstado1.r"                           *#
#* Sobre: Calculo de variables de estado de rodal,  *#
#*  a nivel agregado. El script se organiza como    *#
#*  sigue:                                          *#
#*  1. Calculo de densidad y area basal             *#
#*  2. Calcula de las variables anteriores, pero    *#
#*  segregado por niveles de un factor.             *#
#*                                                  *#
#* Profesor: Christian Salas Eljatib                *#
#* E-mail: cseljatib AT gmail DOT com               *#
#* Web: https://eljatib.com                         *#
#*==================================================*#
#*
#~~~~~~~
#Datos a emplear
library(datana)
data(eucaplot2)
head(eucaplot2)
?eucaplot2
#~~~~~~~
help(eucaplot2)
#asinacion de dataframe a objeto "df"
df<-eucaplot2
summary(eucaplot2)
#==============================
#1) Calculo de variables N y G
#==============================
#-----
#1a. Calculo de la densidad (o "N" segun la iufro)
#i. factor de expansion
df$fe <- 10000/500
head(df)
#ii. la densidad a nivel "de rodal" (N)
nha <- sum(df$fe)
nha #en arb/ha

#-----
#1b. Calculo del area basal (o "G" segun la iufro)
#i. area basal de cada arbol
df$g <- (pi/40000)*df$dap^2
head(df)
summary(df$g)

#ii. area basal de cada arbol expandida a la hectarea
df$garb.ha <- df$g * df$fe
head(df)

#iii. el area basal a nivel de "rodal" (G)
gha <- sum(df$garb.ha)
gha

#=====================
#2) Variables de estado segregadas por niveles de un factor
#=====================
#i. se empleara como factor la clase de copa
unique(df$clase.copa)
table(df$clase.copa)

#-----
#2a. densidad por cada clase de clase de copa
tapply(df$fe,df$clase.copa,sum)
#comparar este resultado con la densidad total
nha

#-----
#2b. area basal por cada clase de clase.copa
tapply(df$garb.ha,df$clase.copa,sum)
#comparar este resultado con el area basal total
gha

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
 df$v <- df$g*df$atot*0.3
 df$varb.ha <- df$v * df$fe
 vha <- sum(df$varb.ha)
 vha
 #m3/ha 
 
df$w <- df$v * 550
df$warb.ha <- df$w * df$fe 
wha <- sum(df$warb.ha)
wha
wha/1000
10000/100
fe
