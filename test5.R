
##Pregunta 1 

library(datana)
data(treelistinve2)
?treelistinve2
treelistinve2

summary(treelistinve2)

head(treelistinve2)
df <- subset(treelistinve2, parce %in% c(7, 9, 11))
df
#útil para saber qué valores diferentes hay en una columna
unique(treelistinve2$sup.parce)

df7 <- subset(df, df$parce == 7)
df7

df9 <- subset(df, df$parce == 9)
df9

df11 <- subset(df, df$parce == 11)
df11

FE7 <- 10000/500
FE9 <- 10000/500
FE11 <- 10000/1000

df7$fe <- FE7
df9$fe <- FE9
df11$fe <- FE11

nha7 <- sum(df7$fe)
nha7

nha9 <- sum(df9$fe)
nha9

arbdf7<-nrow(df7)
arbdf9 <-nrow(df9)
arbdf11<-nrow(df11)

nrow(df7)*FE7
nrow(df9)*FE9
nrow(df11)*FE11
df$fe <- 10000/df$sup.parce
####Parcela 7  -> 900 árboles/ha
####Parcela 9  -> 1360 árboles/ha
####Parcela 11 -> 1190 árboles/ha


df$g <- (pi/40000)*df$dap^2
df$g
#ii. area basal de cada arbol expandida a la hectarea
df$garb.ha <- df$g * df$fe
#iii. area basal por parcela
gha.ppar<-tapply(df$garb.ha,df$parce,sum)
gha.ppar


####Parcela 7  -> 52.17 m^2/ha
####Parcela 9  -> 67.58 m^2/ha
####Parcela 11 -> 46.58 m^2/ha

#Pregunta 2

#na
newdf <- na.omit(df)
newdf

#x
newdf$xi <- 1/(sqrt(newdf$dap)+10)
newdf$xi
#Y
newdf$yi <- log(newdf$atot)
mod<-lm(yi ~ xi, data = newdf )
summary(mod)

####beta0 = 7.27 
####beta1 = -63.392
####error = 0.2264




#Preg 3
#df$Yi.aju <- predict(mod, newdata=newdf)
#newdf$hi.aju <- exp(newdf$Yi.aju)

#newdf
#nrow(newdf)

df$xi <- 1/(sqrt(df$dap)+10)
df

df$Yiaju <- predict(mod, newdata = df)
df

df$hiaju <- exp(df$Yiaju)

df
#con ifelse cambio los datos de na en atot

df$atot <- ifelse(
  is.na(df$atot),
  df$hiaju,
  df$atot
  )

df$atot
df$parce
df7 <- subset(df, df$parce == 7)
df7

df9 <- subset(df, df$parce == 9)
df11 <- subset(df, df$parce == 11)

#buena practica sería revisar si hay algun dato inconsistente
df7$atot
df9$atot
df11$atot

mean(df7$atot)
mean(df9$atot)
mean(df11$atot)


## Altura media parce (otra forma)
summary(df$atot)
hmed.ppar<-tapply(df$atot,df$parce,mean)
hmed.ppar

### Parcela 7 = 19.88 m
#### Parcela 9 = 19.87 m
##### Parcela 11 = 18.28 m


# Pregunta 4
ff = 0.3

#parcela 7
df7
df7Ro <- subset(df7, df7$spp == "Roble")
df7Ro

df7Ro$vol <- exp(-9.918535 + 0.95733 * log((df7Ro$dap**2)*df7Ro$atot))
df7Ro
nrow(df7Ro)
df7Oli <- subset(df7, df7$spp == "Olivillo")
df7Oli$vol   <- 0.00462 + 0.00003238*((df7Oli$dap**2)*df7Oli$atot)
df7Oli
nrow(df7Oli)
df7Otros <- subset(df7, spp != "Olivillo" & spp != "Roble")
df7Otros
nrow(df7Otros)
df7Otros$vol <- (df7Otros$atot * ((df7Otros$dap/100)**2) * ff * pi)/4
df7Otros



#parcela 9
df9
df9Ro <- subset(df9, df9$spp == "Roble")
df9Ro

df9Ro$vol <- exp(-9.918535 + 0.95733 * log((df9Ro$dap**2)*df9Ro$atot))
df9Ro

nrow(df9Ro)
df9Oli <- subset(df9, df9$spp == "Olivillo")
df9Oli$vol   <- 0.00462 + 0.00003238*((df9Oli$dap**2)*df9Oli$atot)
df9Oli
nrow(df9Oli)

df9Otros <- subset(df9, spp != "Olivillo" & spp != "Roble")
df9Otros

nrow(df9Otros)
df9Otros$vol <- (df9Otros$atot * ((df9Otros$dap/100)**2) * ff * pi)/4
df9Otros



#parcela 11
df11
df11Ro <- subset(df11, df11$spp == "Roble")
df11Ro

df11Ro$vol <- exp(-9.918535 + 0.95733 * log((df11Ro$dap**2)*df11Ro$atot))
df11Ro

nrow(df11Ro)
df11Oli <- subset(df11, df11$spp == "Olivillo")
df11Oli$vol   <- 0.00462 + 0.00003238*((df11Oli$dap**2)*df11Oli$atot)
df11Oli

nrow(df11Oli)
df11Otros <- subset(df11, spp != "Olivillo" & spp != "Roble")
df11Otros
nrow(df11Otros)
df11Otros$vol <- (df11Otros$atot * ((df11Otros$dap/100)**2) * ff * pi)/4
df11Otros

dvol <- rbind(df7Ro, df7Oli, df7Otros, df9Ro, df9Oli, df9Otros, df11Ro, df11Oli, df11Otros)
nrow(dvol)
dvol$varb.ha <- dvol$vol * dvol$fe

vha.ppar<-tapply(dvol$varb.ha,dvol$parce,sum)
vha.ppar
dvol$varb.ha

#Pregunta 5 y 6

#volmed


df$fe <- 10000/df$sup.parce
df$fe
w.amp = 10

df$clase.d<-(as.integer((df$dap+((w.amp/2)-0.1))/w.amp))*w.amp
unique(df$clase.d)
sort(unique(df$clase.d))
df

trodal<-tapply(df$fe, df$clase.d, sum)
trodal

##pal rodal
n.parcelas<-length(unique(df$parce))

n.parcelas
df$fe.muestreo <- df$fe/n.parcelas
df$garb.muestreo <- df$g * df$fe.muestreo
df

trodal<-tapply(df$fe.muestreo, df$clase.d, sum)
trodal


nha.cd<-trodal
clase.d<-as.numeric(rownames(trodal)) 
tab1<-data.frame(clase.d,nha.cd)
tab1


nha.ppar<-tapply(df$fe,df$parce,sum)
nha.ppar

N<-nha.ppar;G<-gha.ppar;
Dg<-sqrt((G/N)*(40000/pi))
varest.ppar<-data.frame(N,G,Dg)
varest.ppar

tab.out<-varest.ppar
row.names(tab.out)
Parcela <- row.names(tab.out)
Parcela
tab.out<-cbind(Parcela,varest.ppar)
tab.out
descstat(tab.out[,c("N","G","Dg")],3)

df$g <- (pi/40000)*df$dap^2
df$g

df$garb.ha <- df$g * df$fe
df$garb.ha

gha.ppar<-tapply(df$garb.ha,df$parce,sum)
gha.ppar


nha.med <- mean(tab.out$N);nha.med;
gha.med <- mean(tab.out$G);gha.med;
dg.med <- mean(tab.out$Dg);dg.med


#variables de estado de rodal por parcela
Hm<-hmed.ppar;V<-vha.ppar
tab.out2<-cbind(tab.out,Hm,V)
tab.out2

h.med <- mean(tab.out2$Hm)
h.med
vha.med <- mean(tab.out2$V)
vha.med

hmed.cd<-tapply(df$atot, df$clase.d, mean)
hmed.cd

dvol$fe.muestreo <- dvol$fe/n.parcelas 

dvol$varb.muestreo <- dvol$vol * dvol$fe.muestreo
vha.cd<-tapply(dvol$varb.muestreo, df$clase.d, sum)

#V
vha.cd
sum(vha.cd)

##G 
gha.cd<-tapply(df$garb.muestreo, df$clase.d, sum)
gha.cd

##Pregunta 1/G|Dg
N<-nha.ppar;G<-gha.ppar;
G
Dg<-sqrt((G/N)*(40000/pi))
varest.ppar<-data.frame(N,G,Dg)
varest.ppar
