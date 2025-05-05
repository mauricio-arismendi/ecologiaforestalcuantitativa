sqrt(250/20)
library(datana)
data(treelistinve2)
?treelistinve2
treelistinve2

summary(treelistinve2)

head(treelistinve2)
df <- subset(treelistinve2, parce %in% c(7, 9, 11))
df
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

nrow(df7)*FE7
nrow(df9)*FE9
nrow(df11)*FE11

G7 <- FE7*((pi*(sum(df7$dap))**2)/40000)
G9 <- FE9*((pi*(sum(df9$dap))**2)/40000)
G11 <- FE11*((pi*(sum(df11$dap))**2)/40000)
G7
G9
G11

dmc7 <- sqrt((sum(df7$dap)**2)/nrow(df7))
dmc9 <- sqrt((sum(df9$dap)**2)/nrow(df9))
dmc11 <- sqrt((sum(df11$dap)**2)/nrow(df11))
dmc7
dmc9
dmc11
df

#na
newdf <- na.omit(df)
newdf

#x
xi <- 1/(sqrt(newdf$dap)+10)
xi
#Y
yi <- log(newdf$atot)
mod<-lm(yi ~ xi, data = newdf )
summary(mod)

df$Yipred <- predict(mod, newdata=df)
df

df$newxi <- 1/(sqrt(df$dap)+10)
df

df$Yipred <- predict(mod, newdata=df)
df


