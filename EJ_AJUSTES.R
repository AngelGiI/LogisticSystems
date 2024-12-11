setwd("C:/Users/Angel/Desktop/Uni/5.Descuento/SLOG/Tema4-R")
datos = read.table("SIMLOG7 datos_ajuste.txt", head=T, dec=",")
names(datos)

u = as.numeric(datos$u)
v = as.numeric(datos$v)
w = as.numeric(datos$w)
x = as.numeric(datos$x)
y = as.numeric(datos$y)
z = as.numeric(datos$z)

#### AJUSTE U ####
hist(u, freq = FALSE)
curve(dbeta(x,1,5), add=TRUE, col="red", lwd = 3)


#### AJUSTE V ####
hist(v, freq = FALSE)
curve(dbeta(x,1,5), add=TRUE, col="red", lwd = 3)


#### AJUSTE W ####
hist(w, freq = FALSE)
curve(dbeta(x,1,5), add=TRUE, col="red", lwd = 3)


#### AJUSTE X ####
hist(x, freq = FALSE)
curve(dbeta(x,1,5), add=TRUE, col="red", lwd = 3)


#### AJUSTE Y ####
hist(y, freq = FALSE)
curve(dbeta(x,1,5), add=TRUE, col="red", lwd = 3)


#### AJUSTE Z ####
hist(z, freq = FALSE)
curve(dbeta(x,1,3), add=TRUE, col="red", lwd = 3)
