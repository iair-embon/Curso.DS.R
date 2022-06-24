# creo una funcion para sacar la acumulada hasta t
empirica_datos <- function(datos, t){
  f_t <- mean(datos <= t)
  return(f_t)
}

# tengo los datos
datos <- c(0.57,0.93,0.81,0.44,0.36,0.52,0.65,0.69,0.82,0.12)

# pruebo con una t en 1/2
t <- 1/2
empirica_datos(datos,t)

# grafico
varias_t <- seq(-1,1,0.01)
varias_f_t <- rep(NaN, length(varias_t))

for (i in 1:length(varias_f_t)) {
 varias_f_t[i] <- empirica_datos(datos, varias_t[i]) 
}

plot(varias_f_t, type = "l")
lines(rep(empirica_datos(datos,t),length(varias_f_t)), col= "green")
points(empirica_datos(datos,t), cex = .5, col = "dark red")

# al final salian de una uniforme(0,1) estos datos

# comando de r para hacer esto 
plot(ecdf(datos))

###### Practica de la guia de la clase

datos_2 <-scan()
126.4 82.4 78.1 51.1 90.9 76.2 104.5 87.4 110.5 25.0 69.3 53.5
39.8 63.6 46.7 72.9 79.6 83.6 80.7 60.3 79.0 74.4 49.6 54.7
71.8 49.1 103.9 51.6 82.4 83.6 77.8 79.3 89.6 85.5 58.0 120.7
110.5 65.4 39.9 40.1 88.7 71.4 83.0 55.9 89.9 84.8 105.2 113.7
124.7 114.5 115.6 102.4 101.4 89.8 71.5 70.9 98.3 55.5 66.1 78.4
120.5 97.0 110.0

### 1 
hist(datos_2)
hist(datos_2, breaks =  seq(20, 140, 10))
hist(datos_2, breaks =  seq(20, 140, 5))

# la eleccion de bins modula el refinamiento/suavidad

### 2
cortes <- seq(10,130,10)
hist(datos_2, breaks =  cortes)
cortes_mov <- cortes+2
hist(datos_2, breaks =  cortes_mov)
cortes_mov2 <- cortes_mov + 2
hist(datos_2, breaks =  cortes_mov2)

# los histogramas cambian bastante segun donde empiezan

### 3
probab.est <- function(x,dat,h){
  estimacion <- sum(dat < (x+h) & dat > (x-h))/ (2*h*length(dat)) 
  return(estimacion)
}

prueba_1 <- probab.est(40,datos_2,10)
prueba_2 <- probab.est(40,datos_2,20)

# cambia apenas

### 4

varias_estimaciones_h10 <- rep(NaN, length(datos_2))

for (i in 1:length(datos_2)) {
  varias_estimaciones_h10[i] <- probab.est(datos_2[i],datos_2, 10)
}

varias_estimaciones_h20 <- rep(NaN, length(datos_2))

for (i in 1:length(datos_2)) {
  varias_estimaciones_h20[i] <- probab.est(datos_2[i],datos_2, 20)
}

varias_estimaciones_h30 <- rep(NaN, length(datos_2))

for (i in 1:length(datos_2)) {
  varias_estimaciones_h30[i] <- probab.est(datos_2[i],datos_2, 30)
}

