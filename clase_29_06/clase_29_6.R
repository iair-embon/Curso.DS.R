datos <- read.table("Escritorio/lidar.txt", header = TRUE)

### 1
plot(datos$range,datos$int.conc)

# parece una relacion sigmoide

### 2
FunRegEstimada1 <- ksmooth(datos$range,datos$int.conc, kernel = "normal", bandwidth = 5)
FunRegEstimada2 <- ksmooth(datos$range,datos$int.conc, kernel = "normal", bandwidth = 10)
FunRegEstimada3 <- ksmooth(datos$range,datos$int.conc, kernel = "normal", bandwidth = 30)
FunRegEstimada4 <- ksmooth(datos$range,datos$int.conc, kernel = "normal", bandwidth = 50)


plot(FunRegEstimada1$x, FunRegEstimada1$y)
lines(FunRegEstimada2$x, FunRegEstimada2$y, col = "red")
lines(FunRegEstimada3$x, FunRegEstimada3$y, col = "blue")
lines(FunRegEstimada4$x, FunRegEstimada4$y, col = "green")

### 3

ECPP <- function(y , y_new){
  error <- mean((y-y_new)**2)
  return(error)
}

ECPP_1 <- ECPP(datos$int.conc, FunRegEstimada1$y)
ECPP_2 <- ECPP(datos$int.conc, FunRegEstimada2$y)
ECPP_3 <- ECPP(datos$int.conc, FunRegEstimada3$y)
ECPP_4 <- ECPP(datos$int.conc, FunRegEstimada4$y)

# la ventana que presenta un menor ECPP es la de h=5, mientras mas chica la h
# va a tener un menor error, dado a que estamos midiendo el error en los mismos datos

### 4 

# preparo a los datos para el crossvalidation
library(dplyr)

datos$id <- 1:nrow(datos)

datos_testeo <- datos %>%
  slice_sample(prop = 0.10)

datos_entrenamiento_validacion <- datos %>%
  filter(!id %in% datos_testeo$id) %>%
  slice_sample(prop = 0.20)

datos_entrenamiento <- datos %>%
  filter(!id %in% datos_entrenamiento_validacion$id & !id %in% datos_testeo$id)

grilla <- seq(3,165,by = 1)
ECPP_varias_h <- rep(NaN, length(grilla))

for (i in 1:length(grilla)) {
  FunRegEstimada <- ksmooth(datos_entrenamiento$range,
                            datos_entrenamiento$int.conc, 
                            kernel = "normal",
                            bandwidth = grilla[i]) 
  # x.points validacion  corregir!
  ECPP_varias_h[i] <- ECPP(datos_entrenamiento$int.conc, FunRegEstimada$y)
}


