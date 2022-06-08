####### Estimador 1


### 1.1

## el estimador propuesto es mean(vec > 1)

### 1.2
estsombrero <- function(datos){
  estimacion <- mean(datos > 1)
  return(estimacion)
}

### 1.3
datos_muestra <- c(12.23,6.37,6.10,0.70,3.48,2.82,9.55,2.21,0.72,9.09)

estsombrero(datos_muestra)
####### Mundo Exponencial

### 1.4
lambda <- 0.2

E <-  1/lambda
V <- 1/lambda**2
P_1 <- 1-pexp(1,lambda)

### 1.5
# lambda_est <- 1/mean(X)
# E_est <- 1/lambda_est
# V_est <- 1/lambda_est**2
# P_1_est <- 1-pexp(1,lambda_est)

### 1.6
# el estimador propuesto es 1-pexp(1,lambda_est)

### 1.7
estrulo <- function(datos){
  lambda_est <- 1/mean(datos)
  estimacion <- 1-pexp(1,lambda_est)
  return(estimacion)
}

### 1.8
estrulo(datos_muestra)

####### Simulacion 1

### 1.9
datos_generados <- rexp(1000,lambda)

tita_verdadero <- 1-pexp(1,lambda)

### 1.10
datos_generados <- rexp(50,lambda)
tita_sombrero <- estsombrero(datos_generados)
tita_rulo <- estrulo(datos_generados)

### 1.11
Nrep <- 1000
estimaciones_tita_sombrero <- rep(NaN,Nrep)
estimaciones_tita_rulo <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  datos_generados <- rexp(50,lambda)
  estimaciones_tita_sombrero[i]<- estsombrero(datos_generados)
  estimaciones_tita_rulo[i] <- estrulo(datos_generados)
}

### 1.12
g1 <- hist(estimaciones_tita_sombrero, xlim = c(0.5,1), ylim = c(0,350), breaks = 20)
g2 <- hist(estimaciones_tita_rulo, xlim = c(0.5,1), ylim = c(0,350), breaks = 20)

# g1 tiene los datos mas dispersos que g2. En ambos graficos los datos estan al rededor del verdadero parametro,
# por lo tanto elijo el estimador del grafico de g2, es decir, el estimador rulo

### 1.13   #### FALTA CONSTRUIR DF
ECME_somb_rul <- function(n, lambda){
  
  estimaciones_tita_sombrero <- rep(NaN, 1000)
  estimaciones_tita_rulo <- rep(NaN, 1000)
  
  titas_verdaderos <- rep(NaN, 1000)
  
  for (i in 1:1000) {
    datos_generados <- rexp(n,lambda)
    titas_verdaderos[i] <- 1-pexp(1,lambda)
    estimaciones_tita_sombrero[i]<- estsombrero(datos_generados)
    estimaciones_tita_rulo[i]<- estrulo(datos_generados)
  }
  
  
  ECME_somb <- sum((estimaciones_tita_sombrero-titas_verdaderos)**2)/1000
  ECME_rul <- sum((estimaciones_tita_rulo-titas_verdaderos)**2)/1000
  
  ECME <- list(ECME_somb = ECME_somb,
               ECME_rul = ECME_rul)
 
  return(ECME)
}

df_ECME <- data.frame(n = rep(c(150,200,500,1000), each =2),
                      tipo = rep(c("sombrero", "rulo"), times = 2, each =2))