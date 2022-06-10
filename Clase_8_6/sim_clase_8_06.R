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
# g1
hist(estimaciones_tita_sombrero, xlim = c(0.5,1), ylim = c(0,350), breaks = 20)
# g2
hist(estimaciones_tita_rulo, xlim = c(0.5,1), ylim = c(0,350), breaks = 20)

# g1 tiene los datos mas dispersos que g2. En ambos graficos los datos estan al rededor del verdadero parametro,
# por lo tanto elijo el estimador del grafico de g2, es decir, el estimador rulo

### 1.13   
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

ECME_150 <- ECME_somb_rul(150,0.2)
ECME_200 <- ECME_somb_rul(200,0.2)
ECME_500 <- ECME_somb_rul(500,0.2)
ECME_1000 <- ECME_somb_rul(1000,0.2)

df_ECME <- data.frame(n = rep(c(150,200,500,1000), each =2),
                      tipo = rep(c("sombrero", "rulo"), times = 4),
                      ECME = c(ECME_150[[1]],ECME_150[[2]],
                               ECME_200[[1]],ECME_200[[2]],
                               ECME_500[[1]],ECME_500[[2]],
                               ECME_1000[[1]],ECME_1000[[2]]))

# se prefiere el estimador rulo, ya que tiene un menor error cuadratico en cada
# caso.

####### Mundo normal

mu <- 1/0.2
sigma2 <- 1/0.2**2

# 1.14
P_1_norm <- 1- pnorm(1,mu,sqrt(sigma2))

# 1.15
MuchasEstimacionesTita_n <- function(n){
  
  estimaciones_tita_sombrero <- rep(NaN, n)
  estimaciones_tita_rulo <- rep(NaN, n)
  
  titas_verdaderos <- rep(NaN, n)
  
  for (i in 1:n) {
    datos_generados <- rexp(i,lambda)
    estimaciones_tita_sombrero[i]<- estsombrero(datos_generados)
    estimaciones_tita_rulo[i]<- estrulo(datos_generados)
  }
  
  lista <- c("estimaciones_tita_sombrero" = estimaciones_tita_sombrero,
             "estimaciones_tita_rulo" = estimaciones_tita_rulo)
  
  return(lista)
} 

n <- 5000

limites <- MuchasEstimacionesTita_n(n)

limite_titaSombrero <- rep(NaN, 5000)
limite_titaRulo <- rep(NaN, 5000)

for (i in 1:(length(limites)/2)) {
  limite_titaSombrero[i] <- limites[[i]]
  limite_titaRulo[i] <- limites[[i+n]]
}

plot(1:n, limite_titaSombrero, ylab = "Tita", xlab = "n")
lines(1:n, limite_titaRulo, col="red")

hist(limite_titaSombrero)
hist(limite_titaRulo)

# Respuesta:
mean(limite_titaSombrero)
mean(limite_titaRulo)

### 1.16
tita_asterisco <- function(datos){
  mu_est <- mean(datos)
  sd_est <- sqrt( sum( (datos-mu_est)**2 ) / (length(datos)-1) )
  estimacion <- 1-pnorm(1,mu_est,sd_est)
  return(estimacion)
}

####### Simulacion 2

ECME_sombrero_rulo_asterisco <- function(n, mu, sigma2){
  
  Nrep <- 1000
  
  estimaciones_tita_sombrero <- rep(NaN, Nrep)
  estimaciones_tita_rulo <- rep(NaN, Nrep)
  estimaciones_tita_asterisco <- rep(NaN, Nrep)
  
  titas_verdaderos <- rep(NaN, Nrep)
  
  for (i in 1:Nrep) {
    datos_generados <- rnorm(Nrep, mu, sqrt(sigma2))
    titas_verdaderos[i] <- 1-pnorm(1,mu, sqrt(sigma2))
    estimaciones_tita_sombrero[i]<- estsombrero(datos_generados)
    estimaciones_tita_rulo[i]<- estrulo(datos_generados)
    estimaciones_tita_asterisco[i]<- tita_asterisco(datos_generados)
  }
  
  ECME_somb <- sum((estimaciones_tita_sombrero-titas_verdaderos)**2)/Nrep
  ECME_rul <- sum((estimaciones_tita_rulo-titas_verdaderos)**2)/Nrep
  ECME_ast <- sum((estimaciones_tita_asterisco-titas_verdaderos)**2)/Nrep
  
  ECME <- list(ECME_somb = ECME_somb,
               ECME_rul = ECME_rul,
               ECME_ast = ECME_ast)
  
  return(ECME)
}

ECME_150 <- ECME_sombrero_rulo_asterisco(150,mu, sigma2)
ECME_200 <- ECME_sombrero_rulo_asterisco(200,mu, sigma2)
ECME_500 <- ECME_sombrero_rulo_asterisco(500,mu, sigma2)
ECME_1000 <- ECME_sombrero_rulo_asterisco(1000,mu, sigma2)

df_ECME <- data.frame(n = rep(c(150,200,500,1000), each =3),
                      tipo = rep(c("sombrero", "rulo", "asterisco"), times = 4),
                      ECME = c(ECME_150[[1]],ECME_150[[2]], ECME_150[[3]],
                               ECME_200[[1]],ECME_200[[2]], ECME_200[[3]],
                               ECME_500[[1]],ECME_500[[2]], ECME_500[[3]],
                               ECME_1000[[1]],ECME_1000[[2]], ECME_1000[[3]]))

# El estimador que eligiria en este caso es el estimador asterisco.
# Es interesante notar que el segundo que eligiria, es el no parametrico.
# Ya que no asume una distribucion determinada de los datos. 
# El estimador rulo, que antes era el mejor estimador (cuando los datos
# venian de una distribucion exponencial) es ahora el peor estimador.

library(ggplot2)

ggplot(df_ECME, aes(fill= as.factor(n), y= ECME, x=tipo)) + 
  geom_bar(position="dodge", stat="identity")