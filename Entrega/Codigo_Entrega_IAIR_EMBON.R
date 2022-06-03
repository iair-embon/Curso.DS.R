
## n = 50

n <- 50

Nrep <- 1000

muchos_cuantiles_exponenciales_sombrero <- rep(NaN,Nrep)
muchos_cuantiles_exponenciales_rulo <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  muchas_exponenciales <- rexp(n, 1/10)
  
  # sombrero
  qsombrero <- quantile(muchas_exponenciales,0.9)
  muchos_cuantiles_exponenciales_sombrero[i] <- qsombrero
  
  # rulo
  lamda_est <- 1/mean(muchas_exponenciales)
  qrulo <- (-log(1-0.9))/lamda_est
  muchos_cuantiles_exponenciales_rulo[i] <- qrulo
}

qreal <- (-log(1-0.9))/(1/10)

ecme_sombrero_n50 <- sum((muchos_cuantiles_exponenciales_sombrero-  qreal)**2) / length(muchos_cuantiles_exponenciales_sombrero)
ecme_rulo_n50 <- sum((muchos_cuantiles_exponenciales_rulo- qreal)**2) / length(muchos_cuantiles_exponenciales_rulo) 

## n = 150

n <- 150

Nrep <- 1000

muchos_cuantiles_exponenciales_sombrero <- rep(NaN,Nrep)
muchos_cuantiles_exponenciales_rulo <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  muchas_exponenciales <- rexp(n, 1/10)
  
  # sombrero
  qsombrero <- quantile(muchas_exponenciales,0.9)
  muchos_cuantiles_exponenciales_sombrero[i] <- qsombrero
  
  # rulo
  lamda_est <- 1/mean(muchas_exponenciales)
  qrulo <- (-log(1-0.9))/lamda_est
  muchos_cuantiles_exponenciales_rulo[i] <- qrulo
}

qreal <- (-log(1-0.9))/(1/10)

ecme_sombrero_n150 <- sum((muchos_cuantiles_exponenciales_sombrero-  qreal)**2) / length(muchos_cuantiles_exponenciales_sombrero)
ecme_rulo_n150 <- sum((muchos_cuantiles_exponenciales_rulo- qreal)**2) / length(muchos_cuantiles_exponenciales_rulo) 

## n = 200

n <- 200

Nrep <- 1000

muchos_cuantiles_exponenciales_sombrero <- rep(NaN,Nrep)
muchos_cuantiles_exponenciales_rulo <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  muchas_exponenciales <- rexp(n, 1/10)
  
  # sombrero
  qsombrero <- quantile(muchas_exponenciales,0.9)
  muchos_cuantiles_exponenciales_sombrero[i] <- qsombrero
  
  # rulo
  lamda_est <- 1/mean(muchas_exponenciales)
  qrulo <- (-log(1-0.9))/lamda_est
  muchos_cuantiles_exponenciales_rulo[i] <- qrulo
}

qreal <- (-log(1-0.9))/(1/10)

ecme_sombrero_n200 <- sum((muchos_cuantiles_exponenciales_sombrero-  qreal)**2) / length(muchos_cuantiles_exponenciales_sombrero)
ecme_rulo_n200 <- sum((muchos_cuantiles_exponenciales_rulo- qreal)**2) / length(muchos_cuantiles_exponenciales_rulo) 

## n = 500

n <- 500

Nrep <- 1000

muchos_cuantiles_exponenciales_sombrero <- rep(NaN,Nrep)
muchos_cuantiles_exponenciales_rulo <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  muchas_exponenciales <- rexp(n, 1/10)
  
  # sombrero
  qsombrero <- quantile(muchas_exponenciales,0.9)
  muchos_cuantiles_exponenciales_sombrero[i] <- qsombrero
  
  # rulo
  lamda_est <- 1/mean(muchas_exponenciales)
  qrulo <- (-log(1-0.9))/lamda_est
  muchos_cuantiles_exponenciales_rulo[i] <- qrulo
}

qreal <- (-log(1-0.9))/(1/10)

ecme_sombrero_n500 <- sum((muchos_cuantiles_exponenciales_sombrero-  qreal)**2) / length(muchos_cuantiles_exponenciales_sombrero)
ecme_rulo_n500 <- sum((muchos_cuantiles_exponenciales_rulo- qreal)**2) / length(muchos_cuantiles_exponenciales_rulo) 


# hago una tabla con los resultados.

library(dplyr)
library(janitor)

df.ECME <- data.frame(estimador = c("rulo","sombrero"),
                      n50 = c(ecme_rulo_n50,ecme_sombrero_n50),
                      n150= c(ecme_rulo_n150,ecme_sombrero_n150),
                      n200= c(ecme_rulo_n200,ecme_sombrero_n200),
                      n500= c(ecme_rulo_n500,ecme_sombrero_n500))

print(df.ECME)

# Se prefiere el estimador rulo, ya que en cada estimacion tuvo un menor error
# cuadratico medio empirico.