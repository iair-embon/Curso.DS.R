##############
# Entrega 2  #
# iair embon #
##############

# Leo los datos
df <- read.table("http://astrostatistics.psu.edu/datasets/GRB_afterglow.dat", 
                 header=T, skip=1)

require(dplyr)

### 1

## P(X<=40)
p_40 <- df %>%
  filter(f <= 40) %>%
  nrow()/length(df$f)

## standar error

p_boot <- rep(NaN, 10000)

for (i in 1:length(p_boot)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  p_boot[i] <-length(x_sorteados[x_sorteados <= 40])/ length(x_sorteados)
}

es_p_muestra <- sd(p_boot)


### 2 ??? se puede esto? o tengo que usar boostrap tambien?
prom_muestra <- mean(df$f)
es_media_muestra <- sd(df$f)/ sqrt(nrow(df))

### 3

# estimo la mediana

mediana_muestra <- median(df$f)

# estimo el error estandar de la estimacion de la mediana

mediana_boot <- rep(NaN, 10000)

for (i in 1:length(mediana_boot)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  mediana_boot[i] <- median(x_sorteados)
}

es_mediana_muestra <- sd(mediana_boot)

### 4

# estimo la varianza
var_muestra <- var(df$f) 

varianza_boot <- rep(NaN, 10000)

for (i in 1:length(varianza_boot)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  varianza_boot[i] <- var(x_sorteados)
}

es_varianza_muestra <- sd(varianza_boot)

### 5

# no se asume ningun modelo.

### 6

hist(df$f)
plot(density(df$f))

# se parece a la distribucion expoencial

########### Parte 2

### 1

# estimo P(X <= 40) asumiendo distribucion exponencial
lambda_est <- 1/mean(df$f)

p_40_exp <- pexp(40, lambda_est)

## standar error

p_boot_exp <- rep(NaN, 10000)

for (i in 1:length(p_boot_exp)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  lambda_est <- 1/mean(x_sorteados)
  p_boot_exp[i] <-pexp(40, lambda_est)
}

es_p_muestra_exp <- sd(p_boot_exp)

### 8
# con los datos que tengo
hist(df$f, prob = T)
lines(density(df$f),
      lwd = 2,
      col = "chocolate3")

# la densidad ahora es a partir de una distribucion exp con param lamnda = lamda_est ??? me da mal!
hist(df$f, prob = T)
lines(density(rexp(1000, 1/lambda_est)),
      lwd = 2,
      col = "chocolate3")


### 9

# media estimada
lambda_est <- 1/mean(df$f)

prom_muestra_exp <- 1/lambda_est

# error estandar estimado

prom_boot_exp <- rep(NaN, 10000)

for (i in 1:length(prom_boot_exp)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  lambda_est <- 1/mean(x_sorteados)
  prom_boot_exp[i] <-1/lambda_est
}

es_prom_muestra_exp <- sd(prom_boot_exp)

### 10

# estimo la mediana asumiendo una distribucion exponencial
lambda_est <- 1/mean(df$f)

mediana_muestra_exp <- log(2)/lambda_est

mediana_boot_exp <- rep(NaN, 10000)

for (i in 1:length(mediana_boot_exp)) {
  x_sorteados <- sample(df$f,size = length(df$f), replace = T)
  lambda_est <- 1/mean(x_sorteados)
  mediana_boot_exp[i] <-  log(2)/lambda_est
}

es_mediana_muestra_exp <- sd(mediana_boot_exp)


### 11

# estimo varianza asumiendo una distribucion exponencial
lambda_est <- 1/mean(df$f)

varianza_muestra_exp <- 1/sqrt(lambda_est)

varianza_boot_exp <- rep(NaN, 10000)

for (i in 1:length(varianza_boot_exp)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  lambda_est <- 1/mean(x_sorteados)
  varianza_boot_exp[i] <- 1/sqrt(lambda_est)
}

es_varianza_muestra_exp <- sd(varianza_boot_exp)


########### Parte 3

### 12        
mu <- 1/lambda
vari <- 1/lambda**2

p_40 <- 1-exp(-lambda*40)

#### 13
lambda <- 0.03


p_40_sombrero <- df %>%
  filter(f <= 40) %>%
  nrow()/length(df$f)


lambda_est <- 1/mean(df$f)

p_40_rulo <- pexp(40, lambda_est)


# creo una funcion que saque el ECME para el estimador sombrero
ECME_sombrero <- function(n){
  
  Nrep <- 1000
  
  muchos_p_40_sombrero <- rep(NaN,Nrep)
  
  for (i in 1:Nrep) {
    muchas_exponenciales <- rexp(n, 0.03)
    p_sombrero <- length(muchas_exponenciales[muchas_exponenciales <= 40])/
      length(muchas_exponenciales)
    muchos_p_40_sombrero[i] <- p_sombrero
    
  }
  
  p_40 <- 1-exp(-lambda*40)
  
  ECME_sombrero <- sum((muchos_p_40_sombrero-  p_40)**2) / 
    length(muchos_p_40_sombrero)
  
  return(ECME_sombrero)
}

# creo una funcion que saque el ECME para el estimador rulo
ECME_rulo <- function(n){
  
  Nrep <- 1000
  
  muchos_p_40_rulo <- rep(NaN,Nrep)
  
  for (i in 1:Nrep) {
    muchas_exponenciales <- rexp(n, 0.03)
    lambda_est <- 1/mean(muchas_exponenciales)
    p_rulo <- pexp(40, lambda_est)
    muchos_p_40_rulo[i] <- p_rulo
  }
  
  p_40 <- 1-exp(-lambda*40)
  
  ECME_rulo <- sum((muchos_p_40_rulo -  p_40)**2) / 
    length(muchos_p_40_rulo)
  
  return(ECME_rulo)
}


# pruebo la funcion con diferentes ns

# n50
ECME_sombrero_n50 <- ECME_sombrero(n=50)
ECME_rulo_n50 <- ECME_rulo(n=50)

# n150
ECME_sombrero_n150 <- ECME_sombrero(n=150)
ECME_rulo_n150 <- ECME_rulo(n=150)

# n200
ECME_sombrero_n200 <- ECME_sombrero(n=200)
ECME_rulo_n200 <- ECME_rulo(n=200)

# n500
ECME_sombrero_n500 <- ECME_sombrero(n=500)
ECME_rulo_n500 <- ECME_rulo(n=500)

df.ECME <- data.frame(estimador = c("rulo","sombrero"),
                      n50 = c(ECME_rulo_n50,ECME_sombrero_n50),
                      n150= c(ECME_rulo_n150,ECME_sombrero_n150),
                      n200= c(ECME_rulo_n200,ECME_sombrero_n200),
                      n500= c(ECME_rulo_n500,ECME_sombrero_n500))

print(df.ECME)

# Se prefiere el estimador rulo, ya que en cada estimacion 
# tuvo un menor error cuadratico medio empirico.