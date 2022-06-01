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

p_boot <- rep(NaN, 50000)

for (i in 1:length(p_boot)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  p_boot[i] <-length(x_sorteados[x_sorteados <= 40])/ length(x_sorteados)
}

es_p_muestra <- sd(p_boot)/ sqrt(nrow(df)) ### ????? esta bien dividir aca? por nrow(df) o length 50000?


### 2 ??? se puede esto? o tengo que usar boostrap tambien?
prom_muestra <- mean(df$f)
es_media_muestra <- sd(df$f)/ sqrt(nrow(df))

### 3

# estimo la mediana

mediana_muestra <- median(df$f)

# estimo el error estandar de la estimacion de la mediana

mediana_boot <- rep(NaN, 50000)

for (i in 1:length(mediana_boot)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  mediana_boot[i] <- median(x_sorteados)
}

es_mediana_muestra <- sd(mediana_boot)/ sqrt(nrow(df))

### 4

# estimo la varianza
var_muestra <- var(df$f)

varianza_boot <- rep(NaN, 50000)

for (i in 1:length(varianza_boot)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  varianza_boot[i] <- var(x_sorteados)
}

es_varianza_muestra <- sd(varianza_boot)/ sqrt(nrow(df))

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

p_boot_exp <- rep(NaN, 50000)

for (i in 1:length(p_boot_exp)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  lambda_est <- 1/mean(x_sorteados)
  p_boot_exp[i] <-pexp(40, lambda_est)
}

es_p_muestra_exp <- sd(p_boot_exp)/ sqrt(nrow(df))

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
prom_muestra_exp <- 1/lambda_est

# error estandar estimado

prom_boot_exp <- rep(NaN, 50000)

for (i in 1:length(prom_boot_exp)) {
  x_sorteados <- sample(df$f, size = length(df$f), replace = T)
  lambda_est <- 1/mean(x_sorteados)
  prom_boot_exp[i] <-1/lambda_est
}

es_prom_muestra_exp <- sd(prom_boot_exp)/ sqrt(nrow(df))

### 10

