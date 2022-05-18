# 1

Nrep <- 10000

vector <- runif(Nrep,67,73)
pos <- Nrep*0.9

q9 <- sort(vector)[pos]

# 2

Nrep <- 1000

muchas_uniformes <- runif(Nrep,67,73)

# 3

muchos_cuantiles_uniformes <- rep(NaN, Nrep)

for (i in 1:length(muchos_cuantiles_uniformes)) {
  part_muchas_uniformes <- muchas_uniformes[1:i]
  muchos_cuantiles_uniformes[i] <- quantile(part_muchas_uniformes,0.9)
}

# 4
plot(1:Nrep, muchos_cuantiles_uniformes, ylim = c(65,75))

# 5

Ngen <- 10

for (i in 1:Ngen){
  muchas_uniformes <- runif(Nrep,67,73)
  muchos_cuantiles_uniformes <- rep(NaN, Nrep)
  
  for (j in 1:length(muchos_cuantiles_uniformes)) {
    part_muchas_uniformes <- muchas_uniformes[1:j]
    muchos_cuantiles_uniformes[j] <- quantile(part_muchas_uniformes,0.9)
  }
  points(1:Nrep, muchos_cuantiles_uniformes)
}

# 6 - el varlor limite, se puede observar que el grafico se acerca a ese valor

qunif(0.9,67,73)


######### Distribucion nornal ##########

# 1

p9 <- qnorm(0.9,70,1.2)

# 2

Nrep <- 1000

muchas_normales <- rnorm(Nrep,70,1.2)

# 3

muchos_cuantiles_normales <- rep(NaN, Nrep)

for (i in 1:length(muchos_cuantiles_normales)) {
  part_muchas_normales <- muchas_normales[1:i]
  muchos_cuantiles_normales[i] <- quantile(part_muchas_normales,0.9)
}

# 4
plot(1:Nrep, muchos_cuantiles_normales, ylim = c(60,80))

# 5

Ngen <- 10

for (i in 1:Ngen){
  muchas_normales <- rnorm(Nrep,70,1.2)
  muchos_cuantiles_normales <- rep(NaN, Nrep)
  
  for (j in 1:length(muchos_cuantiles_normales)) {
    part_muchas_normales <- muchas_normales[1:j]
    muchos_cuantiles_normales[j] <- quantile(part_muchas_normales,0.9)
  }
  points(1:Nrep, muchos_cuantiles_normales)
}

# 6
qnorm(0.9,70,1.2)

# se condice con lo esperable

######### Distribucion exponencial ##########

# 1

p9 <- qexp(0.9,1/10)

# 2

Nrep <- 1000

muchas_exponenciales <- rexp(Nrep,1/10)

# 3

muchos_cuantiles_exponenciales <- rep(NaN, Nrep)

for (i in 1:length(muchos_cuantiles_exponenciales)) {
  part_muchas_exponenciales <- muchas_exponenciales[1:i]
  muchos_cuantiles_exponenciales[i] <- quantile(part_muchas_exponenciales,0.9)
}

# 4
plot(1:Nrep, muchos_cuantiles_exponenciales, ylim = c(0,35))

# 5

Ngen <- 10

for (i in 1:Ngen){
  muchas_exponenciales <- rexp(Nrep,1/10)
  muchos_cuantiles_exponenciales <- rep(NaN, Nrep)
  
  for (j in 1:length(muchos_cuantiles_exponenciales)) {
    part_muchas_exponenciales <- muchas_exponenciales[1:j]
    muchos_cuantiles_exponenciales[j] <- quantile(part_muchas_exponenciales,0.9)
  }
  points(1:Nrep, muchos_cuantiles_exponenciales)
}

# 6
qexp(0.9,1/10)

# se condice con lo esperable


######### histogramas ###########

## MUESTRA N5 

Ngen <- 100
N5 <- 5
n5_unif <- c()
n5_norm <- c()
n5_exp <- c()

for (i in 1:Ngen){
  
  # unif
  muchas_uniformes <- runif(N5,67,73)
  muchos_cuantiles_uniformes <- rep(NaN, N5)
  
  # norm
  muchas_normales <- rnorm(N5,70,1.2)
  muchos_cuantiles_normales <- rep(NaN, N5)

  # exp
  muchas_exponenciales <- rnorm(N5,1/10)
  muchos_cuantiles_exponenciales <- rep(NaN, N5)
  
  for (j in 1:N5) {
    
    # unif
    part_muchas_uniformes <- muchas_uniformes[1:j]
    muchos_cuantiles_uniformes[j] <- quantile(part_muchas_uniformes,0.9)
    
    # norm
    part_muchas_normales <- muchas_normales[1:j]
    muchos_cuantiles_normales[j] <- quantile(part_muchas_normales,0.9)
    
    # exp
    part_muchas_exponenciales <- muchas_exponenciales[1:j]
    muchos_cuantiles_exponenciales[j] <- quantile(part_muchas_exponenciales,0.9)
  }

  n5_unif <-  c(n5_unif,muchos_cuantiles_uniformes)
  n5_norm <-  c(n5_norm,muchos_cuantiles_normales)
  n5_exp <-  c(n5_exp,muchos_cuantiles_exponenciales)
}

hist(n5_unif)
hist(n5_norm)
hist(n5_exp)

## MUESTRA N30

Ngen <- 100
N30 <- 30
n30_unif <- c()
n30_norm <- c()
n30_exp <- c()

for (i in 1:Ngen){
  
  # unif
  muchas_uniformes <- runif(N30,67,73)
  muchos_cuantiles_uniformes <- rep(NaN, N30)
  
  # norm
  muchas_normales <- rnorm(N30,70,1.2)
  muchos_cuantiles_normales <- rep(NaN, N30)
  
  # exp
  muchas_exponenciales <- rnorm(N30,1/10)
  muchos_cuantiles_exponenciales <- rep(NaN, N30)
  
  for (j in 1:N30) {
    
    # unif
    part_muchas_uniformes <- muchas_uniformes[1:j]
    muchos_cuantiles_uniformes[j] <- quantile(part_muchas_uniformes,0.9)
    
    # norm
    part_muchas_normales <- muchas_normales[1:j]
    muchos_cuantiles_normales[j] <- quantile(part_muchas_normales,0.9)
    
    # exp
    part_muchas_exponenciales <- muchas_exponenciales[1:j]
    muchos_cuantiles_exponenciales[j] <- quantile(part_muchas_exponenciales,0.9)
  }
  
  n30_unif <-  c(n30_unif,muchos_cuantiles_uniformes)
  n30_norm <-  c(n30_norm,muchos_cuantiles_normales)
  n30_exp <-  c(n30_exp,muchos_cuantiles_exponenciales)
}

hist(n30_unif)
hist(n30_norm)
hist(n30_exp)

## MUESTRA N100

Ngen <- 100
N100 <- 100
n100_unif <- c()
n100_norm <- c()
n100_exp <- c()

for (i in 1:Ngen){
  
  # unif
  muchas_uniformes <- runif(N100,67,73)
  muchos_cuantiles_uniformes <- rep(NaN, N100)
  
  # norm
  muchas_normales <- rnorm(N100,70,1.2)
  muchos_cuantiles_normales <- rep(NaN, N100)
  
  # exp
  muchas_exponenciales <- rnorm(N100,1/10)
  muchos_cuantiles_exponenciales <- rep(NaN, N100)
  
  for (j in 1:N100) {
    
    # unif
    part_muchas_uniformes <- muchas_uniformes[1:j]
    muchos_cuantiles_uniformes[j] <- quantile(part_muchas_uniformes,0.9)
    
    # norm
    part_muchas_normales <- muchas_normales[1:j]
    muchos_cuantiles_normales[j] <- quantile(part_muchas_normales,0.9)
    
    # exp
    part_muchas_exponenciales <- muchas_exponenciales[1:j]
    muchos_cuantiles_exponenciales[j] <- quantile(part_muchas_exponenciales,0.9)
  }
  
  n100_unif <-  c(n100_unif,muchos_cuantiles_uniformes)
  n100_norm <-  c(n100_norm,muchos_cuantiles_normales)
  n100_exp <-  c(n100_exp,muchos_cuantiles_exponenciales)
}

hist(n100_unif)
hist(n100_norm)
hist(n100_exp)
