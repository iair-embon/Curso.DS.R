## 11/05 guia de trabajo
#########

p <- 0.2
Nrep <- 1000
muchas_ber <- rbinom(Nrep,size = 1 ,prob = p)

muchos_prom <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  muchas_ber_corte <- muchas_ber[1:i]
  muchos_prom[i] <- mean(muchas_ber_corte)
}

plot(1:Nrep,
     muchos_prom,
     ylim = c(0,1)
     )

muchas_ber2 <- rbinom(Nrep,size = 1 ,prob = p)
muchos_prom2 <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  muchas_ber_corte <- muchas_ber2[1:i]
  muchos_prom2[i] <- mean(muchas_ber_corte)
}

points(1:Nrep,
     muchos_prom2,
     ylim = c(0,1),
     col=2
)


################
muchas_ber <- rbinom(Nrep,size = 1 ,prob = p)

muchos_prom <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  muchas_ber_corte <- muchas_ber[1:i]
  muchos_prom[i] <- mean(muchas_ber_corte)
}

plot(1:Nrep,
     muchos_prom,
     ylim = c(0,1)
)


Ngen <- 20

for (j in 1:(Ngen-1)) {
  muchas_ber <- rbinom(Nrep,size = 1 ,prob = p)
  muchos_prom <- rep(NaN,Nrep)
  
  for (i in 1:Nrep) {
    muchas_ber_corte <- muchas_ber[1:i]
    muchos_prom[i] <- mean(muchas_ber_corte)
  }
  
  points(1:Nrep,
         muchos_prom,
         ylim = c(0,1),
         col=j
  )
}

########## 1.2

muchos_unif <- runif(Nrep,67,73)

muchos_prom <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  muchas_unif_corte <- muchos_unif[1:i]
  muchos_prom[i] <- mean(muchas_unif_corte)
}

plot(1:Nrep,
     muchos_prom,
     ylim = c(67,73)
)


Ngen <- 20

for (j in 1:(Ngen-1)) {
  muchas_unif <- runif(Nrep,67,73)
  muchos_prom <- rep(NaN,Nrep)
  
  for (i in 1:Nrep) {
    muchas_unif_corte <- muchas_unif[1:i]
    muchos_prom[i] <- mean(muchas_unif_corte)
  }
  
  points(1:Nrep,
         muchos_prom,
         col=j
  )
}


# Frecuencias Relativas y Probabilidades

# 7
# 4/6

# 8
muchas_uniformes <- runif(Nrep, 67,73) 

hist(muchas_uniformes)

# 9
muchos_fr <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  muchas_unif_corte1 <- muchos_unif[1:i]
  muchas_unif_corte2 <- muchas_unif_corte1[muchas_unif_corte1 > 68 & muchas_unif_corte1 < 72]
  muchos_fr[i] <- length(muchas_unif_corte2)/ length(muchas_unif_corte1)
}

# 10 
plot(x=1:length(muchas_uniformes), y= muchos_fr, ylim=c(0,1))

# 11 
Ngen <- 20

for (j in 1:(Ngen-1)) {
  muchas_unif <- runif(Nrep,67,73)
  muchos_fr <- rep(NaN,Nrep)
  
  for (i in 1:Nrep) {
    muchas_unif_corte1 <- muchas_unif[1:i]
    muchas_unif_corte2 <- muchas_unif_corte1[muchas_unif_corte1 > 68 & muchas_unif_corte1 < 72]
    muchos_fr[i] <- length(muchas_unif_corte2)/ length(muchas_unif_corte1)
  }
  
  points(1:Nrep,
         muchos_fr,
         col=j
  )
}

# 12 
# 0.66

#### 1.3 Distribucion Normal

rnorm(n,70,1.2)

# 1
# mu

# 2
muchas_normales <- rnorm(Nrep, 70,1.2)
hist(muchas_normales)

# 3
muchos_promedios_normales <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  muchas_norm_corte <- muchas_normales[1:i]
  muchos_promedios_normales[i] <- mean(muchas_norm_corte)
}

plot(1:Nrep,
     muchos_promedios_normales,
     ylim = c(60,80)
)


Ngen <- 20

for (j in 1:(Ngen-1)) {
  muchas_normales <- rnorm(Nrep, 70,1.2)
  muchos_promedios_normales <- rep(NaN,Nrep)
  
  
  for (i in 1:Nrep) {
    muchas_norm_corte <- muchas_normales[1:i]
    muchos_promedios_normales[i] <- mean(muchas_norm_corte)
  }
  
  points(1:Nrep,
         muchos_promedios_normales,
         col=j
  )
}


# 7








