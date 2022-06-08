### 1

sample(1:6,1, replace = T)

### 2

albun_completo <- 1:6
albun_en_proceso <- rep(0, length(albun_completo))
compra_paquete <- 1

while (sum(albun_completo != albun_en_proceso) != 0) {
  fig <- sample(1:6,1, replace = T)
  
  if(!(fig %in% albun_en_proceso)){
    albun_en_proceso[fig] <- fig
  }
  compra_paquete <- compra_paquete + 1
}

# se debieron comprar 13 paquetes

### 3
cuantasFigus <- function(figusTotal){
  
  albun_completo <- 1:figusTotal
  albun_en_proceso <- rep(0, length(albun_completo))
  compra_paquete <- 1
  
  while (sum(albun_completo != albun_en_proceso) != 0) {
    fig <- sample(1:figusTotal,1, replace = T)
    
    if(!(fig %in% albun_en_proceso)){
      albun_en_proceso[fig] <- fig
    }
    compra_paquete <- compra_paquete + 1
  }
  
  return(compra_paquete)
}


### 4

Nrep <- 1000

sim_cuantas_figuas <- rep(NaN,Nrep)

for (i in 1:Nrep) {
  sim_cuantas_figuas[i] <- cuantasFigus(6)
}

# media
media_sim <- mean(sim_cuantas_figuas)

# P(X <= 16)
p_x_16 <- length(sim_cuantas_figuas[sim_cuantas_figuas <= 16])/ Nrep

# P(X <= x) = 0.9
q24 <- quantile(sim_cuantas_figuas,0.9)


### la ficha sigue, freno aca
