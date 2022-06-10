#### se intento sacar la estimacion de max verosimilitud (no da el resultado correcto, revisar)
datos_norm <- read.table("./Escritorio/datos_norm.txt")
mu <- seq(0,10,0.001)
mult_mu <- c()

mve <- function(datos,mu){
  for (j in 1:length(mu)) {
    multp_normales <- 1
    
    for (i in 1:length(datos_norm)) {
      multp_normales <- multp_normales * dnorm(datos_norm$V1[i],mu[j],9)
    }
    mult_mu <- c(mult_mu,multp_normales)
  }
  mve_index <- which.max(mult_mu)
  return(mu[mve_index])
}

mve_datos <- mve(datos_norm,mu)