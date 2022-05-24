set.seed(123)

# genero datos exponenciales
muchos_exp <- rexp(100,1/10)

# saco el cuantil 90 posta
q9 <- qexp(0.9,1/10)

# lo estimo con R en mis datos generados
qsombrero <- quantile(muchos_exp,0.9)

# estimo lamda a partir del primedio a la menos 1
lamda_est <- mean(muchos_exp)**-1
qrulo <- (-log(1-0.9))/lamda_est


##### vemos cual conviene mas con el error cuadratico medio empirico ####
muchos_exp2 <- rexp(1000,(1/10))

for (i in 1:length(muchos_exp2)) {
  part_muchos_exp2 <- 
}