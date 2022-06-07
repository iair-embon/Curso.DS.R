### simulacion clase

# parametro <- mu
n <- 100
set.seed(917)
tita <- 3
muc_unif <- runif(n,0,tita)
estimacion <- mean(muc_unif)
incert_vieja <- sd(muc_unif)/sqrt(n)

# hago boostraping para estunar a incertidumbre
nboot <- 5000
muchos.est.boot <- rep(NaN, nboot)

for (i in 1:nboot) {
  muestra_boot <-sample(muc_unif,n, replace = T)
  muchos.est.boot[i] <- mean(muestra_boot)
}

error_boot <- sd(muchos.est.boot)
