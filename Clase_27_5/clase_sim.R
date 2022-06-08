# likelihood estimation (estimacion de max verosimilitud con binomial)

tita <- seq(0,1,0.001)

l_muchas_tita <- rep(NaN,length(tita))

for (i in 1:length(tita)) {
  l_tita <- (tita[i]**74) * ((1-tita[i])**26)
  l_muchas_tita[i] <-l_tita
}

plot(tita,l_muchas_tita)

# veo cual es el max
index <- which.max(l_muchas_tita)

max_l <- tita[index]

# saco el log likelihood

log_l_muchas_tita <- rep(NaN,length(tita))

for (i in 1:length(tita)) {
  l_tita <- 74*log(tita[i]) + 26*log(1-tita[i])
  log_l_muchas_tita[i] <-l_tita
}

plot(tita,log_l_muchas_tita)

# veo cual es el max
index <- which.max(log_l_muchas_tita)

max_log_l <- tita[index]

