df <- read.table("./Escritorio/Clase_1_06/GRB_afterglow.dat", 
                 header=T, skip=1)

###########
# Parte 1 #
###########

### 1
require(dplyr)

p_40 <- df %>%
  filter(f <= 40) %>%
  nrow()/length(df$f)

### 2
prom_muestra <- mean(df$f)

### 3
median_muestra <- median(df$f)

# no lo usaria para estimar la media

### 4
varianza_muestra <- var(df$f)

### 5
hist(df$f)

###########
# Parte 2 #
###########

### 6

lambda_est <- 1/mean(df$f)

p_40_mmv <- pexp(40, lambda_est)

### 7
hist(df$f, prob = T)
lines(dexp(seq(0,140),lambda_est),
      lwd = 2,
      col = "chocolate3")

### 8
media_est_mmv <- 1/lambda_est

### 9
mediana_est_mmv <- log(2)/lambda_est