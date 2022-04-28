### parte 1 ###

# 0 aceptadora; 1 rechazadora
y <- c(0,1)
P_y <- c(0.9,0.1)

# r = "remover huevo"
p_r_dado_y <- c(0.3,0.8)

n < -8

x <- 1:8

## ej 1

dbinom(5,8,prob = p_r_dado_y[1])

## ej 2
p_x_0 <- dbinom(x,8,prob = p_r_dado_y[1])

library(ggplot2)

df <- data.frame(x = x, y = p_x_0)

barplot(df$y, names.arg = df$x, main = "Probabilidad de X=x dado Y=0")

## ej 3
dbinom(5,8,prob = p_r_dado_y[2])

## ej 4
p_x_1 <- dbinom(x,8,prob = p_r_dado_y[2])

df <- data.frame(x = x, y = p_x_1)

barplot(df$y, names.arg = df$x, main = "Probabilidad de X=x dado Y=1")

## ej 5

dbinom(5,8,prob = p_r_dado_y[1]) + dbinom(5,8,prob = p_r_dado_y[2])

## ej 6 
