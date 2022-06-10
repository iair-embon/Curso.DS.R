### parte 1 ###

# 0 aceptadora; 1 rechazadora
y <- c(0,1)
P_y <- c(0.9,0.1)

# r = "remover huevo"
p_r_dado_y <- c(0.3,0.8)

n <- 8

x <- 1:8

## ej 1

dbinom(5,n,prob = p_r_dado_y[1])

## ej 2
p_x_0 <- dbinom(x,n,prob = p_r_dado_y[1])

library(ggplot2)

df <- data.frame(x = x, p_x_0 = p_x_0)

barplot(df$p_x_0, names.arg = df$x, main = "Probabilidad de X=x dado Y=0")

## ej 3
dbinom(5,n,prob = p_r_dado_y[2])

## ej 4
p_x_1 <- dbinom(x,n,prob = p_r_dado_y[2])

df$p_x_1 <-  p_x_1

barplot(df$p_x_1, names.arg = df$x, main = "Probabilidad de X=x dado Y=1")

## ej 5

dbinom(5,n,prob = p_r_dado_y[1])*p_r_dado_y[1] + 
  dbinom(5,n,prob = p_r_dado_y[2]) * p_r_dado_y[2]

## ej 6 

p_x_removidos <- dbinom(df$x,n,prob = p_r_dado_y[1])*p_r_dado_y[1] + 
  dbinom(df$x,n,prob = p_r_dado_y[2]) * p_r_dado_y[2]

## ej 7
tab1 <- matrix(c(df$p_x_0, df$p_x_1,p_x_removidos),nrow = 3, byrow = TRUE)

colnames(tab1) <- 1:8
rownames(tab1) <- c("p_x_0","p_x_1","p_x_removidos")

rowSums(tab1)
### da algo raro, las sumas no me dan 1, deberian

## ej 8
library(dplyr)

datos <- read.csv("./depredadosclasificadas.txt", sep = " " )

p_acept <- sum(ifelse(datos$especie == 0, 1,0))/ nrow(datos)

## ej 9

datos %>%
  filter(removidos == 5 & especie == 0) %>%
  sum()/nrow(datos)

## ej 10
barplot(df$p_x_0, names.arg = df$x, main = "Probabilidad de X=x dado Y=1")
