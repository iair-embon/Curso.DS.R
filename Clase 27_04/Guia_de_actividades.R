### parte 1 ###

# 0 aceptadora; 1 rechazadora
y <- c(0,1)
P_y <- c(0.9,0.1)

# r = "remover huevo"
p_r_dado_y <- c(0.3,0.8)

n <- 8

x <- 0:8

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

dbinom(5,n,prob = p_r_dado_y[1])*P_y[1] + 
  dbinom(5,n,prob = p_r_dado_y[2]) * P_y[2]

## ej 6 

p_x_removidos <- dbinom(df$x,n,prob = p_r_dado_y[1])*P_y[1] + 
  dbinom(df$x,n,prob = p_r_dado_y[2]) *P_y[2]

## ej 7
tab1 <- matrix(c(df$p_x_0, df$p_x_1,p_x_removidos),nrow = 3, byrow = TRUE)

colnames(tab1) <- 0:8
rownames(tab1) <- c("p_x_0","p_x_1","p_x_removidos")

rowSums(tab1)

## ej 8
library(dplyr)

datos <- read.csv("./Clase 27_04/depredadosclasificadas.txt", sep = " " )

p_acept <- sum(ifelse(datos$especie == 0, 1,0))/ nrow(datos)

## ej 9

datos %>%
  filter(removidos == 5 & especie == 0) %>%
  sum()/nrow(datos)

## ej 10

library(janitor)

tab2 <- datos %>%
  filter(especie == 0) %>%
  tabyl(removidos)


p_x_0_estimated <-  tab2$percent
p_x_0_estimated <- append(p_x_0_estimated,0)

df$p_x_0_estimated <- p_x_0_estimated

library(reshape2)

df2 <- df %>%
  select(x,p_x_0, p_x_0_estimated)   %>%
  melt(id.vars = "x")

ggplot(df2, aes(x=x, y=value, fill= variable)) +
  geom_bar(stat="identity",position = "dodge")

## ej 11

tab3 <- datos %>%
  filter(especie == 1) %>%
  tabyl(removidos)

p_x_1_estimated <-  tab3$percent
p_x_1_estimated <- append(c(0,0,0),p_x_1_estimated)

df$p_x_1_estimated <- p_x_1_estimated

df3 <- df %>%
  select(x,p_x_1, p_x_1_estimated)   %>%
  melt(id.vars = "x")

ggplot(df3, aes(x=x, y=value, fill= variable)) +
  geom_bar(stat="identity",position = "dodge")

## ej 12

# se estima la prob conjunta de los vect x,y
tab4 <- matrix(c(df$p_x_0_estimated, 
                 df$p_x_1_estimated),
               nrow = 2, byrow = TRUE)

colnames(tab4) <- 0:8
rownames(tab4) <- c("p_x_0_estimated",
                    "p_x_1_estimated")

rowSums(tab4)

#############
## Parte B ##
#############

## ej 13

regla5 <- function(x){
  v <- ifelse(x >= 5,1,0)
  return(v)
  }

y_predicted <- regla5(datos$removidos)

error <- ifelse(y_predicted == datos$especie,0,1)

r <- sum(error)/nrow(datos) 

## ej 14
rechazadorpar <- function(x){
  v <- ifelse((x %% 2) == 0,1,0)
  return(v)
}

y_predicted <- rechazadorpar(datos$removidos)

error <- ifelse(y_predicted == datos$especie,0,1)

r <- sum(error)/nrow(datos) 

## ej 15

# Se prefiere la funcion regla5, porque tiene un error inmensamente menor a
# rechazadorpar

## ej 16

reglacorte <- function(x,t){
  
  v <- c()
  
  for (i in 1:length(t)) {
    y_predicted <- ifelse(x >= t[i],1,0)
    error <- ifelse(y_predicted == datos$especie,0,1)
    
    r <- sum(error)/nrow(datos) 
    
    v <- c(v,r)
  }
  return(v)
}

r <- reglacorte(x= datos$removidos,t = c(1:8))

## ej 17 

which(r == min(r))
# 6

## 18

ErrorClassTRUE <- function(g,y){
  error <- ifelse(g != y,1,0)
  r <- mean(error)
  return(r)
}

