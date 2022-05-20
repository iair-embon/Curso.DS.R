### Guia clase 12 alturnas ###

## 1

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frames with filters already applied
filepath <- root$find_file("./Clase del 6-05/alturas_identificacion_1234_n_500.csv")
df <- read.csv(filepath)

library(ggplot2)
library(hrbrthemes)

ggplot(df, aes(x = altura, fill = genero))+
  geom_histogram()

## 2

indices <- order(abs(df$altura-165))[1:10]
regla <-ifelse(  mean(df$genero[indices]=="F") >= 0.5,1,0)

indices <- order(abs(df$altura-175))[1:10]
regla <-ifelse(  mean(df$genero[indices]=="F") >= 0.5,1,0)

## 3

h <- 1.5
indices <- which(df$altura >= 165 - h   & df$altura <= 165 + h)
prop_1 <- mean(ifelse( df$genero[indices]=="F",1,0))

# si mide 1.65 lo clasificaria como F
indices <- which(df$altura >= 175 - h   & df$altura <= 175 + h)
prop_1 <- mean(ifelse( df$genero[indices]=="F",1,0))

# si mide 1,75 lo clasificaria como M

## 4

ClasificoVecinos <- function(X, Y, xNuevo, k=10){

  indices <- order(abs(X-xNuevo))[1:k]
  regla <-ifelse( mean(Y[indices]== "F") >= 0.5,1,0)
  return(regla)
}

## 5

ClasificoMovil <- function(X, Y, xNuevo, h=1){
  
  indices <- which(X >= xNuevo - h   & X <= xNuevo + h)
  prop_1 <- mean(ifelse( Y[indices]=="F",1,0))
  return(prop_1)
}

## 6 

library(ggplot2)
library(hrbrthemes)

ggplot(df, aes(x = altura, fill = genero))+
  geom_histogram(aes(y = ..density..))+
  geom_density(alpha= 0.4)


# provienen de una distribucion normal. 

## 7
library(dplyr)

prop_f <- df %>% 
  filter(genero == "F") %>%
  nrow()/nrow(df)

# la P(Y=1) = prop_f

## 8

# AlguienNuevo <- 165
AlguienNuevo <- 175

prop_f <- df %>% 
  filter(genero == "F" & 
           altura > (AlguienNuevo-1) &
           altura < (AlguienNuevo+1)) %>%
  nrow()/nrow(df)

prop_m <- df %>% 
  filter(genero == "M" & 
           altura > (AlguienNuevo-1) &
           altura < (AlguienNuevo+1)) %>%
  nrow()/nrow(df)


## 9

ClasificoGenerativo <- function(X, Y, xNuevo){
  df <- data.frame(genero = Y,
                   altura = X)
  prop_f <- df %>% 
    filter(genero == "F" & 
             altura > (xNuevo-1) &
             altura < (xNuevo+1)) %>%
    nrow()/nrow(df)
  
  prop_m <- df %>% 
    filter(genero == "M" & 
             altura > (xNuevo-1) &
             altura < (xNuevo+1)) %>%
    nrow()/nrow(df)
  
  regla <- ifelse(prop_f>prop_m,1,0)
  
  return(regla)
}
  
# la pruebo

ClasificoGenerativo(df$altura,df$genero,165)
ClasificoGenerativo(df$altura,df$genero,175)

## 10


