### parte 1 ###

# 2
datos <- read.csv("./Descargas/alturas_identificacion_1_n_200.csv")

# 3
colnames(datos)

# 4
nrow(datos)

# 5
mean(datos$altura)

# 6 
Datos_sub1 <- datos[datos$genero == "M",]
mean(Datos_sub1$altura)

# 7
Datos_sub2 <- datos[datos$genero == "M" & datos$contextura_madre == "bajita",]
mean(Datos_sub2$altura)

### parte 2 ###

# 1
library(ggplot2)
library(hrbrthemes)

ggplot(datos, aes(x=altura_madre, y=altura, color=genero)) + 
  geom_point(size=6) +
  theme_ipsum()

# 2
Datos_sub1

# 2.1
Datos_sub3 <- Datos_sub1[Datos_sub1$altura_madre >= 155 &
                           Datos_sub1$altura_madre <= 157,]

nrow(Datos_sub3)

# 2.2 
mean(Datos_sub3$altura)

# tarea 3
Datos_sub4 <- Datos_sub1[Datos_sub1$altura_madre >= 153 &
                           Datos_sub1$altura_madre <= 159,]
mean(Datos_sub4$altura)

############
### guia ###
############

# 1
datos2 <- read.csv("./Descargas/alturas_identificacion_1_n_500.csv")

# 2
colnames(datos)

# 3
mean(datos$altura)

# 4 
hist(datos$altura, breaks = 200)

# 5
plot(density(datos$altura))

# 6
library(ggplot2)
library(dplyr)
library(hrbrthemes)


ggplot(datos, aes(x=altura, fill=genero)) +
  geom_histogram(aes(y = stat(density), color="#e9ecef", alpha=0.6, position = 'identity')) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  geom_density(alpha=.7)+
  theme_ipsum() +
  labs(fill="")
  