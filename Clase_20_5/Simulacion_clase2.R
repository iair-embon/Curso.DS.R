df <- read.csv("/home/clinux01/Documentos/Curso.DS.R/Clase20_05/datos_libreta24292n_10.csv")

## error estandar estimado
error_estandar_sombrero <- sd(df$lamparas)/sqrt(length(df$lamparas)) # sd(x)/sqrt(n)


## error estandar de un estimador tita_n es: sqrt(V(tita_n))  


###### parte 2 ##########

df2 <- read.csv("/home/clinux01/Documentos/Curso.DS.R/Clase20_05/datos_libreta1234n_5.csv")

estimacion_mean <- mean(df2$gas_equipo_1)
error_estandar_con_var_conocida <- 3/sqrt(length(df2$gas_equipo_1))

alfa <- 0.05
factor <- qnorm(1-alfa/2)
a_intervalo_95_conf <- estimacion_mean - factor*error_estandar_con_var_conocida
b_intervalo_95_conf <- estimacion_mean+ factor*error_estandar_con_var_conocida

## datos de agos 
a_intervalo_95_conf <= 70 & 70 <= b_intervalo_95_conf ## revisar

## simulo mis datos
n <- 5
sigma.cuadrad <- 9
mu <- 70

mis.datos <- rnorm(n,mu,sqrt(sigma.cuadrad))

estimacion_mean <- mean(mis.datos)
error_estandar_con_var_conocida <- 3/sqrt(length(mis.datos))
a_intervalo_95_conf <- estimacion_mean - factor*error_estandar_con_var_conocida
b_intervalo_95_conf <- estimacion_mean+ factor*error_estandar_con_var_conocida

a_intervalo_95_conf <= 70 & 70 <= b_intervalo_95_conf

## pruebo esto 1000 veces, para ver cuantas de esas veces el int de conf
## contiene al verdadero parametro

vec1 <- rep(NaN,1000)

for (i in 1:length(vec1)) {
  mis.datos <- rnorm(n,mu,sqrt(sigma.cuadrad))
  
  estimacion_mean <- mean(mis.datos)
  error_estandar_con_var_conocida <- 3/sqrt(length(mis.datos))
  a_intervalo_95_conf <- estimacion_mean - factor*error_estandar_con_var_conocida
  b_intervalo_95_conf <- estimacion_mean+ factor*error_estandar_con_var_conocida
  
  vec1[i] <- a_intervalo_95_conf <= 70 & 70 <= b_intervalo_95_conf 
}

sum(vec1)/1000

## jugamos a que no conocemos el sd XD
vec1 <- rep(NaN,1000)

for (i in 1:length(vec1)) {
  mis.datos <- rnorm(n,mu,sqrt(sigma.cuadrad))
  
  estimacion_mean <- mean(mis.datos)
  error_estandar_con_var_conocida <- sd(mis.datos)/sqrt(length(mis.datos))
  a_intervalo_95_conf <- estimacion_mean - factor*error_estandar_con_var_conocida
  b_intervalo_95_conf <- estimacion_mean+ factor*error_estandar_con_var_conocida
  
  vec1[i] <- a_intervalo_95_conf <= 70 & 70 <= b_intervalo_95_conf 
}

## ya no estamos mas en una distribucion nomal, al haber cambiado la V por S**2
## ahora estamos en una distribucion t n-1
sum(vec1)/1000

## arreglamos lo de la distribucion arreglando el factor 
factor <- qt(1-alfa/2, n-1)


vec1 <- rep(NaN,1000)

for (i in 1:length(vec1)) {
  mis.datos <- rnorm(n,mu,sqrt(sigma.cuadrad))
  
  estimacion_mean <- mean(mis.datos)
  error_estandar_con_var_conocida <- sd(mis.datos)/sqrt(length(mis.datos))
  a_intervalo_95_conf <- estimacion_mean - factor*error_estandar_con_var_conocida
  b_intervalo_95_conf <- estimacion_mean+ factor*error_estandar_con_var_conocida
  
  vec1[i] <- a_intervalo_95_conf <= 70 & 70 <= b_intervalo_95_conf 
}

sum(vec1)/1000