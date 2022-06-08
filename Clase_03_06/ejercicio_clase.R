theta <- 3
datos <- runif(1000,0,theta)


# creo una funcion que estimaciones segun el estimador de Max y E
muchos_unif_fun <- function(n){
  
  Nrep <- 1000
  
  muchos_unif_prom <- rep(NaN,Nrep)
  muchos_unif_max <- rep(NaN,Nrep)
  
  for (i in 1:Nrep) {
    # genero datos
    muchas_uniformes <- runif(n,0,theta)
    
    # les aplico los procedimientos a los mismos datos
    estimador_prom <- mean(muchas_uniformes)
    muchos_unif_prom[i] <- estimador_prom
    
    estimador_max <- max(muchas_uniformes)
    muchos_unif_max[i] <- estimador_max
  }
  
  muchos_unif <- list("prom" =  muchos_unif_prom,
                      "max" = muchos_unif_max)
  
  return(muchos_unif)
  }

# pruebo la funcion con diferentes ns

muchos_unif_fun_n5 <- muchos_unif_fun(n=5)
muchos_unif_fun_n30 <- muchos_unif_fun(n=30)
muchos_unif_fun_n50 <- muchos_unif_fun(n=50)

prom_n5 <- muchos_unif_fun_n5$prom
prom_n30 <- muchos_unif_fun_n30$prom
prom_n50 <- muchos_unif_fun_n50$prom

max_n5 <- muchos_unif_fun_n5$max
max_n30 <- muchos_unif_fun_n30$max
max_n50 <- muchos_unif_fun_n50$max

### busco el error cuadratico medio

# hago una funcion que te de el error cuadratico medio
ECME <- function(n,func){
  
  muchos_unif <- func(n)
  muchos_unif_max <- muchos_unif$max
  muchos_unif_prom <- muchos_unif$prom 
  
  ECME_max <- sum((muchos_unif_max-  3)**2) / 
    length(muchos_unif_max)
  
  ECME_prom <- sum((muchos_unif_prom- (3/2))**2) / 
    length(muchos_unif_prom)
  
  ECME <- list("ECME_max" = ECME_max,
               "ECME_prom" = ECME_prom)
  return(ECME)
}

## corregir desde aca!!

ECME_5 <- ECME(5,muchos_unif_fun)
ECME_5_max <- ECME_5$ECME_max 
ECME_5_prom <- ECME_5$ECME_prom

ECME_30 <- ECME(30,muchos_unif_fun)
ECME_30_max <- ECME_30$ECME_max 
ECME_30_prom <- ECME_30$ECME_prom

ECME_50 <- ECME(50,muchos_unif_fun)
ECME_50_max <- ECME_50$ECME_max 
ECME_50_prom <- ECME_50$ECME_prom

df.ECME <- data.frame(estimador = c("max","prom"),
                      n5 = c(ECME_5_max,ECME_5_prom),
                      n30= c(ECME_30_max,ECME_30_prom),
                      n50= c(ECME_50_max,ECME_50_prom))

print(df.ECME)