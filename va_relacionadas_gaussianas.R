library(ggplot2)

##############################
##############################
##############################

############################################################
##################### v.a. chi-cuadrada ####################
############################################################

### Tabulación chi^2 con 5 grados de libertad desde x=0 hasta x=25
curva_teorica <- data.frame(x_val = c(0:1250)/50,
                            y_val = dchisq(c(0:1250)/50,df=5))

ggplot() +
  geom_line(data=curva_teorica,
            aes(x=x_val,y=y_val))

###### simulación: simulamos una 
###### chi^2 con 5 grados de libertad 

chi_simulada <- data.frame(datos = rchisq(100000,df=5),
                           tipo = "simulada")

###### Vemos su histograma

ggplot()+
  geom_histogram(data=chi_simulada,
                 aes(x=datos,y=stat(density)),
                 fill = "salmon",
                 binwidth = 0.5)

###### Generamos 5 muestras gaussianas estandar de tamaño 800
X1 <- rnorm(800,0,1)
X2 <- rnorm(800,0,1)
X3 <- rnorm(800,0,1)
X4 <- rnorm(800,0,1)
X5 <- rnorm(800,0,1)

###### Hacemos la suma de sus cuadrados

suma_cuadrada <- data.frame(datos = X1^2+X2^2+X3^2+X4^2+X5^2,
                            tipo = "suma_cuadrados")

###### Unimos chi_simulada con suma_cuadrada y graficamos

mis_datos <- rbind(chi_simulada,suma_cuadrada)

ggplot()+
  geom_histogram(data=mis_datos,
                 aes(x=datos,y=stat(density),fill = tipo),
                 alpha=0.5,position = "identity",
                 binwidth = 0.5)

#### histogramas con curva teorica:

ggplot()+
  geom_histogram(data=mis_datos,
                 aes(x=datos,y=stat(density),fill = tipo),
                 alpha=0.5,position = "identity",
                 binwidth = 0.5) +
  geom_line(data = curva_teorica,aes(x=x_val,y=y_val))

############################################################
##################### v.a. t de Student ####################
############################################################

### Tabulación t de Student con 7 grados de libertad desde 
### x=-5 hasta x=5

curva_teorica <- data.frame(x_val = c(-250:250)/50,
                            y_val = dt(c(-250:250)/50,df=7))

ggplot() +
  geom_line(data=curva_teorica,
            aes(x=x_val,y=y_val))

###### simulación: simulamos una 
###### t con 7 grados de libertad 

n=7

t_simulada <- data.frame(datos = rt(100000,df=n),
                         tipo = "simulada")

###### Vemos su histograma

ggplot()+
  geom_histogram(data=t_simulada,
                 aes(x=datos,y=stat(density)),
                 fill = "salmon",
                 binwidth = 0.5)

###### Generamos 7 muestras gaussianas estandar de tamaño 800

mu = 3

gaussianas <- data.frame(X1 = rnorm(800,mu,1.5),
                         X2 = rnorm(800,mu,1.5),
                         X3 = rnorm(800,mu,1.5),
                         X4 = rnorm(800,mu,1.5),
                         X5 = rnorm(800,mu,1.5),
                         X6 = rnorm(800,mu,1.5),
                         X7 = rnorm(800,mu,1.5)
)

##### Construimos una t con 7 grados de libertad utilizando
##### las 7 gaussianas anterioes.

X_prom <- rowMeans(gaussianas)
S <- sqrt(rowSums((gaussianas-X_prom)^2)/(n-1)) 

t_construida <- data.frame(datos =(X_prom-mu)/(S/sqrt(n)),
                           tipo = "t_construida")

###### Unimos t_simulada con t_construida y graficamos

mis_datos <- rbind(t_simulada,t_construida)

ggplot()+
  geom_histogram(data=mis_datos,
                 aes(x=datos,y=stat(density),fill = tipo),
                 alpha=0.5,position = "identity",
                 binwidth = 0.5) +
  scale_fill_manual(values=c("red", "blue"))

#### histogramas con curva teorica:

ggplot()+
  geom_histogram(data=mis_datos,
                 aes(x=datos,y=stat(density),fill = tipo),
                 alpha=0.5,position = "identity",
                 binwidth = 0.5) +
  scale_fill_manual(values=c("red", "blue")) +
  geom_line(data = curva_teorica,aes(x=x_val,y=y_val)) +
  xlim(-6,6)

############################################################
############################ TLC ###########################
############################################################

tabulador <- function(vector_parametros){
  parametros = as.numeric(vector_parametros[-1])
  if(vector_parametros[1]=="uniforme"){
    a = parametros[1]
    b = parametros[2]
    x_val = c(a-1.5+(b+1.5)/100 * (1:100),a,b)
    y_val = dunif(x_val,a,b)
    simulacion <- runif(1000000,a,b)
    limites = c(a-1.5,b+1.5)
  } else if(vector_parametros[1]=="poisson"){
    lambda = parametros[1]
    x_val = (0:200)/10
    y_val = dpois(x_val,lambda = lambda)
    simulacion <- rpois(100000,lambda)
    limites = c(-2,4*lambda+1)
  } else if(vector_parametros[1]=="exponencial"){
    lambda = parametros[1]
    x_val = (0:2500)/5
    y_val = dexp(x_val,rate = lambda)
    simulacion <- rexp(1000000,lambda)
    limites = c(0,3*lambda+1)
  } else if(vector_parametros[1]=="geometrica"){
    p = parametros[1]
    x_val = (0:500)/5
    y_val = dgeom(x_val,prob = p)
    simulacion <- rgeom(100000,p)
    limites = c(0,50) 
  }
  tabla <- data.frame(x_val = x_val, y_val = y_val,
                      tipo = vector_parametros[1])
  va_simulada <- data.frame(datos = simulacion,
                            tipo = vector_parametros[1])
  return(list(tabla = tabla,
              va_simulada = va_simulada,
              x_lim = limites))
}


graficador <- function(tipo_variable){
  if(tipo_variable == "continua"){
    ggplot() +
      geom_histogram(data = simulacion,
                     aes(x=datos,y=stat(density)),
                     fill="salmon",
                     binwidth = 0.1) +
      geom_line(data = tabla, aes(x=x_val,y=y_val))+
      xlim(limites)
  } else if(tipo_variable=="discreta"){
    ggplot() +
      geom_histogram(data = simulacion,
                     aes(x=datos,y=stat(density)),
                     fill="salmon",
                     binwidth = 1) +
      geom_point(data = tabla, aes(x=x_val,y=y_val))+
      xlim(limites)
  }
}

variable_aleatoria <- c("geometrica",0.13)
tabla <- tabulador(variable_aleatoria)$tabla
limites <- tabulador(variable_aleatoria)$x_lim
simulacion <- tabulador(variable_aleatoria)$va_simulada

graficador("discreta")

############################################################
############################################################

generador_variables <- function(vector_parametros,cuantos){
  parametros = as.numeric(vector_parametros[-1])
  if(vector_parametros[1] == "uniforme"){
    va_ind <- data.frame(replicate(cuantos,
                                   runif(1000000,parametros[1],parametros[2])))
    media = mean(parametros)
    varianza_original = (parametros[1]-parametros[2])^2/12
  } else if(vector_parametros[1]=="exponencial"){
    va_ind <- data.frame(replicate(cuantos,
                                   rexp(1000000,parametros[1])))
    media = 1/parametros[1]
    varianza_original = 1/parametros[1]^2
  } else if(vector_parametros[1]=="poisson"){
    va_ind <- data.frame(replicate(cuantos,
                                   rpois(100000,parametros[1])))
    media = parametros[1]
    varianza_original = parametros[1]
  } else if(vector_parametros[1]=="geometrica"){
    va_ind <- data.frame(replicate(cuantos,
                                   rgeom(100000,parametros[1])))
    media = (1-parametros[1])/parametros[1]
    varianza_original = (1-parametros[1])/parametros[1]^2
  }
  va_medias <- data.frame(medias = rowMeans(va_ind),
                          tipo = vector_parametros[1])
  varianza = varianza_original/cuantos
  desviacion = sqrt(varianza)
  x_val = (-30:30)*desviacion/3 + media
  gauss_teor <- data.frame(x_val = x_val,
                           y_val = dnorm(x_val,media,desviacion)) 
  
  salida <- list(va_ind = va_ind,
                 va_medias = va_medias,
                 mu = media,
                 desviacion = desviacion,
                 gauss_teor = gauss_teor)
  
  return(salida)
}

mi_reacomodo <- function(tabla){
  separadas <- mapply(
    function(x,y){
      data.frame(datos = x,
                 tipo = paste0("X",y))},
    tabla,
    1:ncol(tabla),
    SIMPLIFY = FALSE
  )
  return(separadas)
}

vector_parametros <- c("geometrica",0.5)
cuantos <- 50

datos = generador_variables(vector_parametros,cuantos)

names(datos$va_medias) <- c("datos","tipo")

tabla_reacomodada <- do.call(rbind,mi_reacomodo(datos$va_ind))
tabla_reacomodada_extendida <- rbind(tabla_reacomodada,datos$va_medias)


ggplot() +
  geom_line(data=datos$gauss_teor,
            aes(x=x_val,y=y_val)) +
  geom_histogram(data=tabla_reacomodada_extendida,
                 aes(x=datos,y=stat(density),fill=tipo),
                 alpha=0.1,position = "identity",
                 binwidth = 0.1) + 
  xlim(0,4)


###########################################
###########################################

