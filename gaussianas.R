###################################################
# Dados una media y una desviación, esta función
# tabula los valores de una gaussiana en 121 valores 
# centrados en la media con separación de 0.02

mi_gaussiana <- function(media,desviacion){
  abscisas <- c(-3000:3000)/50+media
  ordenadas <- dnorm(abscisas,mean = media,sd = desviacion)
  gaussiana <- data.frame(x_val = abscisas,y_val=ordenadas)
  gaussiana$parametros <- paste("med",
                                media,
                                "sd",
                                desviacion,
                                sep="_")
  return(gaussiana)
}

###################################################
## Visualización cambiando medias y desviaciones ##
###################################################

# tabula los valores de una gaussiana estándar
gaussiana_estandar <- mi_gaussiana(0,1)

# Grafica una gaussiana estándar
ggplot() +
  geom_line(data=gaussiana_estandar,aes(x_val,y_val)) +
  xlim(-3,3)

# Se crea una tabla de tres filas y dos columnas.
# Cada fila represente un valor de una media y un valor 
# de la desviación estándar

parametros <- data.frame(medias=c(0,10,4.3),
                         desviaciones=c(1,1,2))

# Construimos los tabulados correspondientes a
# cada uno de los renglones de parámetros
gaussianas <- mapply(mi_gaussiana,
                     parametros$medias,
                     parametros$desviaciones,
                     SIMPLIFY = FALSE)

# Unimos verticalmente los tabulados anteriores
gaussianas <- do.call(rbind,gaussianas)

# Graficamos todos los tabulados
ggplot(data = gaussianas,aes(x_val,y_val,colour=parametros)) +
  geom_line() +
  xlim(-3,13)

###################################################
#### El reescalado de gaussianas es gaussiano #####
###################################################

# Generamos dos números para a y para b
set.seed(2022)
a_b <- sample(1:10,2)

# Generamos una gaussiana con media 1 y desviación 2
gaussiana <- data.frame(datos = rnorm(10000,1,2),
                        tipo = "original")

# Reescalamos la gaussiana anterior
gaussiana_reescalada <- data.frame(datos = a_b[1]*gaussiana$datos+a_b[2],
                                   tipo = "reescalada")

# Unimos verticalmente ambas gaussianas
gaussianas_juntas <- rbind(gaussiana,gaussiana_reescalada)

# Se muestran los histogramas de ambas gaussianas
ggplot()+
  geom_histogram(data = gaussianas_juntas,
                 aes(x=datos,y=stat(density),fill=tipo),
                 alpha=0.5,binwidth = 2)

# Se construye una tabla de dos filas y dos colummas.
# La primera fila es la media y la desviación de la gaussiana
# original. La segunda fila es la media y la desviación
# de la gaussiana reescalada
parametros <- data.frame(medias=c(1,a_b[1]*1+a_b[2]),
                         desviaciones=c(2,a_b[1]*2))

# Construimos los tabulados correspondientes a
# cada uno de los renglones de parámetros
gaussianas <- mapply(mi_gaussiana,
                     parametros$medias,
                     parametros$desviaciones,
                     SIMPLIFY = FALSE)

# Unimos verticalmente los tabulados anteriores
gaussianas <- do.call(rbind,gaussianas)

# Graficamos los histogramas de las simulaciones y las 
# curvas teoricas

ggplot()+
  geom_histogram(data = gaussianas_juntas,
                 aes(x=datos,y=stat(density),fill=tipo),
                 alpha=0.5,binwidth = 0.5) +
  geom_line(data=gaussianas,
            aes(x_val,y_val,colour=parametros)) +
  xlim(-20,40)

###################################################
##### La suma de gaussianas independientes es #####
######## gaussiana con ciertos parámetros #########
###################################################

# Simulamos tres gaussianas independientes
X1 <- data.frame(datos = rnorm(10000,mean=0,sd=1),
                 tipo = "Sumando 1")
X2 <- data.frame(datos = rnorm(10000,mean=-30,sd=sqrt(4)),
                 tipo = "Sumando 2")
X3 <- data.frame(datos = rnorm(10000,mean=15,sd=1),
                 tipo = "Sumando 3")

# Calculamos la suma

suma <- data.frame(datos = X1$datos+X2$datos+X3$datos,
                   tipo = "suma")

# Pegamos verticalmente las cuatro tablas
gaussianas_juntas <- rbind(X1,X2,X3,suma)

# Graficamos los histogramas de las cuatro variables
ggplot()+
  geom_histogram(data = gaussianas_juntas,
                 aes(x=datos,y=stat(density),fill=tipo),
                 alpha=0.5,binwidth = 0.05)

# Creamos una tabla de una fila y dos columnas.
# La fila es la media y la desviación que en teoría debe
# tener la variable suma
parametros <- data.frame(medias=-15,
                         desviaciones=sqrt(6))

# Construimos los tabulados correspondientes a
# cada uno de los renglones de parámetros (solo es un renglón)
gaussianas <- mapply(mi_gaussiana,
                     parametros$medias,
                     parametros$desviaciones,
                     SIMPLIFY = FALSE)

# Unimos verticalmente los tabulados anteriores
gaussianas <- do.call(rbind,gaussianas)

# Graficamos los histogramas de las simulaciones, de la suma 
# y de la curva teorica de la suma
ggplot()+
  geom_histogram(data = gaussianas_juntas,
                 aes(x=datos,y=stat(density),fill=tipo),
                 binwidth = 0.1) + 
  geom_line(data=gaussianas,aes(x_val,y_val)) +
  xlim(-40,20)