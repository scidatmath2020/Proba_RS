###################################################
# Dados una media y una desviaci�n, esta funci�n
# tabula los valores de una gaussiana en 6001 valores 
# centrados en la media con separaci�n de 0.02

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
## Visualizaci�n cambiando medias y desviaciones ##
###################################################

# tabula los valores de una gaussiana est�ndar
gaussiana_estandar <- mi_gaussiana(0,1)

# Grafica una gaussiana est�ndar
ggplot() +
  geom_line(data=gaussiana_estandar,aes(x_val,y_val)) +
  xlim(-3,3)

# Se crea una tabla de tres filas y dos columnas.
# Cada fila represente un valor de una media y un valor 
# de la desviaci�n est�ndar

parametros <- data.frame(medias=c(0,10,4.3),
                         desviaciones=c(1,1,2))

# Construimos los tabulados correspondientes a
# cada uno de los renglones de par�metros
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

# Generamos dos n�meros para a y para b
set.seed(2022)
a_b <- sample(1:10,2)

# Generamos una gaussiana con media 1 y desviaci�n 2
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
                 alpha=0.5,
                 binwidth = 0.5,
                 position = "identity")

# Se construye una tabla de dos filas y dos colummas.
# La primera fila es la media y la desviaci�n de la gaussiana
# original. La segunda fila es la media y la desviaci�n
# de la gaussiana reescalada
parametros <- data.frame(medias=c(1,a_b[1]*1+a_b[2]),
                         desviaciones=c(2,a_b[1]*2))

# Construimos los tabulados correspondientes a
# cada uno de los renglones de par�metros
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
                 alpha=0.5,
                 binwidth = 0.5,
                 position = "identity") +
  geom_line(data=gaussianas,
            aes(x_val,y_val,colour=parametros)) +
  xlim(-20,40)

###################################################
##### La suma de gaussianas independientes es #####
######## gaussiana con ciertos par�metros #########
###################################################

# Simulamos tres gaussianas independientes
X1 <- data.frame(datos = rnorm(10000,mean=0,sd=1),
                 tipo = "Sumando 1")
X2 <- data.frame(datos = rnorm(10000,mean=5,sd=sqrt(4)),
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
                 alpha=0.5,
                 binwidth = 0.05,
                 position = "identity")

# Creamos una tabla de una fila y dos columnas.
# La fila es la media y la desviaci�n que en teor�a debe
# tener la variable suma
parametros <- data.frame(medias=20,
                         desviaciones=sqrt(6))

# Construimos los tabulados correspondientes a
# cada uno de los renglones de par�metros (solo es un rengl�n)
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
                 binwidth = 0.1,
                 position = "identity",
                 alpha=0.5) + 
  geom_line(data=gaussianas,aes(x_val,y_val)) +
  xlim(-5,30)