data <- read.delim("~/Universidad/Analisis Multivariado/datasets/trade.txt")

#----a) Hacemos un scatterplot con los datos centrados que estan en una matriz

X <- as.matrix(scale(data,center=TRUE, scale= FALSE))
plot(X, pch=20)

#----b) Graficamos una recta que pase sobre el origen para proyectar la nube de datos
# Armamos un vector que indica la dirección de proyección

a <- matrix(c(1/2,1/2)*sqrt(2)) #Nuestro vector de coeficientes.  Le ponemos el matrix para que sea un vector columna
slope = a[2,]/a[1,]
abline(a = 0, b = slope, col='grey50')

#----c) Ecuanción que permite transformar X1 y x2 a Y:
# Queremos pasar todo de dos dimensiones a una dimension, para eso 
# Armamos una combinación lineal de las variables para armar nuestra unica variable aleatoria
# Y = 1/raiz(2)*(X1 - media_X1) + 1/raiz(2)*(X2 - media_X2)

# El vector aleatorio (que tiene dos dimensiones), cuando lo multiplicamos por un vector de coeficientes
#vamos a armar nuestra variable de una dimensión por combinación lineal.

#----d) Armamos nuestro vector aleatorio de nuestra nueva variable Y (que es un resumen de nuestras dos variables)

Y <- X %*% a

#----e) Histograma y varianza muestral de Y

hist(Y, freq=FALSE) #Hay muchos que tienen importaciones y exportaciones bajas
lines(density(Y))
var(Y)

#----f) Buscamos la distancia ortogonal (perpendicular a la recta) de cada residuo
#Buscamos la matriz de proyección P = a*a_transpuesta

P <- a %*% t(a) #Es una matriz de transformación lineal
X_reducido <- X %*% P
abline(a = 0, b = slope, col='grey50')
points(X_reducido, pch=20, col="red")

#Los puntos rojos (que son las proyecciones de los puntos) se ven encojidos y es por la línea de X_reducido
#La norma o magnitud de un vector conserva como se van a ver los puntos, la norma tiene que ser 1 para que queden bien los puntos

a <- sqrt(2) * a #Aca cambiamos la magnitud de a y volvemos a hacer todo de vuelta
P <- a %*% t(a)
X_reducido <- X %*% P
plot(X, pch = 20)
abline(a = 0, b = slope, col='grey50')
points(X_reducido, pch=20, col="red")

# Definamos una nueva variable E que resuma el error de representacion de cada pais
# E = 1/raiz(2)*(X1 - media_X1) - 1/raiz(2) *(X2 - media_X2)

b <- matrix(c(-1/2,1/2)* sqrt(2))
E <- X %*% b #Tiene los errores de representacion de los puntos.  La distancia entre el punto con su punto ortogonal en la recta.

#----g) Encontrar la matriz T que al multiplicarla por la matriz X (de las importaciones y exportaciones) nos de la matriz de YE (los valores de Y con su error)
# Sabemos que T es ortonormal : que son ortogonales y de magnitud = 1

Te <- cbind(a, b)
W <- X %*% Te #A los datos X les aplicamos una transformacion lineal

#----h) El scatter de W y las covarianzas de X y W
plot(W, pch=20) #Se puede ver cómo rotamos la nube para que se vea horizontal
#Sirve para ver hacia donde tienen los datos

cov(X)
cov(W) #Toda la información se concentro en la primera porque en el eje x hay mas puntos lejos del 0
#La suma de las diagonales se sigue manteniendo? ahora si pero no se si se cumple siempre

#Hacemos el experimento de buscar un nuevo vector para garantizar que si no es ortogonal entonces hay una alta correlación
ce <- matrix(c(1,2)/sqrt(5))
plot(X %*% cbind(a, ce), pch=20)


#Resumen: armar combinaciones de variables nos sirve para resumir la información