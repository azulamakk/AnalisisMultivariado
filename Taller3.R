source('Librerias y Funciones.R')

data <- read.csv("/cloud/project/datasets/mercurio.csv")

#----a) Visualiza en 3 dimensiones los datos

X <- data[,1:3]
library(plotly)
plot3d(X, col=as.factor(data$etiqueta))

#----b) tarea

#----c) Aplicar transformaciones porque pinta para ver que pasa

dataTransformada <- data.frame('w1' = sqrt(data$alcalinidad),
                               'w2' = sqrt(data$clorofila),
                               'w3' = log(data$mercurio)) #Quedaron transformadas 

plot3d(dataTransformada, col=as.factor(data$etiqueta))

# Cambiar la escala de los datos transformandolos con logaritmo o otra función no lineal corrige asimetría y corrige linealidad
# Lo que se ve en este plot 3d es que, a diferencia del plot de los datos sin transformar, se mejora muchisimo la normalidad.

#----d) Matriz de correlaciones de datos originales vs. datos transformados

cor(X)
cor(dataTransformada) #Hay que tener cuidado porque quizas vemos que no hay correlación lineal pero hay otro tipo de correlación

#----e) Se propone realizar una transformación lineal al vector w = (w1 w2 w3)t cuyas direcciones 
# de proyección estan en las columnas de A
W <- as.matrix(dataTransformada)
A <- cbind(c(-3/4, -2/3, 1/5), c(2/3, -4/5, -1/20)) #La cantidad de columnas en A te dice la cantidad de dimensiones que van a tener los vectores resultantes.

plot(W %*% A, xlim=c(-20,10), ylim=c(-20,10), col=data$etiqueta+1, pch=20)

crossprod(A[,1], A[,2])
crossprod(A[,1], A[,1])
crossprod(A[,1], A[,1])