data <- read.csv("~/Universidad/Analisis Multivariado/datasets/mercurio.csv")

plot3d <- function(X,col=NULL,id=NULL, size=6, cube=TRUE){
  
  library(plotly)
  library(RColorBrewer)
  
  n <- nrow(X)
  p <- ncol(X)
  
  data <- data.frame(scale(X, scale=FALSE))
  names(data) <- c('x1','x2','x3')
  
  if(is.null(col)==TRUE){
    data$col <- rep('black',n)
  } else {
    data$col <-col}
  
  if(is.null(id)==TRUE){
    data$id<-1:n
  } else {data$id <- id}
  
  fig <- plot_ly(data,
                 x = ~data[,1], y = ~data[,2], z = ~data[,3],
                 colors = brewer.pal(p,'Set1'), text=~id,
                 marker = list(size=size))
  fig <- fig %>% add_markers(color = ~col)
  fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(X)[1],
                                                  range = c(min(data$x1),max(data$x1))),
                                     yaxis = list(title = colnames(X)[2],
                                                  range = c(min(data$x2),max(data$x2))),
                                     zaxis = list(title = colnames(X)[3],
                                                  range = c(min(data$x3),max(data$x3))),
                                     aspectmode = ifelse(cube==TRUE,'cube','auto')))
  fig
  
}

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