data <- read.csv("~/Universidad/Analisis Multivariado/datasets/churn_mini.csv")
data <- data[,-1]

#-----T5.1

#Distancia de Gower es una medida de disimilitud de dos cosas (en este dataset de personas)
#dij es un numero entre 0 y 1.  Si la disimilitud
#Sij es una medida de similitud.  Si al variables es categorica va a dar 1 si es similar y 0 cuando no lo es.
#Si la variable es cuantitativa
#w es el peso que se le asigna a cada variable
#k es la variable que vamos a estar analizando
#i y j son las dos personas que vamos a estar calculandole la distancia de gower
p <- ncol(data)
w <- rep(1,p)

#Antes hago que mis variables categoricas sean categoricas asi me lo toma el R
data$Geography <- as.factor(data$Geography)
data$IsActiveMember <- as.factor(data$IsActiveMember)

distancia_gower <- function(data, w){
  n <- nrow(data)
  p <- ncol(data)
  dist <- matrix(0, n, n) #Creo la matriz vacia
  ranges <- sapply(data, function(x) if (!is.factor(x)) diff(range(x)) else NA)
  
  for(i in 1:n){
    for(j in 1:n){#Porque quiero crear la matriz n x n que compara las distancias entre todos
      a <- 0
      for(k in 1:p){#Porque tengo que ir por cada variable de la tabla
        if(is.factor(data[, k]) == TRUE){#si la variable es categorica
          s <- as.numeric((data[i, k] == data[j, k]))
        } else { #si la variables es numerica
          s <- 1 - abs(data[i, k] - data[j, k])/ranges[k]
        }
        a <- a + w[k] * s
      }
      dist[i,j]= 1 - (a / sum(w)) #Cada elemento de la matriz es Dij
    }
  }
  
  return(dist)
}

distancia_gower(data,w=rep(1,7))

#Graficamos 
delta<-distancia_gower(data, w = rep(1,7)) 
mds <- cmdscale(as.dist(delta)) #agarra una matriz de dimisimilitares, y nos la transforma en un mapa
plot(mds, pch = 20, type='n', xlim=c(-0.4, 0.4),
     ylim=c(-0.3,0.3))
text(mds,rownames(data))
#Los puntos que estan mas cercas son los mas parecidos (en cuanto a sus caracteristicas).  La distancia entre puntos es la distancia uclidea. 

#Ahora quiero cambiar el peso a las variables y ver como influencian en sus disimilitudes
mds <- cmdscale(as.dist(distancia_gower(data, w=c(1,1,1,1,1,10,1)))) #Quiero ver que pasa si le pongo mucho peso a la variable de IsActive
text(mds,rownames(data),col='firebrick') #Los colores marcan los pesos cambiados
#Se puede ver que se re dividen del lado izquierdo y del derecho

#Confiamos mas en ver las distancias en 3d
plot3d(cmdscale(as.dist(delta),k=3),
       cube=FALSE)


