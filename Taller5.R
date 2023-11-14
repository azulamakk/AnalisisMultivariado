source('Librerias y Funciones.R')

# Ejercicio paises mundo
paises_mundo <- read.csv("/cloud/project/datasets/paises_mundo.csv")

nombres <- paises_mundo$Nombre
data <- paises_mundo[,-1]

# Componentes principales (reducción a dos dimensiones)
pca <- prcomp(data)



# Somos grupo 3, por lo que tenemos que aplicar logaritmo y estandarizar
dataLog <- log(data)
dataLogEstandarizada <- scale(dataLog)

X <- as.matrix(dataLogEstandarizada)
pairs(dataLogEstandarizada, pch=20) 

# Tres variables:
# 1 - Datos originales,

# -- Paso 1) Obtenemos matriz de covarianzas
sigma_hat <- cov(X)

# -- Paso 2) Obtenemos los autovectores y los autovalores
eig <- eigen(sigma_hat)

#Queremos reducir a dos dimesiones
V <- eig$vectors[,1:2]  # Matriz de rotacion
lambda <- diag(eig$values[1:2])

# -- Paso 3) Obtener los 'escores'
Z <- X %*% V 
plot(Z, pch=20, xlab='Z1', ylab='Z2', xlim = c(-4,5),
     ylim = c(-4,4))
sum(eig$values[1:2])/sum(eig$values) # R-cuadrado nos da mayor a 0.8

# Proyecta los vectores canonicos la transformacion que le hicimos a los datos.
O <- cbind(rep(0,5), rep(0,5))
D <- 3*cbind(V[,1], V[,2])

origen_1 <- O[,1]
origen_2 <- O[,2]
destino_1 <- D[,1]
destino_2 <- D[,2]

segments(origen_1,origen_2,destino_1,destino_2)
arrows(origen_1,origen_2,destino_1,destino_2, length = 0.1, col = 'firebrick', lwd=2)
text(D, colnames(dataLogEstandarizada), offset=3, col='firebrick')
text(Z, nombres, cex = 0.7, col='grey50')

heatmap(cor(dataLogEstandarizada, Z), Rowv = NA, Colv = NA)
cor_heatmap(cor(dataLogEstandarizada, Z))

# A veces hacer reduccion antes de hacer clustering esta bueno
# 
# Todo esto puede ser reemplazado por la funcion biplot
biplot(pca)

