drogas <- read.csv("~/Universidad/Analisis Multivariado/Parcial/drogas.csv")

scatter_pares <- function(data){
  panel.hist <- function(x, ...)
  {
    usr <- par("usr")
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "lightblue", ...)
  }
  
  pairs(data, lower.panel = panel.smooth,
        upper.panel= NULL, pch=20, lwd=2, diag.panel = panel.hist,
        cex.labels = 1.3)
  
}

library(dplyr)
library(corrplot)
X <- drogas %>% select(DrugUse, BingeDrink, Poverty, HSdrop) %>% as.matrix()

pairs(X)

# --------------------- a 
correlaciones <- cor(X)

corrplot(cor(X), method = "color", type = "upper")


# --------------------- b
datosEstandarizados <- scale(X)

covarianzasEst <- cov(datosEstandarizados)

# Cálculo de los autovalores y autovectores de la matriz de covarianzas
eigen_resultEst = eigen(covarianzasEst) 

# Ordenar los autovalores y autovectores: Se ordenan los autovalores de mayor a menor
autovectoresEst = eigen_resultEst$vectors[ , 1:2]

# Transformación lineal de los datos
scoresEst = datosEstandarizados %*% autovectoresEst

# Matriz de covarianzas de los componentes principales
matrizCovarianzasPCAEst <- diag(eigen_resultEst$values)

covarianzasEst
matrizCovarianzasPCAEst

sumaAutovaloresEst <- 1.841361 + 1.112605 + 0.7839599 + 0.2620743
suma2CompEst <- 1.841361 + 1.112605 
varTotalExplicadaEst <- suma2CompEst/sumaAutovaloresEst


# --------------------- c
print(autovectoresEst)


# --------------------- d
plot(scoresEst, xlab="Componente Principal 1", ylab="Componente Principal 2", main="Gráfico de Scores PCA")
V <- eigen_resultEst $vectors[,1:2]
Z <- datosEstandarizados %*% V 

nombres <- drogas$State
plot(Z, pch=20, xlab='Z1', ylab='Z2', xlim = c(-3,3),
     ylim = c(-2,3.5))

O <- cbind(rep(0,5), rep(0,5))
D <- 3*cbind(V[,1], V[,2])

origen_1 <- O[,1]
origen_2 <- O[,2]
destino_1 <- D[,1]
destino_2 <- D[,2]

segments(origen_1,origen_2,destino_1,destino_2)
arrows(origen_1,origen_2,destino_1,destino_2, length = 0.1, col = 'firebrick', lwd=2)
text(D, colnames(datosEstandarizados), offset=3, col='firebrick')
text(Z, nombres, cex = 0.7, col='grey50')
