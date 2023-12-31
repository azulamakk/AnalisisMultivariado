#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("corrplot")
library("corrplot")
library("tidyverse")
library("dplyr")

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

options(scipen=999)

data <- read.csv("~/Universidad/Analisis Multivariado/datasets/melbourne_filtrado.csv")

#-------------T1.1
#-----a) 

#-----b)

X <- data %>%
  select(Price, Landsize, BuildingArea, Age) %>%
  as.matrix

#Hacemos una matriz para poder hacer las regresiones.  Una matriz tiene solo variables numericas mientras que un dataframe
#En cada línea de la matriz va a haber una observación
#En las columnas van a estar la cantidad de variables

#En estadística aplicada, pensabamos los datos como un vector numérico, pero en esta materia los datos son matrices de datos.
#Trabajamos con vectores aleatorios que se representan así x = (x,y,z), siendo x, y, z variables o columnas de la matriz, o variables aleatorias marginales.
#Analisis marginal de una variables es aislarla de una matriz y analizar la distribución por su cuenta
#Cada fila de una matriz es una observación de un vector aleatorio.


#Centrar las variables es restarles sus propias medias a cada variable
scale(X,center=TRUE, scale= FALSE)

#-----c)Realizar un gráfico de pares con las variables de X e 
# interpretar las relaciones entre ellas, ¿Presentan linealidad? 
# Observar si las distribuciones se ajustan bien a un modelo Normal.

#Para vizualizar scatterplots de varias dimenciones se usa la función pairs(matriz)
pairs(X)
scatter_pares(X)
#Los datos presentan mucha asimetría, outliers que joden, la falta de linealidad.
#Hay algunas lineales pero ninguna normal.

#-----d)A partir de los datos, obtener el vector de medias muestrales x 
# y la matriz de varianzas y covarianzas, ¿A quienes estiman?

#De cada vector aleatorio se puede estimar la esperanza 

colMeans(X) #Vector de medias muestrales, que se llama el vector de esperanzas

#Varianza muestral marginal
apply(X,2,var)

#Se puede calcular la media muestral de una columna de una matriz, la marianza muestral marginal de una matriz.

#La covarianza puede servir para evaluar si hay una recta con asociación negativa o positivas.

cov(X) #Se le llama matriz de varianzas y covarianzas = S
#No nos da información porque las magnitudes son re chotas, pero el signo si nos dicen
#En la diagonal de la matriz de covarianzas son las varianzas.

# Esta matriz proporciona información sobre la variabilidad y las relaciones lineales 
# entre variables en un conjunto de datos, siendo esencial para realizar inferencias, 
# construir modelos y comprender la estructura de dependencia en diversas disciplinas.
#Para poder elavuar mejor los resultados de la matriz de covarianzas vamos a estanderizar los valores de la matriz.
scale(X) #Estandarizamos.  La unidades estan expresadas en desvíos

#-----e) Calcular la matriz de correlaciones muestrales y 
# representarlas en un heatmap. ¿Hasta qué punto sirve este resultado?

#La covarianza de los valores estandarizados de la matriz X sirve para tener la matriz de correlaciones
cov(scale(X)) #Que es lo mismo de cor(X)

#El tema aca es que no nos dejemos engañar.  No es tan lineal.  
#Porque quizás el coeficiente nos da 0, pero igual hay algún tipo de dependencia entre las variables.

#Un heatmap le pone colores a las relaciones
covarianzasEscaladas <- cov(scale(X))
corrplot(covarianzasEscaladas, method = "ellipse")

# Limitaciones:
# - Sensibilidad a la linealidad: La matriz de correlaciones solo mide relaciones lineales entre variables. 
# Si existen relaciones no lineales, la correlación puede no capturarlas adecuadamente.
# - Influencia de valores atípicos: Los valores atípicos pueden afectar significativamente las estimaciones 
# de correlación, especialmente en conjuntos de datos pequeños.
# - No implica causalidad: La correlación no implica causalidad. Dos variables pueden estar correlacionadas, 
# pero esto no significa necesariamente que una causa la otra.

#-----f) Repetir los últimos tres incisos si se decide eliminar 
# las propiedades con Landside mayor a 2000. Detección de datos atípicos

X_filtrado <- data %>%
  filter(Landsize < 2000) %>%
  select(Price, Landsize, BuildingArea, Age) %>%
  as.matrix

pairs(X_filtrado) 
scatter_pares(X_filtrado)
#La diferencia entre el gráfico anterior con este de los datos filtrados es porque se sacaron algunos outliers que afectaban mucho en la tendencia.

#-------------T1.2

# Para una sola variable --> Para evaluar outliers se pueden estandarizar los datos y graficarlos para verlo visualmente 
# Para un vector con muchas variables --> Hay varias formas

# 1era forma: Distancia euclídea: Vamos a usar la distancia de los vectores al centro de la media de las distancias de los vectores para evaluar outliers
#Esta forma no sirve cuando hay una especie de correlación entre las variables.  No es útil siempre.
#Se usa cuando tenemos idealmente distribuciones normales en forma de circulo (con cieta independencia)

# 2da forma: Distancia de Mahalanobis
# Se usa para cuando tenemos idealmente distribuciones normales (con una dependencia).

#----Podemos encontrar dos tipos de outliers:

#1. Outliers marginales: se calculan con los z (estandarizando).  Rompen con alguna de de las variables
#2. Outliers "de nube": se calcula con Mahalanobis. Rompen con la correlación (no necesariamente con las variables)

#-----a) Identificar datos atípicos marginales inspeccionando las variables estandarizadas.

View(scale(X)) #Para ver los primeros 5
pairs(scale(X))

#-----b) Identificar datos atípicos realizando un ranking de las distancias de Mahalanobis de los datos a la media muestral. 
# Interpretar cualitativamente los datos atípicos

mahala <- mahalanobis(X, colMeans(X), cov(X))
View(data_frame(1:1184,mahala)) #Para ver los primeros 5 


