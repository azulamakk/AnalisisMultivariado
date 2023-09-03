#install.packages("tidyverse")
#install.packages("dplyr")
install.packages("corrplot")
library("corrplot")
library("tidyverse")
library("dplyr")

options(scipen=999)

data <- melbourne_filtrado

#-------------T1.1
#-----a) Pedro dijo que nos quedamos con las 4 varibles da abajo
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

#-----c)

#Para vizualizar scatterplots de varias dimenciones se usa la función pairs(matriz)
pairs(X)
#Los datos presentan mucha asimetría, outliers que joden, la falta de linealidad.
#Hay algunas lineales pero ninguna normal.

#-----d)

#De cada vector aleatorio se puede estimar la esperanza 

colMeans(X) #Vector de medias muestrales, que se llama el vector de esperanzas

#Varianza muestral marginal
apply(X,2,var)

#Se puede calcular la media muestral de una columna de una matriz, la marianza muestral marginal de una matriz.

#La covarianza puede servir para evaluar si hay una recta con asociación negativa o positivas.

cov(X) #Se le llama matriz de varianzas y covarianzas = S
#No nos da información porque las magnitudes son re chotas, pero el signo si nos dicen
#En la diagonal de la matriz de covarianzas son las varianzas.

#Para poder elavuar mejor los resultados de la matriz de covarianzas vamos a estanderizar los valores de la matriz.
scale(X) #Estandarizamos.  La unidades estan expresadas en desvíos

#-----e)

#La covarianza de los valores estandarizados de la matriz X sirve para tener la matriz de correlaciones
cov(scale(X)) #Que es lo mismo de cor(X)

#El tema aca es que no nos dejemos engañar.  No es tan lineal.  
#Porque quizás el coeficiente nos da 0, pero igual hay algún tipo de dependencia entre las variables.

#Un heatmap le pone colores a las relaciones
covarianzasEscaladas <- cov(scale(X))
corrplot(covarianzasEscaladas, method = "ellipse")

#-----f)

X_filtrado <- data %>%
  filter(Landsize < 2000) %>%
  select(Price, Landsize, BuildingArea, Age) %>%
  as.matrix

pairs(X_filtrado) 

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

#-----a)

View(scale(X)) #Para ver los primeros 5
pairs(scale(X))

#-----b)

mahala <- mahalanobis(X, colMeans(X), cov(X))
View(data_frame(1:1184,mahala)) #Para ver los primeros 5 


