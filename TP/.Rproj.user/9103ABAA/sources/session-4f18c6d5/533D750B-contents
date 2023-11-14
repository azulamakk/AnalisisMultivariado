winequality.red <- read.csv("/cloud/project/winequality-red.csv")
source('Librerias y Funciones.R')

library("ggbeeswarm")
library('tidyverse')
library('dplyr')
library('GGally')
library('car')

#-------------------------Limpieza de datos
set.seed(13)
DS2 <- winequality.red %>% filter(winequality.red$quality==4 | winequality.red$quality==6 | winequality.red$quality==8)
DS2 <- DS2[sample(nrow(DS2), 500, replace = FALSE, prob = NULL),]
DS2 <- DS2 %>% select(c(fixed.acidity, density, alcohol, pH, quality))

#-------------------------2. Análisis descriptivo de los datos

#2.1. ----- fixed.acidity
x1 <- DS2$fixed.acidity

# El rango de los datos es el maximo menos el mínimo de la muestra.
minimo_x1 <- min(x1)
maximo_x1 <- max(x1)
rango_x1 <- maximo_x1 - minimo_x1

# Función de distribución empírica = Función de probabilidad acumulada
ggplot(DS2, aes(fixed.acidity)) + 
  stat_ecdf(geom = "point", 
            color="#00688B") +
  labs(title="Función de distribución empírica para Acidez Fija",
       y = "Probabilidad Acumulada", 
       x="Valores de Acidez") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Histograma - estima la función de densidad - Continuas
ggplot(DS2, aes(fixed.acidity)) + 
  geom_histogram(color="#104E8B",
                 fill = "#00688B",
                 bins = 15) +
  labs(title="Histograma para la Acidez Fija",
       y = "Frecuencia", 
       x="Valores de Acidez Fija") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Gráfico de densidad - Continuas 
ggplot(DS2, aes(fixed.acidity)) + 
  geom_density(color="#00688B", size = 2) +
  labs(title="Función de Densidad para la Acidez Fija",
       y = "Densidad", 
       x="Valores de Acidez Fija") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Media y mediana
media_x1 <- mean(x1)
mediana_x1 <- median(x1)
# Como la media es mayor a la mediana entonces la distribución tiene asimetria positiva

#Cuantiles y Rango intercuantil (RIC)
quantile(x1, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
ggplot(DS2, aes(y = fixed.acidity, x = frequency(fixed.acidity))) + 
  geom_beeswarm(aes(), size = 1,cex = 4.5, stroke = 0.2, col = "#00688B") +
  labs(title="Beeswarm y Boxplot para Acidez Fija",
       y = "Valores de Acidez Fija", 
       x= "") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot(color="black", 
               fill = "#00688B",
               alpha = 0.7)

#Desvío estándar
varianza_x1 <- sum((x1 - media_x1)^2)/500
desvio_estandar_x1 <- sqrt(varianza_x1)

#Coeficiente variación
coef_vari_x1 <- desvio_estandar_x1/media_x1

#Coeficiente de asimetría
coef_asim_x1 <- sum(((x1 - media_x1)/desvio_estandar_x1)^3)/500

#Coeficiente de kurtosis
coef_kurtosis_x1 <- sum(((x1 - media_x1)/desvio_estandar_x1)^4)/500


#2.2. ----- density
x2 <- DS2$density

# El rango de los datos es el maximo menos el mínimo de la muestra.
minimo_x2 <- min(x2)
maximo_x2 <- max(x2)
rango_x2 <- maximo_x2 - minimo_x2

# Función de distribución empírica = Función de probabilidad acumulada
ggplot(DS2, aes(density)) + 
  stat_ecdf(geom = "point", 
            color="#698B22") +
  labs(title="Función de distribución empírica para Densidad",
       y = "Probabilidad Acumulada", 
       x="Valores de Densidad") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 


#Histograma - estima la función de densidad - Continuas
ggplot(DS2, aes(density)) + 
  geom_histogram(color="#556B2F",
                 fill = "#698B22",
                 bins = 15) +
  labs(title="Histograma para la Densidad",
       y = "Frecuencia", 
       x="Valores de Densidad") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 


#Gráfico de densidad - Continuas 
ggplot(DS2, aes(density)) + 
  geom_density(color="#698B22", size = 2) +
  labs(title="Función de Densidad para la Densidad del vino",
       y = "Densidad", 
       x="Valores de Densidad del vino") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Gráficos de densidad sin outliers y con
extraer_outliers = function(x){
  sup = quantile(x,0.75, na.rm = T)+IQR(x, na.rm = T)*1.5 #bigote de la derecha
  inf = quantile(x,0.25, na.rm = T)-IQR(x, na.rm = T)*1.5 #bigote de la izquierda
  outliers <- na.omit(x[(x > sup) | (x<inf)])
  return(outliers)
}
outliers_x2 <- extraer_outliers(DS2$density)
chau_outliers <- DS2[!(DS2$density %in% outliers_x2),]

ggplot(NULL, aes(density)) + 
  geom_density(data = chau_outliers, color= "#C0FF3E", size = 2) +
  labs(title="Función de Densidad para la Densidad del vino",
       y = "Densidad", 
       x="Valores de Densidad del vino",
       subtitle = "(Verde oscuro = con outliers, Verde claro = sin outliers)") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) +
  geom_density(data = DS2, color = "#698B22", size = 2)
#En verde claro --> la distribución sin outliers
#En verde oscuro --> la distribución con outliers

#Media y mediana
media_x2 <- mean(x2)
mediana_x2 <- median(x2)
# Como la media y la mediana son iguales entonces la distribución es simetrica.

#Cuantiles y Rango intercuantil (RIC)
quantile(x2, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
ggplot(DS2, aes(y = density, x = frequency(density))) + 
  geom_beeswarm(aes(), size = 1,cex = 4.5, stroke = 0.2, col = "#698B22") +
  labs(title="Beeswarm y Boxplot para la Densidad",
       y = "Valores de la Densidad", 
       x= "") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot(color="black", 
               fill = "#698B22",
               alpha = 0.7)

#Desvío estándar
varianza_x2 <- sum((x2 - media_x2)^2)/500
desvio_estandar_x2 <- sqrt(varianza_x2)

#Coeficiente variación
coef_vari_x2 <- desvio_estandar_x2/media_x2

#Coeficiente de asimetría
coef_asim_x2 <- sum(((x2 - media_x2)/desvio_estandar_x2)^3)/500

#Coeficiente de kurtosis
coef_kurtosis_x2 <- sum(((x2 - media_x2)/desvio_estandar_x2)^4)/500


#2.3. ----- alcohol
x3 <- DS2$alcohol

# El rango de los datos es el maximo menos el mínimo de la muestra.
minimo_x3 <- min(x3)
maximo_x3 <- max(x3)
rango_x3 <- maximo_x3 - minimo_x3

# Función de distribución empírica = Función de probabilidad acumulada
ggplot(DS2, aes(alcohol)) + 
  stat_ecdf(geom = "point", 
            color="hotpink3",) +
  labs(title="Función de distribución empírica para Alcohol",
       y = "Probabilidad Acumulada", 
       x="Valores de Alcohol") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Histograma - estima la función de densidad - Continuas
ggplot(DS2, aes(alcohol)) + 
  geom_histogram(color="hotpink4",
                 fill = "hotpink3",
                 bins = 15) +
  labs(title="Histograma para el Alcohol",
       y = "Frecuencia", 
       x="Valores de Alcohol") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) +
  geom_vline(xintercept = mean(x3))

#Gráfico de densidad - Continuas 
ggplot(DS2, aes(alcohol)) + 
  geom_density(color="hotpink3", size = 2) +
  labs(title="Función de Densidad para el Alcohol",
       y = "Densidad", 
       x="Valores de Alcohol") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Media y mediana
media_x3 <- mean(x3)
mediana_x3 <- median(x3)
# Como la media y la mediana son iguales entonces la distribución es simetrica.

#Cuantiles y Rango intercuantil (RIC)
quantile(x3, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
ggplot(DS2, aes(y = alcohol, x = frequency(alcohol))) + 
  geom_beeswarm(aes(), size = 1,cex = 4.5, stroke = 0.2, col = "hotpink3") +
  labs(title="Beeswarm y Boxplot para el Alcohol",
       y = "Valores de la Alcohol", 
       x= "") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot(color="black", 
               fill = "hotpink3",
               alpha = 0.7)

#Desvío estándar
varianza_x3 <- sum((x3 - media_x3)^2)/500
desvio_estandar_x3 <- sqrt(varianza_x3)

#Coeficiente variación
coef_vari_x3 <- desvio_estandar_x3/media_x3

#Coeficiente de asimetría
coef_asim_x3 <- sum(((x3 - media_x3)/desvio_estandar_x3)^3)/500

#Coeficiente de kurtosis
coef_kurtosis_x3 <- sum(((x3 - media_x3)/desvio_estandar_x3)^4)/500


#2.4. ----- pH
x4 <- DS2$pH

# El rango de los datos es el maximo menos el mínimo de la muestra.
minimo_x4 <- min(x4)
maximo_x4 <- max(x4)
rango_x4 <- maximo_x4 - minimo_x4

# Función de distribución empírica = Función de probabilidad acumulada
ggplot(DS2, aes(pH)) + 
  stat_ecdf(geom = "point", 
            color="orange",) +
  labs(title="Función de distribución empírica para pH",
       y = "Probabilidad Acumulada", 
       x="Valores del pH") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Histograma - estima la función de densidad - Continuas
ggplot(DS2, aes(pH)) + 
  geom_histogram(color="orange3",
                 fill = "orange",
                 bins = 15) +
  labs(title="Histograma para el pH",
       y = "Frecuencia", 
       x="Valores del pH") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) +
  geom_vline(xintercept = mean(x4))

#Gráfico de densidad - Continuas 
ggplot(DS2, aes(pH)) + 
  geom_density(color="orange", size = 2) +
  labs(title="Función de Densidad para el pH",
       y = "Densidad", 
       x="Valores de pH") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 


#Media y mediana
media_x4 <- mean(x4)
mediana_x4 <- median(x4)
#Como la media y la mediana son iguales entonces la distribución es simetrica.

#Cuantiles y Rango intercuantil (RIC)
quantile(x4, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
ggplot(DS2, aes(y = pH, x = frequency(pH))) + 
  geom_beeswarm(aes(), size = 1,cex = 4.5, stroke = 0.2, col = "orange") +
  labs(title="Beeswarm y Boxplot para el pH",
       y = "Valores de pH", 
       x= "") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot(color="black", 
               fill = "orange",
               alpha = 0.7)

#Desvío estándar
varianza_x4 <- sum((x4 - media_x4)^2)/500
desvio_estandar_x4 <- sqrt(varianza_x4)

#Coeficiente variación
coef_vari_x4 <- desvio_estandar_x4/media_x4

#Coeficiente de asimetría
coef_asim_x4 <- sum(((x4 - media_x4)/desvio_estandar_x4)^3)/500

#Coeficiente de kurtosis
coef_kurtosis_x4 <- sum(((x4 - media_x4)/desvio_estandar_x4)^4)/500

#2.5. ----- Relación entre variables

X <- DS2 %>% #Seleccionamos solo los datos numéricos
  select("fixed.acidity", "density", "alcohol", "pH") %>%
  as.matrix
para_pairs <- X %>% as.data.frame()

ggpairs(para_pairs) +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_line(color = 'grey', size = 0.5),  
        axis.text.x = element_text(size = 10),  
        axis.ticks.y = element_line(color = 'grey', size = 0.5),  
        axis.text.y = element_text(size = 10))  + labs(title = "Gráfico de pairs")

#2.6. ----- Análisis de outliers
dist1 <- X[217,] #Dato con distancia 29.8
dist2 <- X[281,] #Dato con distancia 29.8
dist3 <- X[154,] #Dato con distancia 23.6
dist4 <- X[458,] #Dato con distancia 23.6
dist5 <- X[383,] #Dato con distancia 22.9
dist6 <- X[227,] #Dato con distancia 16.9
dist7 <- X[59,]  #Dato con distancia 16.5
dist1
dist2
dist3
dist4
dist5
dist6
dist7
#-------------------------3. Transformaciónes Previas

#3.1. ----- Ajuste Chi-cuadrado

mahala <- mahalanobis(X, colMeans(X), cov(X)) # Distancias de mahalanobis
distanciasMahala <- tibble(Observation = 1:500, Mahalanobis = mahala)

distanciasMahala <- distanciasMahala %>%
  arrange(desc(Mahalanobis))
densidadMahala <- density(distanciasMahala$Mahalanobis)
densidadMahala <- ggplot(distanciasMahala, aes(x = Mahalanobis)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Densidad de Distancias de Mahalanobis",
       x = "Distancia de Mahalanobis",
       y = "Densidad") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

simulacionChi2 <- rchisq(n = 500, df = 4) # Simulacion datos chi-cuadrado cuadrado

densidadMahala + # Superposición de datos chi cuadrado con distancias mahala
  geom_density(data = data.frame(x = simulacionChi2), aes(x = x),
               fill = "red", alpha = 0.5) +
  labs(title = "Densidad de Distancias de Mahalanobis y Chi-cuadrado",
       x = "Valor", 
       y = "Densidad") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Distancias de Mahalanobis", "Chi-cuadrado")) +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_line(color = 'grey', size = 0.5), 
        axis.text.x = element_text(size = 10)) 

#3.2. ----- Box y Cox
transformacion <- powerTransform(X, family='bcPower')

XTransformado <- data.frame(matrix(NA, nrow = nrow(X), ncol = ncol(X)))
colnames(XTransformado) <- colnames(X)

for(i in 1:4){
  XTransformado[, i] <- X[, i] ^ transformacion$lambda[i]
}

#3.4. ----- Inciso 3.1 con datos transformados 
mahalaTrans <- mahalanobis(XTransformado, colMeans(XTransformado), cov(XTransformado)) # Distancias de mahalanobis
distanciasMahalaTrans<- tibble(Observation = 1:500, Mahalanobis = mahalaTrans)

distanciasMahalaTrans <- distanciasMahalaTrans %>%
  arrange(desc(Mahalanobis))
densidadMahalaTrans <- density(distanciasMahalaTrans$Mahalanobis)
densidadMahalaTrans <- ggplot(distanciasMahalaTrans, aes(x = Mahalanobis)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Densidad de Distancias de Mahalanobis",
       x = "Distancia de Mahalanobis",
       y = "Densidad")+
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

densidadMahalaTrans + # Superposicion de datos chi cuadrado con distancias mahala
  geom_density(data = data.frame(x = simulacionChi2), aes(x = x),
               fill = "red", alpha = 0.5) +
  labs(title = "Densidad de Distancias de Mahalanobis de datos Transformados y Chi-cuadrado",
       x = "Distancia de Mahalanobis de datos Transformados",
       y = "Densidad") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Distancias de Mahalanobis de datos Transformados", "Chi-cuadrado"))+
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_line(color = 'grey', size = 0.5), 
        axis.text.x = element_text(size = 10))

#Gráfico de pairs con los datos transformados
para_pairs2 <- XTransformado %>% as.data.frame()
ggpairs(para_pairs2) +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_line(color = 'grey', size = 0.5),  
        axis.text.x = element_text(size = 10),  
        axis.ticks.y = element_line(color = 'grey', size = 0.5),  
        axis.text.y = element_text(size = 10))+
  labs(title="Grafico de pairs con los datos transformados") 

#-------------------------4. Componentes principales y biplot

#4.1. ----- Funcion PCA

PCA = function(data,k,scale=T){
  if(scale==T){
    data = as.matrix(scale(data))
  } else {
    data = as.matrix(scale(data,scale = F)) #solo centra
  }
  
  eig = eigen(cov(data)) #Obtenemos los ak son las dimensiones deseadas
  V = eig$vectors[,1:k]
  Z = data %*% V
  lambda = eig$values[1:k] #Reduccion a k dimesiones
  
  return(list(scores = Z, effects = V, autovalores = lambda)) #Los autovalores serian las varianzas
}

pca <- PCA(XTransformado, 2)
pca$scores #Los scores
pca$effects #La matriz de componentes principales
pca$autovalores #Varianzas de los componentes principales

quality <- DS2 %>% select(quality)
scoresCalidad <- cbind(pca$scores, quality) %>% rename('Comp1'=1, 'Comp2'=2)

#Graficamos los scores (sin ser biplot)
ggplot(scoresCalidad, aes(x = Comp1, y = Comp2, color = as.factor(quality))) +
  geom_point(shape = 20, size = 3) +  # Ajusta size para hacer que los puntos sean más grandes
  labs(
    x = "Componente 1",
    y = "Componente 2",
    title = "Scores segun Componente 1 y 2 por categoría"  
  ) +
  scale_color_discrete(name = "Calidad") +
  theme(
    text = element_text(family = "mono"),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
    panel.grid.major = element_line(color = 'grey'),
    panel.grid.minor = element_line(color = 'grey'),
    axis.ticks.x = element_line(color = 'grey', size = 0.5),
    axis.text.x = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) 

# Biplot
D <- 3*pca$effects
data <- data.frame(Comp1 = scoresCalidad$Comp1,
                   Comp2 = scoresCalidad$Comp2,
                   Quality = as.factor(scoresCalidad$quality))

flechas_df <- data.frame(origen_1 = rep(0, 4),
                         origen_2 = rep(0, 4),
                         destino_1 = D[1:4, 1],
                         destino_2 = D[1:4, 2])

p <- ggplot(data, aes(x = Comp1, y = Comp2)) +
  geom_point(shape = 20, size=3) +
  labs(title = "Biplot del PCA", x = "Componente 1", y = "Componente 2")

# Agregar las flechas desde las coordenadas de origen a destino
p + geom_segment(data = flechas_df, aes(x = origen_1, y = origen_2, xend = destino_1, yend = destino_2),
                 color = "firebrick", arrow = arrow(type = "closed", length = unit(0.1, "inches")), size = 1.5) +
  geom_text(data = flechas_df, aes(x = destino_1, y = destino_2, label = colnames(X)), vjust = -0.5) +
  theme(
    text = element_text(family = "mono"),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),  # Ajusta el tamaño del título aquí
    panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
    panel.grid.major = element_line(color = 'grey'),
    panel.grid.minor = element_line(color = 'grey'),
    axis.ticks.x = element_line(color = 'grey', size = 0.5),
    axis.text.x = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) 

#Calculamos el R-cuadrado
sum(pca$autovalores)/sum(PCA(XTransformado, 4)$autovalores) #Como R-cuadrado nos da mayor a 0.8 entonces el modelo de 2 dimensiones sirve.
#Los dos primeros componentes explican el 84,5% de la variabilidad total

#Matriz de correlaciones entre las Yi y las componentes principales
cor(XTransformado, pca$scores)

cor_heatmap(cor(XTransformado, pca$sc))
