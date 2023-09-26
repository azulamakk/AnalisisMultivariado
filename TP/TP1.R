winequality.red <- read.csv("~/Universidad/Analisis Multivariado/TP/winequality-red.csv")

library(tidyverse)
library(car)

DS2 <- winequality.red %>%
  filter(quality == 4 | quality == 6 | quality == 8) %>%
  sample_n(500, replace = FALSE, weight = NULL) %>%
  select("fixed.acidity", "density", "alcohol", "pH", "quality")

X <- DS2 %>%
  select("fixed.acidity", "density", "alcohol", "pH") %>%
  as.matrix

# Distancias de mahalanobis
mahala <- mahalanobis(X, colMeans(X), cov(X))
distanciasMahala <- tibble(Observation = 1:500, Mahalanobis = mahala)

distanciasMahala <- distanciasMahala %>%
  arrange(desc(Mahalanobis))

densidadMahala <- density(distanciasMahala$Mahalanobis)

densidadMahala <- ggplot(distanciasMahala, aes(x = Mahalanobis)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Densidad de Distancias de Mahalanobis",
    x = "Distancia de Mahalanobis",
    y = "Densidad"
  )

# Simulacion datos chi-cuadrado cuadrado
simulacionChi2 <- rchisq(n = 500, df = 5)

# Superposicion de datos chi cuadrado con distancias mahala
densidadMahala + 
  geom_density(data = data.frame(x = simulacionChi2), aes(x = x),
               fill = "red", alpha = 0.5) +
  labs(
    title = "Densidad de Distancias de Mahalanobis y Chi-cuadrado",
    x = "Distancia de Mahalanobis",
    y = "Densidad"
  ) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("Distancias de Mahalanobis", "Chi-cuadrado"))

# Boxi - Cox
result <- powerTransform(X, family = "bcPower")
lambda_optimo <- result$roundlam
X_transformed <- (X^lambda_optimo - 1) / lambda_optimo

