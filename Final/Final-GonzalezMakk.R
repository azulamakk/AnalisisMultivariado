library(dplyr)
library(caret)

calculoIndicadores <- function(predicciones, etiquetas) {
  # Crear una matriz de confusión
  confusion_matrix <- confusionMatrix(predicciones, etiquetas)
  
  # Calcular Accuracy
  accuracy <- confusion_matrix$overall["Accuracy"]
  
  # Calcular Precision
  precision <- confusion_matrix$byClass["Precision"]
  
  # Calcular Recall (Sensitivity)
  recall <- confusion_matrix$byClass["Sensitivity"]
  
  # Calcular F1 Score
  f1_score <- confusion_matrix$byClass["F1"]
  
  # Crear un vector con los resultados
  indicadores <- c("Accuracy" = accuracy, "Precision" = precision, "Recall" = recall, "F1 Score" = f1_score)
  
  return(indicadores)
}

set.seed(13)
data(iris)
data <- iris %>% select(Sepal.Length, Petal.Length, Species)

# Crear partición de datos
split_index <- createDataPartition(data$Species, p = 0.8, list = FALSE)
train <- data[split_index, ]
test <- data[-split_index, ]

# Definir el espacio de parámetros
grid <- expand.grid(k = 1:10)

# Configurar el control de entrenamiento con 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Realizar grid search con cross-validation
model <- train(Species ~ Sepal.Length + Petal.Length, 
               data = train, 
               method = "knn", 
               trControl = ctrl,
               tuneGrid = grid)

# Obtener los resultados
print(model)

# Hacer predicciones con el mejor modelo
predictions <- predict(model, newdata = test)

# Calcular indicadores de rendimiento
indicadores <- calculoIndicadores(predictions, test$Species)

cat("Best k:", model$bestTune$k, "\n")
cat("Accuracy:", indicadores["Accuracy"], "\n")
cat("Precision:", indicadores["Precision"], "\n")
cat("Recall:", indicadores["Recall"], "\n")
cat("F1 Score:", indicadores["F1 Score"], "\n")

