#install.packages("KernelKnn")
library(KernelKnn)
library(ggplot2)
library(cowplot)

#----Limpieza de datos 
X = scale(sample_data[, -ncol(sample_data)])
y = sample_data[, ncol(sample_data)]

y = c(1:length(unique(y)))[match(sample_data$loan_grade, sort(unique(sample_data$loan_grade)))]

set.seed(42)
spl_train = sample(1:length(y), round(length(y) * 0.80))
spl_test = setdiff(1:length(y), spl_train)

#----Funcion para calcular indicadores
calculo_indicadores <- function(matriz) {
  
  #Calculando el accuracy
  num_predicciones_correctas <- sum(diag(matriz))
  total_predicciones <- sum(matriz)
  accuracy <- num_predicciones_correctas / total_predicciones
  
  #Calculando la precision
  precision_A <- matriz[1, 1] / sum(matriz[1, ])
  precision_B <- matriz[2, 2] / sum(matriz[2, ])
  precision_C <- matriz[3, 3] / sum(matriz[3, ])
  precision_global <- sum(diag(matriz)) / sum(matriz)
  
  #Calculando el recall
  recall_A <- matriz[1, 1] / sum(matriz[, 1])
  recall_B <- matriz[2, 2] / sum(matriz[, 2])
  recall_C <- matriz[3, 3] / sum(matriz[, 3])
  recall_global <- sum(diag(matriz)) / sum(matriz)
  
  #Calculando el F1
  F1 <- 2*((precision_global*recall_global)/(precision_global+recall_global))
  
  #Calculando la correlación de Matthews
  num_true_positives <- matriz[2, 2]  # Verdaderos Positivos
  num_false_positives <- matriz[1, 2]  # Falsos Positivos
  num_false_negatives <- matriz[2, 1]  # Falsos Negativos
  mcc <- (num_true_positives * matriz[1, 1] - num_false_positives * num_false_negatives) /
    sqrt((num_true_positives + num_false_positives) * (num_true_positives + num_false_negatives) *
           (matriz[1, 1] + num_false_positives) * (matriz[1, 1] + num_false_negatives))
  
  indicadores <- list("Matriz de Confusion" = matriz,
                      "Accuracy" = accuracy,
                      "Precision" = precision_global,
                      "Recall" = recall_global,
                      "F1 Score" = F1,
                      "Matthews Correlation Coefficient" = mcc)
  return(indicadores)
}

#--------DISTANCIA EUCLIDIAN
# Inicializar variables para el mejor k y el mejor accuracy
best_k <- 0
k_values <- numeric()
best_accuracy <- -Inf
accuracy_values <- numeric()

# Bucle para probar diferentes valores de k con EUCLIDIAN
for (k in 1:20) {  # Prueba desde k = 1 hasta k = 20
  preds_VAL = KernelKnn(X[spl_train, ], TEST_data = X[spl_test, ], y[spl_train], k = k, 
                        method = 'euclidean', weights_function = NULL, regression = F,
                        Levels = unique(y))
  
  predicted_classes_val <- colnames(preds_VAL)[max.col(preds_VAL, ties.method = "random")]
  predicted_classes_val <- gsub("class_1", "A", predicted_classes_val)
  predicted_classes_val <- gsub("class_2", "B", predicted_classes_val)
  predicted_classes_val <- gsub("class_3", "C", predicted_classes_val)
  predicted_classes_val <- factor(predicted_classes_val, levels = c("A", "B", "C"))
  
  true_labels_val <- y[spl_test]
  predicted_labels_val <- as.character(predicted_classes_val)
  conf_matrix_val <- table(Actual = true_labels_val, Predicted = predicted_labels_val)
  indicadores_val <- calculo_indicadores(conf_matrix_val)
  
  accuracy_values <- c(accuracy_values, indicadores_val$Accuracy)
  k_values <- c(k_values, k)
  
  # Comparar con el mejor accuracy
  if (indicadores_val$Accuracy > best_accuracy) {
    best_accuracy <- indicadores_val$Accuracy
    best_k <- k
  }
}

#----K-NN con EUCLIDEAN
preds_TEST = KernelKnn(X[spl_train, ], TEST_data = X[spl_test, ], y[spl_train], k = best_k , 
                       method = 'euclidean', weights_function = NULL, regression = F,
                       Levels = unique(y))

predicted_classes <- colnames(preds_TEST)[max.col(preds_TEST, ties.method = "random")]
predicted_classes <- gsub("class_1", "A", predicted_classes)
predicted_classes <- gsub("class_2", "B", predicted_classes)
predicted_classes <- gsub("class_3", "C", predicted_classes)
predicted_classes <- factor(predicted_classes, levels = c("A", "B", "C"))

true_labels <- y[spl_test]
predicted_labels <- as.character(predicted_classes)
conf_matrix1 <- table(Actual = true_labels, Predicted = predicted_labels)
indicadores <- calculo_indicadores(conf_matrix)

results_df_euclidean <- data.frame(k = k_values, accuracy = accuracy_values)
ggplot(results_df_euclidean, aes(x = k, y = accuracy)) +
  geom_line(color = "blue", size = 1) +
  labs(
    x = "Valor de k",
    y = "Accuracy",
    title = "Accuracy de los distintos valores de k",
    subtitle = "Usando la distancia euclidiana"
  )

print("CON DISTANCIA EUCLIDIANA:")
print(paste("Best k:", best_k))
print("Matriz de Confusion:")
print(conf_matrix1)
print(paste("Accuracy por formula:", indicadores$Accuracy))
print(paste("Precision:", indicadores$Precision))
print(paste("Matthews Correlation Coefficient:", indicadores$`Matthews Correlation Coefficient`))
print(paste("F1 Score:", indicadores$`F1 Score`))

#--------DISTANCIA MANHATTAN
# Inicializar variables para el mejor k y el mejor accuracy
best_k <- 0
k_values <- numeric()
best_accuracy <- -Inf
accuracy_values <- numeric()

# Bucle para probar diferentes valores de k con EUCLIDIAN
for (k in 1:20) {  # Prueba desde k = 1 hasta k = 20
  preds_VAL = KernelKnn(X[spl_train, ], TEST_data = X[spl_test, ], y[spl_train], k = k, 
                        method = 'manhattan', weights_function = NULL, regression = F,
                        Levels = unique(y))
  
  predicted_classes_val <- colnames(preds_VAL)[max.col(preds_VAL, ties.method = "random")]
  predicted_classes_val <- gsub("class_1", "A", predicted_classes_val)
  predicted_classes_val <- gsub("class_2", "B", predicted_classes_val)
  predicted_classes_val <- gsub("class_3", "C", predicted_classes_val)
  predicted_classes_val <- factor(predicted_classes_val, levels = c("A", "B", "C"))
  
  true_labels_val <- y[spl_test]
  predicted_labels_val <- as.character(predicted_classes_val)
  conf_matrix_val <- table(Actual = true_labels_val, Predicted = predicted_labels_val)
  indicadores_val <- calculo_indicadores(conf_matrix_val)
  
  accuracy_values <- c(accuracy_values, indicadores_val$Accuracy)
  k_values <- c(k_values, k)
  
  # Comparar con el mejor accuracy
  if (indicadores_val$Accuracy > best_accuracy) {
    best_accuracy <- indicadores_val$Accuracy
    best_k <- k
  }
}

#----K-NN con MANHATTAN
preds_TEST = KernelKnn(X[spl_train, ], TEST_data = X[spl_test, ], y[spl_train], k = best_k , 
                       method = 'manhattan', weights_function = NULL, regression = F,
                       Levels = unique(y))

predicted_classes <- colnames(preds_TEST)[max.col(preds_TEST, ties.method = "random")]
predicted_classes <- gsub("class_1", "A", predicted_classes)
predicted_classes <- gsub("class_2", "B", predicted_classes)
predicted_classes <- gsub("class_3", "C", predicted_classes)
predicted_classes <- factor(predicted_classes, levels = c("A", "B", "C"))

true_labels <- y[spl_test]
predicted_labels <- as.character(predicted_classes)
conf_matrix2 <- table(Actual = true_labels, Predicted = predicted_labels)
indicadores <- calculo_indicadores(conf_matrix)

results_df_manhattan <- data.frame(k = k_values, accuracy = accuracy_values)
ggplot(results_df_manhattan, aes(x = k, y = accuracy)) +
  geom_line(color = "blue", size = 1) +
  labs(
    x = "Valor de k",
    y = "Accuracy",
    title = "Accuracy de los distintos valores de k",
    subtitle = "Usando la distancia manhattan"
  )

print("CON DISTANCIA MANHATTAN:")
print(paste("Best k:", best_k))
print("Matriz de Confusion:")
print(conf_matrix2)
print(paste("Accuracy por formula:", indicadores$Accuracy))
print(paste("Precision:", indicadores$Precision))
print(paste("Matthews Correlation Coefficient:", indicadores$`Matthews Correlation Coefficient`))
print(paste("F1 Score:", indicadores$`F1 Score`))

#--------DISTANCIA CHEBYSHEV
# Inicializar variables para el mejor k y el mejor accuracy
best_k <- 0
k_values <- numeric()
best_accuracy <- -Inf
accuracy_values <- numeric()

# Bucle para probar diferentes valores de k con CHEBYSHEV
for (k in 1:20) {  # Prueba desde k = 1 hasta k = 20
  preds_VAL = KernelKnn(X[spl_train, ], TEST_data = X[spl_test, ], y[spl_train], k = k, 
                        method = 'chebyshev', weights_function = NULL, regression = F,
                        Levels = unique(y))
  
  predicted_classes_val <- colnames(preds_VAL)[max.col(preds_VAL, ties.method = "random")]
  predicted_classes_val <- gsub("class_1", "A", predicted_classes_val)
  predicted_classes_val <- gsub("class_2", "B", predicted_classes_val)
  predicted_classes_val <- gsub("class_3", "C", predicted_classes_val)
  predicted_classes_val <- factor(predicted_classes_val, levels = c("A", "B", "C"))
  
  true_labels_val <- y[spl_test]
  predicted_labels_val <- as.character(predicted_classes_val)
  conf_matrix_val <- table(Actual = true_labels_val, Predicted = predicted_labels_val)
  indicadores_val <- calculo_indicadores(conf_matrix_val)
  
  accuracy_values <- c(accuracy_values, indicadores_val$Accuracy)
  k_values <- c(k_values, k)
  
  # Comparar con el mejor accuracy
  if (indicadores_val$Accuracy > best_accuracy) {
    best_accuracy <- indicadores_val$Accuracy
    best_k <- k
  }
}

#----K-NN con CHEBYSHEV
preds_TEST = KernelKnn(X[spl_train, ], TEST_data = X[spl_test, ], y[spl_train], k = best_k , 
                       method = 'chebyshev', weights_function = NULL, regression = F,
                       Levels = unique(y))

predicted_classes <- colnames(preds_TEST)[max.col(preds_TEST, ties.method = "random")]
predicted_classes <- gsub("class_1", "A", predicted_classes)
predicted_classes <- gsub("class_2", "B", predicted_classes)
predicted_classes <- gsub("class_3", "C", predicted_classes)
predicted_classes <- factor(predicted_classes, levels = c("A", "B", "C"))

true_labels <- y[spl_test]
predicted_labels <- as.character(predicted_classes)
conf_matrix3 <- table(Actual = true_labels, Predicted = predicted_labels)
indicadores <- calculo_indicadores(conf_matrix)

results_df_chebyshev <- data.frame(k = k_values, accuracy = accuracy_values)
ggplot(results_df_chebyshev, aes(x = k, y = accuracy)) +
  geom_line(color = "blue", size = 1) +
  labs(
    x = "Valor de k",
    y = "Accuracy",
    title = "Accuracy de los distintos valores de k",
    subtitle = "Usando la distancia chebyshev"
  )

print("CON DISTANCIA CHEBYSHEV:")
print(paste("Best k:", best_k))
print("Matriz de Confusion:")
print(conf_matrix3)
print(paste("Accuracy por formula:", indicadores$Accuracy))
print(paste("Precision:", indicadores$Precision))
print(paste("Matthews Correlation Coefficient:", indicadores$`Matthews Correlation Coefficient`))
print(paste("F1 Score:", indicadores$`F1 Score`))

#--------DISTANCIA CANBERRA
# Inicializar variables para el mejor k y el mejor accuracy
best_k <- 0
k_values <- numeric()
best_accuracy <- -Inf
accuracy_values <- numeric()

# Bucle para probar diferentes valores de k con CANBERRA
for (k in 1:20) {  # Prueba desde k = 1 hasta k = 20
  preds_VAL = KernelKnn(X[spl_train, ], TEST_data = X[spl_test, ], y[spl_train], k = k, 
                        method = 'canberra', weights_function = NULL, regression = F,
                        Levels = unique(y))
  
  predicted_classes_val <- colnames(preds_VAL)[max.col(preds_VAL, ties.method = "random")]
  predicted_classes_val <- gsub("class_1", "A", predicted_classes_val)
  predicted_classes_val <- gsub("class_2", "B", predicted_classes_val)
  predicted_classes_val <- gsub("class_3", "C", predicted_classes_val)
  predicted_classes_val <- factor(predicted_classes_val, levels = c("A", "B", "C"))
  
  true_labels_val <- y[spl_test]
  predicted_labels_val <- as.character(predicted_classes_val)
  conf_matrix_val <- table(Actual = true_labels_val, Predicted = predicted_labels_val)
  indicadores_val <- calculo_indicadores(conf_matrix_val)
  
  accuracy_values <- c(accuracy_values, indicadores_val$Accuracy)
  k_values <- c(k_values, k)
  
  # Comparar con el mejor accuracy
  if (indicadores_val$Accuracy > best_accuracy) {
    best_accuracy <- indicadores_val$Accuracy
    best_k <- k
  }
}

#----K-NN con CANBERRA
preds_TEST = KernelKnn(X[spl_train, ], TEST_data = X[spl_test, ], y[spl_train], k = best_k , 
                       method = 'canberra', weights_function = NULL, regression = F,
                       Levels = unique(y))

predicted_classes <- colnames(preds_TEST)[max.col(preds_TEST, ties.method = "random")]
predicted_classes <- gsub("class_1", "A", predicted_classes)
predicted_classes <- gsub("class_2", "B", predicted_classes)
predicted_classes <- gsub("class_3", "C", predicted_classes)
predicted_classes <- factor(predicted_classes, levels = c("A", "B", "C"))

true_labels <- y[spl_test]
predicted_labels <- as.character(predicted_classes)
conf_matrix4 <- table(Actual = true_labels, Predicted = predicted_labels)
indicadores <- calculo_indicadores(conf_matrix)

results_df_canberra <- data.frame(k = k_values, accuracy = accuracy_values)
ggplot(results_df_canberra, aes(x = k, y = accuracy)) +
  geom_line(color = "blue", size = 1) +
  labs(
    x = "Valor de k",
    y = "Accuracy",
    title = "Accuracy de los distintos valores de k",
    subtitle = "Usando la distancia canberra"
  )

print("CON DISTANCIA CANBERRA:")
print(paste("Best k:", best_k))
print("Matriz de Confusion:")
print(conf_matrix4)
print(paste("Accuracy por formula:", indicadores$Accuracy))
print(paste("Precision:", indicadores$Precision))
print(paste("Matthews Correlation Coefficient:", indicadores$`Matthews Correlation Coefficient`))
print(paste("F1 Score:", indicadores$`F1 Score`))

#----GRAFICOS PRIMERA PARTE
#Grafico de líneas para todas las distancias
combined_results <- rbind( # Combina los resultados en un solo dataframe
  cbind(results_df_euclidean, distance = "Euclidean"),
  cbind(results_df_manhattan, distance = "Manhattan"),
  cbind(results_df_chebyshev, distance = "Chebyshev"),
  cbind(results_df_canberra, distance = "Canberra"))
ggplot(combined_results, aes(x = k, y = accuracy, color = distance)) +
  geom_line(size = 1) +
  labs(
    x = "Valor de k",
    y = "Accuracy",
    title = "Accuracy vs. Valor de k para distintas distancias")

#Heatmaps con matriz de confusión
heatmap_euclidean <- ggplot(as.data.frame(as.table(conf_matrix1)), aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +  # Puedes ajustar los colores
  labs(title = "Euclidean",x = "Predicted",y = "Actual") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

heatmap_manhattan <- ggplot(as.data.frame(as.table(conf_matrix2)), aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +  # Puedes ajustar los colores
  labs(title = "Manhattan",x = "Predicted",y = "Actual") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

heatmap_chebyshev <- ggplot(as.data.frame(as.table(conf_matrix3)), aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +  # Puedes ajustar los colores
  labs(title = "Chebyshev",x = "Predicted",y = "Actual") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

heatmap_canberra <- ggplot(as.data.frame(as.table(conf_matrix4)), aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +  # Puedes ajustar los colores
  labs(title = "Canberra",x = "Predicted",y = "Actual") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plot_grid(
  heatmap_euclidean, heatmap_manhattan,
  heatmap_chebyshev, heatmap_canberra,
  ncol = 2, nrow = 2  # Indica el número de columnas y filas
)

#----ALGORITMO DE ELABORACIÓN PROPIA PARA K-NN
# Función para calcular probabilidades ponderadas por grupo
calculate_group_probabilitiesEucli <- function(X_train, y_train, X_test, k_neighbors) {
  probabilities <- list()
  
  for (i in 1:nrow(X_test)) {
    distances <- apply(X_train, 1, function(j) sqrt(sum((X_test[i, ] - j)^2)))
    neighbors <- order(distances)[1:k_neighbors]
    
    group_probabilities <- rep(0, length(unique(y_train)))
    total_distance <- 0
    
    for (idx in neighbors) {
      group <- y_train[idx]
      weight <- 1 / (distances[idx]^2 + 1e-6)
      group_probabilities[group] <- group_probabilities[group] + weight
      total_distance <- total_distance + weight
    }
    
    normalized_probabilities <- group_probabilities / total_distance
    probabilities[[i]] <- normalized_probabilities
  }
  
  return(probabilities)
}

# Inicializar variables para el mejor k y el mejor accuracy
best_k <- 0
k_values <- numeric()
best_accuracy <- -Inf
accuracy_values <- numeric()

# Bucle para probar diferentes valores de k con EUCLIDIAN
for (k in 1:20) {  # Prueba desde k = 1 hasta k = 20
  preds_VAL = calculate_group_probabilitiesEucli(X[spl_train, ], y[spl_train], X[spl_test, ], k)
  
  predicted_classes_val <- colnames(as.data.frame(preds_VAL))[max.col(as.data.frame(preds_VAL), ties.method = "random")]
  predicted_classes_val <- factor(predicted_classes_val, levels = unique(y))
  
  true_labels_val <- y[spl_test]
  conf_matrix_val <- table(Actual = true_labels_val, Predicted = predicted_classes_val)
  indicadores_val <- calculo_indicadores(conf_matrix_val)
  
  accuracy_values <- c(accuracy_values, indicadores_val$Accuracy)
  k_values <- c(k_values, k)
  
  # Comparar con el mejor accuracy
  if (indicadores_val$Accuracy > best_accuracy) {
    best_accuracy <- indicadores_val$Accuracy
    best_k <- k
  }
}

# K-NN con EUCLIDEAN y ponderación
preds_TEST = calculate_group_probabilitiesEucli(X[spl_train, ], y[spl_train], X[spl_test, ], best_k)

predicted_classes <- colnames(as.data.frame(preds_TEST))[max.col(as.data.frame(preds_TEST), ties.method = "random")]
predicted_classes <- factor(predicted_classes, levels = unique(y))

true_labels <- y[spl_test]
conf_matrix1 <- table(Actual = true_labels, Predicted = predicted_classes)
indicadores <- calculo_indicadores(conf_matrix1)

results_df_euclidean <- data.frame(k = k_values, accuracy = accuracy_values)
ggplot(results_df_euclidean, aes(x = k, y = accuracy)) +
  geom_line(color = "blue", size = 1) +
  labs(
    x = "Valor de k",
    y = "Accuracy",
    title = "Accuracy de los distintos valores de k",
    subtitle = "Usando la distancia euclidiana con ponderación"
  )

print("CON DISTANCIA EUCLIDIANA Y PONDERACIÓN:")
print(paste("Best k:", best_k))
print("Matriz de Confusion:")
print(conf_matrix1)
print(paste("Accuracy por formula:", indicadores$Accuracy))
print(paste("Precision:", indicadores$Precision))
print(paste("Matthews Correlation Coefficient:", indicadores$`Matthews Correlation Coefficient`))
print(paste("F1 Score:", indicadores$`F1 Score`))
