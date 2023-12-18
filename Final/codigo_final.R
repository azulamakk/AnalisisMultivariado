library(dplyr)
library(caret)
library(class)
library(e1071)
library(proxy)
library(Metrics)

credit <- read.csv("~/Universidad/Analisis Multivariado/Final/credit_risk_dataset.csv")

selected_credit <- credit %>%
  select(loan_amnt, loan_percent_income, person_emp_length, loan_int_rate, loan_grade)
print(head(selected_credit))

# Convertir a numérico y eliminar NA
selected_credit[, 1:4] <- sapply(selected_credit[, 1:4], as.numeric)
selected_credit <- na.omit(selected_credit)
selected_credit$loan_grade <- as.factor(selected_credit$loan_grade)

# Filtrar por categorías
selected_cat <- c("A", "B", "C")
sample <- selected_credit[selected_credit$loan_grade %in% selected_cat, ]

# Muestreo aleatorio
set.seed(42)
sample_size <- 300
sample <- sample_n(sample, sample_size)
print(head(sample))

# Assuming 'sample' is your data frame
cor_matrix <- cor(sample[, 1:4])  # Calculate correlation matrix for numeric columns

# Create a heatmap
heatmap(cor_matrix,
        col = colorRampPalette(c("blue", "white", "red"))(20),
        main = "Correlation Heatmap",
        xlab = "Variables",
        ylab = "Variables",
        margins = c(5, 5))

# Add correlation values to the heatmap
text(expand.grid(1:ncol(cor_matrix), 1:ncol(cor_matrix)),
     labels = round(cor_matrix, 2),
     cex = 0.8, col = "black")

# You may need to install the RColorBrewer package if not installed
# install.packages("RColorBrewer")
library(RColorBrewer)

# Customize the color palette (optional)
heatmap(cor_matrix,
        col = colorRampPalette(brewer.pal(9, "RdBu"))(20),
        main = "Correlation Heatmap",
        xlab = "Variables",
        ylab = "Variables",
        margins = c(5, 5))

plot3d <- function(X, loan_grade, size = 6, cube = TRUE) {
  library(plotly)
  library(RColorBrewer)
  
  n <- nrow(X)
  p <- ncol(X)
  
  # Identify numeric columns
  numeric_cols <- sapply(X, is.numeric)
  
  # Scale only numeric columns
  data <- data.frame(X)
  data[, numeric_cols] <- scale(data[, numeric_cols], scale = FALSE)
  names(data) <- c('x1', 'x2', 'x3')
  data$loan_grade <- loan_grade  # Add loan_grade column
  
  fig <- plot_ly(data,
                 x = ~data[, 1], y = ~data[, 2], z = ~data[, 3],
                 color = ~loan_grade,  # Use color attribute directly
                 colors = brewer.pal(p, 'Set1'),
                 text = ~loan_grade,
                 marker = list(size = size))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(X)[1],
                                                  range = c(min(data$x1), max(data$x1))),
                                     yaxis = list(title = colnames(X)[2],
                                                  range = c(min(data$x2), max(data$x2))),
                                     zaxis = list(title = colnames(X)[3],
                                                  range = c(min(data$x3), max(data$x3))),
                                     aspectmode = ifelse(cube == TRUE, 'cube', 'auto')))
  fig
}

datosRelevantes <- sample %>% select(loan_amnt, person_emp_length, loan_int_rate)
X <- datosRelevantes
loan_grade <- sample$loan_grade

plot3d(X, loan_grade)
