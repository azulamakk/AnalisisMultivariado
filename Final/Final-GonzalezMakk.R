library(dplyr)
library(ggplot2)

student <- read.csv("~/Universidad/Analisis Multivariado/Final/student-mat.csv")
View(student)


# 1 --------------- Seleccion de variables
data <- student %>% select(address, studytime, internet, absences, Dalc, goout, G3)

# 2 --------------- Analisis descriptivo 

# 2.1 --------------- Datos Numericos

# 2.1.1 --------------- studytime


# 2.1.2 --------------- absenses 


# 2.1.3 --------------- Dalc


# 2.1.4 --------------- goout


# 2.1.5 --------------- G3



# 2.2 --------------- Datos Categoricos


# 2.2.1 --------------- address


# 2.2.2 --------------- internet