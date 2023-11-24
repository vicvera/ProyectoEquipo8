
install.packages(c("tidyverse", "janitor", "car", "caTools", "lightgbm", "caret", "rpart"))


#Cargar librerias
library(tidyverse)
library(janitor)
library(car)
library(caTools)
library(lightgbm)
library(caret)
library(rpart)


url = 'https://raw.githubusercontent.com/mikeismerio/M5E8/main/BankChurners.csv'
df <- read.csv(url)

# Eliminamos columnas inecesarias
df2 <- subset(df, select = -c(CLIENTNUM,
                              Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1,
                              Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2))

# Limpia y estandariza los nombres de las columnas (librería janitor)
df2 <- clean_names(df2)

# Crear un data frame con los nombres de las columnas
column_names <- data.frame(Column_Names = colnames(df2))

# Mostrar el data frame
print(column_names)




df2$attrition_flag <- case_when(
  df2$attrition_flag == "Attrited Customer" ~ 0,
  df2$attrition_flag == "Existing Customer" ~ 1,
  TRUE ~ as.integer(df2$attrition_flag)
)

df2$gender <- case_when(
  df2$gender == "F" ~ 0,
  df2$gender == "M" ~ 1,
  TRUE ~ as.integer(df2$gender)
)

df2$education_level <- case_when(
  df2$education_level == "Unknown" ~ 0,
  df2$education_level == "Uneducated" ~ 1,
  df2$education_level == "High School" ~ 2,
  df2$education_level == "College" ~ 3,
  df2$education_level == "Graduate" ~ 4,
  df2$education_level == "Post-Graduate" ~ 5,
  df2$education_level == "Doctorate" ~ 6,
  TRUE ~ as.integer(df2$education_level)
)

df2$marital_status <- case_when(
  df2$marital_status == "Unknown" ~ 0,
  df2$marital_status == "Single" ~ 1,
  df2$marital_status == "Married" ~ 2,
  df2$marital_status == "Divorced" ~ 3,
  TRUE ~ as.integer(df2$marital_status)
)

df2$income_category <- case_when(
  df2$income_category == "Unknown" ~ 0,
  df2$income_category == "Less than $40K" ~ 1,
  df2$income_category == "$40K - $60K" ~ 2,
  df2$income_category == "$60K - $80K" ~ 3,
  df2$income_category == "$80K - $120K" ~ 4,
  df2$income_category == "$120K +" ~ 5,
  TRUE ~ as.integer(df2$income_category)
)

df2$card_category <- case_when(
  df2$card_category == "Blue" ~ 0,
  df2$card_category == "Silver" ~ 1,
  df2$card_category == "Gold" ~ 2,
  df2$card_category == "Platinum" ~ 3,
  TRUE ~ as.integer(df2$card_category)
)


head(df2)







# Crear la matriz de características (x) y el vector de etiquetas (y)
x <- df2[, !(names(df2) %in% c('Attrition_Flag'))]

y <- df2$attrition_flag

# Dividir el conjunto de datos en conjuntos de entrenamiento y prueba
set.seed(42)  # Para reproducibilidad
split <- sample.split(y, SplitRatio = 0.7)
x_train <- subset(x, split == TRUE)
x_test <- subset(x, split == FALSE)
y_train <- y[split == TRUE]
y_test <- y[split == FALSE]

# Calcular el conteo de valores en y_train y y_test
train_value <- table(y_train)
test_value <- table(y_test)

# Imprimir información sobre los datos de entrenamiento
cat("Datos para entrenamiento en y_train: ", length(y_train), "\n")
cat("Porcentaje de Clientes Existentes: ", sprintf("%.2f%%", (train_value[2]/length(y_train)) * 100), "\n")
cat("Porcentaje de Clientes Attrited: ", sprintf("%.2f%%", (train_value[1]/length(y_train)) * 100), "\n\n")

# Imprimir información sobre los datos de prueba
cat("Datos para prueba en y_test: ", length(y_test), "\n")
cat("Porcentaje de Clientes Existentes: ", sprintf("%.2f%%", (test_value[2]/length(y_test)) * 100), "\n")
cat("Porcentaje de Clientes Attrited: ", sprintf("%.2f%%", (test_value[1]/length(y_test)) * 100), "\n")





# Decision Tree Regression Model
# Create the decision tree model
tree_model <- rpart(attrition_flag ~ ., data = df2, method = "anova")

# Predict on the training set
y_train_pred <- predict(tree_model, newdata = x_train)

# Predict on the test set
y_test_pred <- predict(tree_model, newdata = x_test)

# Evaluate the performance on the training set
train_rmse <- sqrt(mean((y_train_pred - y_train)^2))
cat("Training RMSE (Decision Tree):", train_rmse, "\n")

# Evaluate the performance on the test set
test_rmse <- sqrt(mean((y_test_pred - y_test)^2))
cat("Test RMSE (Decision Tree):", test_rmse, "\n")


# Predict probabilities on the entire dataset
df2$tree_probabilities <- predict(tree_model, newdata = df2, type = "vector")

# Display the first few rows of the updated dataframe
head(df2)


# Crear un nuevo dataframe con las columnas de interés
df_probabilities <- df2[c("attrition_flag", "tree_probabilities")]





# Convertir las predicciones a factores binarios
y_test_pred <- as.factor(ifelse(y_test_pred > 0.5, 1, 0))

# Confusion Matrix
conf_matrix <- confusionMatrix(data = y_test_pred, reference = as.factor(y_test))
print(conf_matrix)
