# Tarjetas de crédito: un análisis estadístico
# Prototype Day
# Modulo 5: Programación y estadítica con R
# 
# Octubre - Noviembre 2023
# 
# Equipo 8
# Integrantes:
#   
# Genesis Viridiana Varela Salinas
# Lizeth Marquez Quiroz
# Maricruz Conti del Castillo
# Miguel Augusto Ismerio Legarreta
# Victor Manuel Vera Arzate


# LIBRARIES
# Install the 'tidyverse' package, which is a collection of packages for data manipulation and visualization
install.packages("tidyverse")
# Install the 'janitor' package for cleaning and tidying data
install.packages("janitor")
# Install the 'qcc' package for statistical quality control
install.packages("qcc")
# Install the 'ggQC' package for visualizing quality control data using ggplot2
install.packages("ggQC")
# Install the 'visdat' package for visualizing missing or unexpected data patterns
install.packages("visdat")
# Install the 'inspectdf' package for inspecting and diagnosing data frames
install.packages("inspectdf")
# Install the 'psych' package for various procedures in psychological research
install.packages("psych")
# Instala el paquete 'car' para acceder a funciones como 'qqPlot' para diagnósticos gráficos en análisis estadísticos.
install.packages("car")


library(tidyverse)
library(janitor)
library(qcc)
library(visdat)
library(inspectdf)
library(psych)
library(car)




# Specify the URL of the CSV file
url = 'https://raw.githubusercontent.com/mikeismerio/M5E8/main/BankChurners.csv'

# Read the CSV file from the specified URL into a data frame named df
df <- read.csv(url)


# ggpareto function for creating a Pareto chart using ggplot2
# Arguments:
#   - df2: Data frame containing the variable to be visualized
#   - variable_name: Name of the variable to be visualized

ggpareto <- function(df2, variable_name) {
  # Extract the variable name as a character string for use in the chart title
  title <- deparse(substitute(variable_name))
  # Create a data frame with non-missing values of the specified variable
  df2 <- data.frame(modality = na.omit(df2[[variable_name]]))
  # Summarize the frequency of each modality and arrange in descending order
  df2 <- df2 %>% group_by(modality) %>% summarise(frequency = n()) %>%
    arrange(desc(frequency))
  # Order the modality levels and convert them to ordered factors
  df2$modality <- ordered(df2$modality, levels = unlist(df2$modality, use.names = FALSE))
  # Add columns for integer representation of modality, cumulative frequency, and percentage
  df2 <- df2 %>% mutate(modality_int = as.integer(modality),
                        cumfreq = cumsum(frequency), perc = (frequency / nrow(df2)) * 100)
  # Get the number of rows and the total frequency
  nr <- nrow(df2)
  N <- sum(df2$frequency)
  # Create data frame for ticks on the right side of the chart
  df2_ticks <- data.frame(xtick0 = rep(nr + 0.55, 11), xtick1 = rep(nr + 0.59, 11),
                          ytick = seq(0, N, N/10))
  # Define labels for the right side of the chart
  y2 <- c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%")
  # Create the Pareto chart using ggplot2
  g <- ggplot(df2, aes(x = modality, y = frequency)) +
    geom_bar(stat = "identity", aes(fill = modality_int)) +
    geom_line(aes(x = modality_int, y = cumfreq, color = modality_int)) +
    geom_point(aes(x = modality_int, y = cumfreq, color = modality_int), pch = 19) +
    scale_y_continuous(breaks = seq(0, N, N/10), limits = c(-0.02 * N, N * 1.02)) +
    scale_x_discrete(breaks = df2$modality) +
    guides(fill = FALSE, color = FALSE) +
    annotate("rect", xmin = nr + 0.55, xmax = nr + 1,
             ymin = -0.02 * N, ymax = N * 1.02, fill = "white") +
    annotate("text", x = nr + 0.8, y = seq(0, N, N/10), label = y2, size = 3.5) +
    geom_segment(x = nr + 0.55, xend = nr + 0.55, y = -0.02 * N, yend = N * 1.02, color = "grey50") +
    geom_segment(data = df2_ticks, aes(x = xtick0, y = ytick, xend = xtick1, yend = ytick)) +
    labs(title = paste0(variable_name), y = "Frequency") +
    theme_bw()
  # Return a list containing the generated plot and a subset of the data frame for reference
  return(list(graph = g, df2 = df2[, c(3, 1, 2, 4, 5)]))
}


# Remove unnecessary columns
df2 <- subset(df, select = -c(CLIENTNUM,
                              Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1,
                              Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2))

# Clean and standardize column names using the janitor library
df2 <- clean_names(df2)
# Create a data frame containing the cleaned column names
column_names <- data.frame(Column_Names = colnames(df2))
# Print the data frame displaying the cleaned column names
print(column_names)


vis_dat(df2) # visualiza un DataFrame mostrándo las clases de las columnas

# Los datos de tipo character, cadenas, son las variables cualitativas (o categóricas) del dataframe que se enfocan en varias caracteristicas demográficas de los clientes.
# La mayoria de las variables de tipo integer y numeric hacen referencia a los datos financieros (numéricos) de los clientes.

inspect_types(df2) %>% show_plot() # Explora los tipos de columnas

inspect_na(df2) %>% show_plot() # Resume la prevalencia de valores faltantes en cada columna.

# Del gráfico anterior confirmamos que no tenemos valores nulos o faltantes en el dataframe.

inspect_cat(df2) %>% show_plot() # Devuelve una tabla resumiendo las características categóricas.

De este gráfico podemos obtener información relevante sobre las variables categóricas del dataframe.

# attrtion_flag: En el dataset se tienen más registros de clientes activos que de clientes que han cancelado su tarjeta de crédito (abandonado el banco).
# card_category: La mayoría de los clientes poseen la tarjeta "Blue".
# education_level: Hay siete categorías de nivel de estudios. Al rededor de un 50 % de los clientes son personas graduadas de la licenciatura (graduate) y del bachillerato (high school).
# gender: Más de la mitad de nuestros clientes son mujeres.
# income_category: Una gran parte de nuestros clientes gana menos de $40K o esta en el intervalo de $40 - $60 K.
# marital_status: La mayoría de nuestros clientes son casados o solteros.

inspect_num(df2) %>% show_plot() # explorar los datos numéricos

# Esta exploración rápida de los datos numéricos nos permite observar la distribución de los datos de manera general.
# 
# avg_open_to_buy: La distribución del crédito promedio disponible para gastar muestra una tendencia similar al promedio de la razón de uso de tarjeta.
# avg_utilization_ratio: Hay una cantidad considerable de clientes que en promedio usan su tarjeta muy poco.
# contacts_count_12_mon: Los clientes contactan al banco de 0 a 6 veces, y estos contactos podrían estar relacionados con la decisión de abandonar el banco.
# credit_limit: Tenemos muchos clientes con límites de crédito muy bajos, y otra cantidad considerable (que no puede ser considerada atípica) de clientes con crédito alto. El promedio de límite de crédito no describe bien a la población.
# customer_age: La edad de nuestros clientes tiende a una distribución gaussiana (ie, normal)
# dependent_count: Notamos que la mayor parte de nuestros clientes tienen de 2 a 3 dependientes financieros. Siendo 5 dependientes el valor menos frecuente (atípico).
# months_inactive_12_mon: Por lo general las personas que cancelan su tarjeta de crédito tienen de 1 a 3 meses de inactividad.
# months_on_book: Notamos que la gran mayoría de nuestros clientes tiene más de 3 años con nosotros, pero a su vez, este valor está relacionado con la mayoría de casos de cancelación de tarjeta
# total_amt_chng_q4_q1 y total_ct_chng_q4_q1: Ambas poseen distribuciones muy similares. Son los cambios observados de un trimestre al otro.
# total_relationship_count: Nuestros clientes tienen de 1 a 6 servicios contratados, siendo 3 la cantidad promedio.
# total_revolving_bal: un saldo renovable es la parte del gasto de la tarjeta de crédito que no se paga al final de un ciclo de facturación. El monto puede variar, subiendo o bajando según el monto prestado y el monto reembolsado. A excepción de la cola inferior y superior, notamos una distribución gaussiana platicúrtica.
# total_trans_amt: el monto total de transacciones tiene su pico máximo cerca de los $5000.
# total_trans_ct:la cantidad total de transacciones descrbe una distribución similar a la bimodal.




# atrition_flag

# Realizamos el resumen de estadísticas descriptivas de la variable attrition_flag
summary(df2$attrition_flag)
# Crear una tabla de distribución para la columna "attrition_flag"
table(df2$attrition_flag)
# Crear un gráfico de Pareto para la variable "attrition_flag"
pareto.chart(table(df2$attrition_flag), ylab = "Frecuencia", ylab2 = "Porcentaje", col = "skyblue", main = "Gráfico de Pareto para attrition_flag")
# Notamos una relación de Pareto en el estado de los clientes. Un poco mas del 80% de los clientes son activos y el otro casi 20% son de clientes que abandonaron.




# customer_age

# Realizamos el resumen de estadísticas descriptivas de la variable customer_age
summary(df2$customer_age)
cat("\n")
# Encontrar las edades únicas
unique(df2$customer_age) %>%
  sort()
cat("\n")
# Histograma de la edad
ggplot(df2, aes(x = customer_age)) +
  geom_histogram(fill = "lightblue", color = "black", binwidth  = 5) +
  labs(title = "customer_age", x = "Edad", y = "Frecuencia")

# La edad media de nuestros clientes es 46 años.
# La edad mínima registrada es de 26 años y la máxima de 73 años.




# gender

# Realizamos el resumen de estadísticas descriptivas de la variable gender
summary(df2$gender)
# Crear una tabla de distribución para la columna "gender"
table(df2$gender)
cat("\n")
# Crear un gráfico de Pareto para la variable "gender"
variable_name <- "gender"
ggpareto(df2, variable_name)




# Tranformar variables categóricas a numéricas


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



# Correlación
df3 <- df2[, c("total_trans_amt", "total_trans_ct")]

# Configura el tamaño del gráfico
# Las dimensiones se establecen en pulgadas
options(repr.plot.width = 12, repr.plot.height = 12)

pairs.panels(df3,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes


# Pruebas de Hipótesis

# ¿El nivel de ingresos de los clientes determina qué tanto gastan?
#   Hipótesis: Los clientes con nivel de ingresos de menos  40Ka 60K utilizan su tarjeta de crédito más veces que los demás clientes.
# Hipótesis: El género de los clientes no es un factor determinante de su nivel de ingresos.
# ¿Cuál es el nivel de ingresos de los clientes con tarjetas Platinum o Gold?
#   Hipotesis: Los clientes con tarjetas Blue no tienen un nivel de ingresos mayor a $80K
# Hipotesis: Los clientes con ingresos menores a $40K no tienen mas de 2 relaciones con el banco.
# Hipótesis: Sólo los clientes con ingresos superiores a $120K tienen el límite de crédito máximo.



# Filtra los datos para crear un subconjunto del grupo de clientes cancelados
grupo_cancelado <- subset(df2, attrition_flag == 0)

# Filtra los datos para crear un subconjunto del grupo de clientes no cancelados
grupo_no_cancelado <- subset(df2, attrition_flag == 1)






# Función para realizar la la prueba t de Welch

# Visualización de datos:
# Boxplot para comparar la distribución de las edades entre los dos grupos. Q-Q plots para evaluar la normalidad de las distribuciones en cada grupo.

# Prueba de hipótesis:
# Se realiza la prueba t de Welch para comparar las medias de las edades entre los grupos cancelados y no cancelados.


realizar_analisis_t_test <- function(variable) {
  # Realiza la prueba t de Welch
  t_test_resultado <- t.test(grupo_cancelado[[variable]], grupo_no_cancelado[[variable]])
  
  # Visualiza la distribución de la variable en cada grupo
  par(mfrow = c(1, 3), oma = c(1, 1, 1, 1), mar = c(4, 4, 2, 1))
  
  # Boxplot de la variable por grupo
  boxplot(df2[[variable]] ~ df2$attrition_flag, col = c("blue", "red"), main = paste("Boxplot de", variable, "por Grupo"))
  
  # Q-Q plot de la variable por grupo
  qqPlot(grupo_cancelado[[variable]], main = paste("Q-Q Plot - Grupo", variable, "Cancelado"), col = "blue")
  qqPlot(grupo_no_cancelado[[variable]], main = paste("Q-Q Plot - Grupo", variable, "No Cancelado"), col = "red")
  
  # Imprime los resultados de la prueba t
  cat("Resultados de la prueba t de Welch para", variable, ":\n")
  cat("Estadística de prueba (t):", t_test_resultado$statistic, "\n")
  cat("Grados de libertad:", t_test_resultado$parameter, "\n")
  cat("Valor p:", t_test_resultado$p.value, "\n")
  
  # Evalúa el valor p
  alpha = 0.05
  if (t_test_resultado$p.value < alpha) {
    cat("Conclusión: Se rechaza la hipótesis nula.\n")
  } else {
    cat("Conclusión: No hay suficiente evidencia para rechazar la hipótesis nula.\n")
  }
  
}



# customer_age

# Configura el tamaño del gráfico
# Las dimensiones se establecen en pulgadas
options(repr.plot.width = 20, repr.plot.height = 8)

# Llama a la función con el nombre de la variable específica
realizar_analisis_t_test("customer_age")

# Distribución normal?
# En resumen, la prueba de hipótesis no encuentra evidencia significativa para afirmar que hay una diferencia en las edades entre los clientes que han cancelado y los que no han cancelado.




# credit_limit

# Configura el tamaño del gráfico
# Las dimensiones se establecen en pulgadas
options(repr.plot.width = 20, repr.plot.height = 8)

realizar_analisis_t_test("credit_limit")

# Distribución exponencial?
# En resumen, la prueba de hipótesis sugiere que hay una diferencia significativa en los límites de crédito entre los clientes que han cancelado y los que no han cancelado, con un límite de crédito promedio menor para el grupo que ha cancelado.




# total_trans_amt

# Configura el tamaño del gráfico
# Las dimensiones se establecen en pulgadas
options(repr.plot.width = 20, repr.plot.height = 8)

realizar_analisis_t_test("total_trans_amt")




# avg_utilization_ratio

# Configura el tamaño del gráfico
# Las dimensiones se establecen en pulgadas
options(repr.plot.width = 20, repr.plot.height = 8)

realizar_analisis_t_test("avg_utilization_ratio")

# Distribución beta?
# Esto indica que hay evidencia suficiente para afirmar que existe una diferencia significativa en avg_utilization_ratio entre los grupos cancelados y no cancelados. En otras palabras, la diferencia en las tasas de utilización promedio observadas entre los dos grupos no es probable que sea debida al azar y probablemente refleje una diferencia real en la población subyacente.
# En términos prácticos, esto sugiere que avg_utilization_ratio puede ser un factor importante que distingue a los clientes que cancelan de aquellos que no cancelan.