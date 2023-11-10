# Specify the URL of the CSV file
url = 'https://raw.githubusercontent.com/mikeismerio/M5E8/main/BankChurners.csv'

# Read the CSV file from the specified URL into a data frame named df
df <- read.csv(url)


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

library(tidyverse)
library(janitor)
library(qcc)
library(visdat)
library(inspectdf)
library(psych)


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

