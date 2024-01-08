### Plotting the Regression Models

# Loading the required libraries
library(tidyverse)
library(ggplot2)

## Plotting the Regression Models for the Bi-Variate Analysis
ggplot(fh_pop_gdp, aes(x = Log_Population, y = Total_Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Total Score vs. Population",
       x = "Population",
       y = "Total Score") +
  theme(plot.title = element_text(hjust = 0.5))

# Load necessary library
library(ggplot2)

# Create a new data frame for predictions
new_data <- data.frame(Log_Population = seq(min(fh_pop_gdp$Log_Population), 
                                            max(fh_pop_gdp$Log_Population), 
                                            length.out = 100))

# Predict probabilities
new_data$Status_prob <- predict(fit_ord, newdata = new_data, type = "probs")

# Reshape for ggplot2
new_data_long <- tidyr::pivot_longer(new_data, 
                                     cols = starts_with("Status_prob"),
                                     names_to = "Status",
                                     values_to = "Probability")

# Adjust the Status levels to match original data
new_data_long$Status <- factor(new_data_long$Status, levels = c('NF', 'PF', 'F'))
