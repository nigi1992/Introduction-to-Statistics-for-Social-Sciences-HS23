### Plotting the Regression Models

# Loading the required libraries
library(tidyverse)
library(tidyr)
library(ggplot2)
library(here)

## Plotting the Regression Models for the Bi-Variate Analysis
ggplot(fh_pop_gdp, aes(x = Log_Population, y = Total_Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Total Score vs. Population",
       x = "Population",
       y = "Total Score") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plot1.png")) #saving as image


# trying the other more complicated ordered logistic regression model

# creating a new data frame for predictions
new_data <- data.frame(Log_Population = seq(min(fh_pop_gdp$Log_Population), 
                                            max(fh_pop_gdp$Log_Population), 
                                            length.out = 100))
# predicting probabilities
new_data$Status_prob <- predict(fit_ord, newdata = new_data, type = "probs")
predicted_probs <- predict(fit_ord, newdata = new_data, type = "probs")
# adding the predicted probabilities to new_data 
new_data[c("NF", "PF", "F")] <- predicted_probs
# reshaping the data to long format for ggplot2
new_data_long <- tidyr::pivot_longer(new_data, 
                                     cols = c("NF", "PF", "F"),
                                     names_to = "Status",
                                     values_to = "Probability")
# Plotting 
ggplot(new_data_long, aes(x = Log_Population, y = Probability, color = Status)) +
  geom_line() +
  labs(title = "Predicted Probabilities of Status vs Log Population",
       x = "Log Population",
       y = "Predicted Probability") +
  theme_minimal()
ggsave(file = here("Output", "Plot1b.png")) #saving as image



## Plotting the Regression Models for the Multivariate Analysis
# Create a new dataframe for plotting
plot_data1 <- data.frame(
  Log_Population = fh_pop_gdp$Log_Population,
  Total_Score = fh_pop_gdp$Total_Score,
  GDP_per_Capita = fh_pop_gdp$GDP_per_Capita
)

# Add fitted values from the model
plot_data1$fitted_values <- predict(fit2, newdata = plot_data1)

# Plot
ggplot(plot_data1, aes(x = Log_Population, y = Total_Score)) +
  geom_point() +
  geom_line(aes(y = fitted_values), color = "blue") +
  labs(title = "Total Score vs Log Population",
       x = "Log Population",
       y = "Total Score") +
  theme_minimal()
ggsave(file = here("Output", "Plot2a.png")) #saving as image

# Create a new data frame for plotting
plot_data2 <- data.frame(
  GDP_per_Capita = fh_pop_gdp$GDP_per_Capita,
  Total_Score = fh_pop_gdp$Total_Score,
  Log_Population = fh_pop_gdp$Log_Population
)

# Add fitted values from the model
plot_data2$fitted_values <- predict(fit2, newdata = plot_data2)

# Plot
ggplot(plot_data2, aes(x = GDP_per_Capita, y = Total_Score)) +
  geom_point() +
  geom_line(aes(y = fitted_values), color = "blue") +
  labs(title = "Total Score vs GDP per Capita",
       x = "GDP per Capita",
       y = "Total Score") +
  theme_minimal()
ggsave(file = here("Output", "Plot2b.png")) #saving as image

