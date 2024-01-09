### Statistical Analysis
## Bi-Variate Model
# The bi-variate model is used to determine the relationship between the two variables: Population and Freedom Score
library(tidyverse)

# importing the data
library(readr)
file_path <- "/Users/nicolaswaser/New-project-GitHub-first/R/Capstone Project HS23/Input Data/fh_pop_gdp.csv"
fh_pop_gdp <- read_csv(file_path)

## Regression Model 1
fit1 <- lm(fh_pop_gdp$Total_Score ~ fh_pop_gdp$Population, data = fh_pop_gdp)
summary(fit1)

# very unsatisfying results, trying with the status variable
fit1b <- lm(fh_pop_gdp$Status ~ fh_pop_gdp$Population, data = fh_pop_gdp)
summary(fit1b)

# error message due to the fact that the Status variable is not numeric, but ordinal
# trying to convert it to factor variable
fh_pop_gdp$Status <- factor(fh_pop_gdp$Status, levels = c('NF', 'PF', 'F'))
# checking the levels to confirm they are in the correct order
levels(fh_pop_gdp$Status)

# running multinomial logistic regression
install.packages("nnet")
library(nnet)
fit1b <- multinom(Status ~ Population, data = fh_pop_gdp)
summary(fit1b)
# still unsatisfying

# trying to transform population variable to log scale, due to very large range of values
fh_pop_gdp$Log_Population <- log(fh_pop_gdp$Population)
summary(fh_pop_gdp$Log_Population)
# rerunning the regression with the transformed variable
fit1b_transformed <- multinom(Status ~ Log_Population, data = fh_pop_gdp)
summary(fit1b_transformed)
# better, but there is room for improvement

# Ordered Logistic Regression
install.packages("MASS")
library(MASS)
# converting 'Status' to an ordered factor
fh_pop_gdp$Status <- factor(fh_pop_gdp$Status, levels = c('NF', 'PF', 'F'), ordered = TRUE)
levels(fh_pop_gdp$Status)
# fitting the ordinal logistic regression model
fit_ord <- polr(Status ~ Log_Population, data = fh_pop_gdp, Hess = TRUE)
summary(fit_ord)

## Trying Total_Score as dependent variable again, but with Log_Population as independent
fit1c <- lm(fh_pop_gdp$Total_Score ~ fh_pop_gdp$Log_Population, data = fh_pop_gdp)
summary(fit1c)

### The last two approaches deliver far better results than those before



## Multivariate Model
# The multivariate model is used to determine the relationship between the three variables: Population, Freedom Score and GDP per Capita as the confounding variable

## Regression Model 2
fit2 <- lm(fh_pop_gdp$Total_Score ~ fh_pop_gdp$Log_Population + fh_pop_gdp$GDP_per_Capita, data = fh_pop_gdp)
summary(fit2)


## Regression Table
library(stargazer)
stargazer(fit1c, fit2, type = "text", title = "Regression Table", out = "/Users/nicolaswaser/New-project-GitHub-first/R/Capstone Project HS23/Output/Regression Table.txt") 
          

