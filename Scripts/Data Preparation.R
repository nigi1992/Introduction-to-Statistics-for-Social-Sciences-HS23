### Data Preparation
#### Importing the data

# Importing Freedom House Aggregate Scores (xlsx)
install.packages("readxl")
library(readxl)
#file_path <- "Input Data/data.xlsx"
# Reading Excel file
fh_data <- read_excel("/Users/nicolaswaser/New-project-GitHub-first/R/Capstone Project HS23/Input Data/Aggregate_Category_and_Subcategory_Scores_FIW_2003-2022.xlsx")
head(fh_data)
View(fh_data)

# Importing Population Data (xls)
Pop_data <- read_excel("/Users/nicolaswaser/New-project-GitHub-first/R/Capstone Project HS23/Input Data/API_SP.POP.TOTL_DS2_en_excel_v2_6299418.xls")
head(Pop_data)
View(Pop_data)

# Importing GDP per Capita Data (xls)
GDPperCapita_data <- read_excel("/Users/nicolaswaser/New-project-GitHub-first/R/Capstone Project HS23/Input Data/API_NY.GDP.PCAP.PP.CD_DS2_en_excel_v2_6299221.xls")
head(GDPperCapita_data)
View(GDPperCapita_data)

# Preparing the data for merge
install.packages(dplyr)
library(dplyr)

# selecting relevant columns, filtering out territories + year earlier than 2022 and renaming columns
fh <- fh_data %>% select(`Country/Territory`, `C/T?`, `Edition`, `Status`, `PR`, `CL`, `Total`) %>%
  filter(`C/T?` == "c" & `Edition` == "2022") %>%
  rename(Country = `Country/Territory`, Year = `Edition`, Status = `Status`, Political_Rights = `PR`, Civil_Liberties = `CL`, Total_Score = `Total`)
class(fh$Total_Score)
class(fh$Status)

# same for population data, but also removing the first 3 rows
colnames(Pop_data)
Pop <- Pop_data %>% select(`Data Source`, `...67`) %>%
  rename(Country = `Data Source`, Population = `...67`) %>% 
  slice(-1:-3)
# turns out values are set as characters which is not ideal for later calculations
class(Pop$Population)
# converting to numeric
Pop$Population <- as.numeric(Pop$Population)
class(Pop$Population)

# same for GDP per capita data
GDPperCapita <- GDPperCapita_data %>% select(`Data Source`, `...67`) %>%
  rename(Country = `Data Source`, GDP_per_Capita = `...67`) %>% 
  slice(-1:-3)
# converting to numeric
GDPperCapita$GDP_per_Capita <- as.numeric(GDPperCapita$GDP_per_Capita)

# Merging the data
# merging Freedom House and Population data
fh_pop <- fh %>% left_join(Pop, by = "Country")
# merging Freedom House, Population and GDP per Capita data
fh_pop_gdp <- fh_pop %>% left_join(GDPperCapita, by = "Country")

# Filtering NA values
fh_pop_gdp <- filter(fh_pop_gdp, !is.na(GDP_per_Capita) & !is.na(Population))
View(fh_pop_gdp)

# Saving as csv.file
file_path <- "/Users/nicolaswaser/New-project-GitHub-first/R/Capstone Project HS23/Input Data/fh_pop_gdp.csv"
write.csv(fh_pop_gdp, file_path, row.names = FALSE)
