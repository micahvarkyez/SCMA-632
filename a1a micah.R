setwd('C:\\Users\\HP\\Documents\\ns')
getwd()
install_and_load <- function(package)
library(readr)
library(dplyr)
data <- read_csv("NSSO68 new.csv")
WestBengal_data <- filter(data, state == "West Bengal")
missing_values <- sapply(WestBengal_data, function(x) sum(is.na(x)))
print(missing_values)
WestBengal_data <- WestBengal_data %>%
library(ggplot2)
WestBengal_data <- WestBengal_data %>%
mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
missing_values_after <- sapply(WestBengal_data, function(x) sum(is.na(x)))
print(missing_values_after)
numeric_columns <- WestBengal_data %>% select_if(is.numeric)
outliers <- list()

for(col in colnames(numeric_columns)) {
  Q1 <- quantile(numeric_columns[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(numeric_columns[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers[[col]] <- numeric_columns %>%
    filter((.data[[col]] < lower_bound) | (.data[[col]] > upper_bound)) %>%
    select(col)
}
sapply(outliers, nrow)
for(col in colnames(numeric_columns)) {
  Q1 <- quantile(numeric_columns[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(numeric_columns[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  median_val <- median(numeric_columns[[col]], na.rm = TRUE)
  
  odisha_data[[col]] <- ifelse(WestBengal_data[[col]] < lower_bound | WestBengal_data[[col]] > upper_bound, 
                               median_val, 
                               WestBengal_data[[col]])
}  
WestBengal_data <- WestBengal_data %>%
  mutate(
    district = recode(district, 
                      '1' = 'District1', 
                      '2' = 'District2', 
                      '3' = 'District3'  
    ),
    sector = recode(sector, 
                    '1' = 'Rural', 
                    '2' = 'Urban')
  )
summary_data <- WestBengal_data %>%
  group_by(district, sector) %>%
  summarize(
    avg_consumption = mean(consumption, na.rm = TRUE),
    total_consumption = sum(consumption, na.rm = TRUE),
    .groups = 'drop'
  )
top_three <- summary_data %>%
  arrange(desc(avg_consumption)) %>%
  head(3)

bottom_three <- summary_data %>%
  arrange(avg_consumption) %>%
  head(3)
print("Top Three Districts of Consumption:")
print(top_three)

print("Bottom Three Districts of Consumption:")
print(bottom_three)
rural_consumption <- WestBengal_data %>%
  filter(sector == "Rural") %>%
  pull(total_consumption)

urban_consumption <- WestBengal_data %>%
  filter(sector == "Urban") %>%
  pull(total_consumption)
install.packages(BSDA)
library(BSDA)
sigma_rural <- 2.56
sigma_urban <- 2.34 
if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, ", Therefore we reject the null hypothesis.\n")
  cat("There is a significant difference between mean consumptions of urban and rural.\n")
} else {
  cat("P value is >=", 0.05, ", Therefore we fail to reject the null hypothesis.\n")
  cat("There is no significant difference between mean consumptions of urban and rural.\n")
}
print(z_test_result)