library(readr)
library(readxl)
setwd('C:\\Users\\Dell\\Desktop\\SCMA 24')
df_p = read.csv('IPL_ball_by_ball_updated till 2024 (1).csv')
df_s = read_excel('IPL SALARIES 2024.xlsx')

head(df_p)
head(df_s)

install.packages("dplyr")
install.packages("fitdistrplus")
library(dplyr)
library(fitdistrplus)

colnames(df_p)
colnames(df_s)

# Calculate total runs per player per match
runs_data <- df_p %>%
  group_by(Match.id,Striker) %>%
  summarise(total_runs = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup()

# Calculate total wickets per bowler per match
wickets_data <- df_p %>%
  filter(wicket_type %in% c("bowled", "caught", "lbw", "stumped", "caught and bowled", "hit wicket")) %>%
  group_by(Match.id, Bowler) %>%
  summarise(total_wickets = n()) %>%
  ungroup()

# Combine the data
combined_data <- list(runs = runs_data, wickets = wickets_data)
print(combined_data)

# Display the processed data
head(combined_data$runs)
head(combined_data$wickets)

# Find top 3 run-getters and wicket-takers per round
top_performers <- function(data, metric, top_n = 3) {
  data %>%
    group_by(Match.id) %>%
    top_n(n = top_n, wt = !!sym(metric)) %>%
    ungroup()
}

top_run_getters <- top_performers(runs_data, "total_runs")
top_wicket_takers <- top_performers(wickets_data, "total_wickets")

head(top_run_getters)
head(top_wicket_takers)

# Filter data for the last three IPL tournaments
last_three_seasons <- c(2022, 2023, 2024)
filtered_runs_data <- runs_data %>%
  filter('Season' %in% last_three_seasons)
filtered_wickets_data <- wickets_data %>%
  filter('Season' %in% last_three_seasons)

# Extract top 3 performers in the last three seasons
top_run_getters_last_three <- top_performers(filtered_runs_data, "total_runs")
top_wicket_takers_last_three <- top_performers(filtered_wickets_data, "total_wickets")

# Fit distributions
fit_distribution <- function(data, column) {
  fit <- fitdist(data[[column]], "norm")
  plot(fit)
  fit
}

# Ensure the data is numeric and has more than one value
if (is.numeric(top_run_getters_last_three$total_runs) && length(top_run_getters_last_three$total_runs) > 1) {
  run_distribution_fit <- fitdist(top_run_getters_last_three$total_runs, "norm")
  plot(run_distribution_fit)
  print(run_distribution_fit)
} else {
  cat("The data for top_run_getters_last_three$total_runs is not sufficient for fitting a distribution.\n")
}

if (is.numeric(top_wicket_takers_last_three$total_wickets) && length(top_wicket_takers_last_three$total_wickets) > 1) {
  wicket_distribution_fit <- fitdist(top_wicket_takers_last_three$total_wickets, "norm")
  plot(wicket_distribution_fit)
  print(wicket_distribution_fit)
} else {
  cat("The data for top_wicket_takers_last_three$total_wickets is not sufficient for fitting a distribution.\n")
}

# Compare R Ashwin's salary and performance
# Filter R Ashwin's performance data
ashwin_wickets <- wickets_data %>%
  filter(Bowler == "R Ashwin") %>%
  group_by('Season') %>%
  summarise(total_wickets = sum(total_wickets), average_wickets_per_match = mean(total_wickets))

# Extract R Ashwin's salary
ashwin_salary <- df_s %>%
  filter(Player == "R Ashwin")

# Display R Ashwin's performance and salary
ashwin_wickets_summary <- ashwin_wickets %>%
  summarise(total_wickets = sum(total_wickets), average_wickets_per_match = mean(total_wickets))

print(ashwin_salary)
print(ashwin_wickets_summary)

# Check the structure of ashwin_wickets and ashwin_salary
str(ashwin_wickets)
str(ashwin_salary)

# Print the first few rows to verify the data
head(ashwin_wickets)
head(ashwin_salary)

# Convert season column to character if necessary for merging
ashwin_wickets$season <- as.character(ashwin_wickets$Season)
ashwin_salary$Season <- as.character(ashwin_salary$Season)

# Ensure Salary column is numeric
ashwin_salary$Salary <- as.numeric(gsub(",", "", ashwin_salary$Salary))

# Merge Ashwin's performance and salary data
ashwin_performance_salary <- merge(ashwin_wickets, ashwin_salary, by.x = "Season", by.y = "Season")

# Compute the correlation between Ashwin's salary and his performance metrics
correlation_total_wickets <- cor(ashwin_performance_salary$total_wickets, ashwin_performance_salary$Salary, use = "complete.obs")
correlation_average_wickets <- cor(ashwin_performance_salary$average_wickets_per_match, ashwin_performance_salary$Salary, use = "complete.obs")

# Print the correlation results
cat("Correlation between Ashwin's total wickets and salary: ", correlation_total_wickets, "\n")
cat("Correlation between Ashwin's average wickets per match and salary: ", correlation_average_wickets, "\n")