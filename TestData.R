library(dplyr)
library(anytime)
library(lubridate)
library(tidyr)

#Loading data
stocks_df <- read.csv("stock_data.csv")
#------------------------------------Preprocessing---------------------------------------#
#Convert 'Date' column to datetime format
stocks_df$Date <- as.POSIXct(stocks_df$Date, tz = "UTC")
print(stocks_df)
#---------Only include data from the last 5 years---------#
# Extract the stock data from 2019 to 2023
stocks_df <- stocks_df[which(year(stocks_df$Date) >= 2019 & year(stocks_df$Date) <= 2023), ]

# Extract year from the Date column
stocks_df$Year <- year(stocks_df$Date)
#---------Calculating annual returns per stock per year---------#
# Group by 'Company' and 'Year' and get the first and last close prices
first_close <- stocks_df %>% group_by(Company, Year) %>% summarise(First_Close = first(Close))
last_close <- stocks_df %>% group_by(Company, Year) %>% summarise(Last_Close = last(Close))

# Calculate the returns per year
returns <- merge(last_close, first_close, by = c("Company", "Year"))
returns$Annual_Return <- (returns$Last_Close - returns$First_Close) / returns$First_Close
# Normalize returns
returns$Annual_Return <- scale(returns$Annual_Return)

# Keep only 'Company', 'Year', and 'Annual_Return' columns
stocks_df <- returns[c("Company", "Year", "Annual_Return")]

# Display the result
print(stocks_df)

# Count the number of unique years each company appears in
company_years_count <- stocks_df %>%
  group_by(Company) %>%
  summarize(Num_Years = n_distinct(Year))

# Filter the dataframe to include only companies present in all the years
companies_present_all_years <- company_years_count %>%
  filter(Num_Years == n_distinct(stocks_df$Year)) %>%
  pull(Company)

# Create a dataframe containing only the companies present in all the years
stocks_df <- stocks_df %>%
  filter(Company %in% companies_present_all_years)

# Display the result
print(stocks_df)

#Unique stocks in the Company column
unique_stocks <- unique(stocks_df$Company)
unique_stocks_num <- length(unique_stocks)
print(unique_stocks_num)

#---------Select 5 random stocks---------#
select_random_stocks <- function(data, num_stocks_to_select, seed=NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Select num_stocks_to_select random stocks
  selected_stocks <- sample(data$Company, num_stocks_to_select)
  
  # Filter the DataFrame for the selected stocks
  selected_data <- data[data$Company %in% selected_stocks, ]
  
  return(selected_data)
}

stocks_df <- select_random_stocks(stocks_df, num_stocks_to_select=5, seed=42)
print(stocks_df)

unique_companies <- unique(stocks_df$Company)
print(unique_companies)

#---------Data Splitting---------#
# Splitting the data into training and testing sets based on the 'Year' column
train <- stocks_df %>% filter(Year %in% c(2019, 2020, 2021))
test <- stocks_df %>% filter(Year %in% c(2022, 2023))

# Printing the first few rows of the training and testing sets
print(train)
print(test)

#---------Avg Return for each stock---------#
calculate_average_annual_return <- function(data) {
  # Calculate average annual return for each company
  Mean_Annual_Return <- data %>%
    group_by(Company) %>%
    summarise(Mean_Annual_Return = mean(Annual_Return)) %>%
    rename(Mean_Annual_Return = Mean_Annual_Return)
  
  return(Mean_Annual_Return)
}

# Calculate average annual return
test_avg_data <- calculate_average_annual_return(test)

# Displaying the unique instances
cat("\nAverage Annual Return for each Stock:\n")
print(test_avg_data)

returns <- test_avg_data$Mean_Annual_Return
returns
str(returns)
#---------Calculating Excess Return---------#
# Pivot train_data to wide format
stock_wide <- test %>%
  spread(key = Company, value = Annual_Return)

# Subtract average annual return from each stock's annual return
for (Company in colnames(stock_wide)) {
  stock_wide[, Company] <- stock_wide[, Company] - test_avg_data[test_avg_data$Company == Company, "Mean_Annual_Return"]
}

print(stock_wide)
#---------Covariance matrix---------#
stock_wide <- stock_wide %>%
  select(-Year)

cov_matrix <- cov(stock_wide)
print(cov_matrix)
str(cov_matrix)

#---------Setting up functions to be used in GA---------#
#Portfolio returns
calculate_portfolio_returns <- function(returns, weights) {
  return(sum(returns * weights))
}

#Sharpe_ratio
calculate_sharpe_ratio <- function(returns, weights) {
  portfolio_return <- calculate_portfolio_returns(returns, weights)
  portfolio_std_dev <- sqrt(t(weights) %*% cov_matrix %*% weights)
  sharpe_ratio <- portfolio_return / portfolio_std_dev
  return(sharpe_ratio)
}

best_weights_sp <- c(0.0001674443, 0.9990069, 0.0007464737, 0.002405807, 8.949637e-05)

sharpe_ratio_test <- calculate_sharpe_ratio(returns, best_weights_sp)
print(sharpe_ratio_test)
      
portfolio_return_test <- calculate_portfolio_returns(returns, best_weights_sp)
print(portfolio_return_test)

portfolio_std_dev <- sqrt(t(best_weights_sp) %*% cov_matrix %*% best_weights_sp)
print(portfolio_std_dev)

