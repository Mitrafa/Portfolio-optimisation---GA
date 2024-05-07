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

# Display the result
print(stocks_df)
#--------------------------Stocks that present in all 5 years ----------------------------#
#Unique stocks in the Company column
unique_stocks <- unique(stocks_df$Company)
unique_stocks_num <- length(unique_stocks)
print(unique_stocks_num)
#-----------------------------------------
library(dplyr)
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
#-----------
#Unique stocks in the Company column
unique_stocks <- unique(stocks_df$Company)
unique_stocks_num <- length(unique_stocks)
print(unique_stocks_num)

#---------Select 5 stocks---------#
stocks_df <- stocks_df %>%
  filter(Company %in% c("DG", "IMO", "JPM", "LNG", "TT"))

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

#------------Plots
library(ggplot2)

# Plot 'Close' for all 5 stocks
ggplot(train, aes(x = Date, y = Close, color = Company)) +
  geom_line() +
  labs(title = "Closing Prices of the stocks in training Data",
       x = "Date",
       y = "Close Prices",
       color = "Company") +
  theme_minimal()


