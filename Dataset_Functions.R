#----------------------------Importing Libraries--------------------------------#
library(dplyr)
library(anytime)
library(lubridate)
library(tidyr)

#------------------------------Loading data-------------------------------------#
stocks_df <- read.csv("stock_data.csv")

#-------------------------------Preprocessing-----------------------------------#
#Convert 'Date' column to datetime format
stocks_df$Date <- as.POSIXct(stocks_df$Date, tz = "UTC")

#-------------------Only include data from the last 5 years---------------------#
# Extract the stock data from 2019 to 2023
stocks_df <- stocks_df[which(year(stocks_df$Date) >= 2019 & year(stocks_df$Date) <= 2023), ]

# Extract year from the Date column
stocks_df$Year <- year(stocks_df$Date)
#----------------Calculating annual returns per stock per year------------------#
# Group by 'Company' and 'Year' and get the first and last close prices
first_close <- stocks_df %>% group_by(Company, Year) %>% summarise(First_Close = first(Close))
last_close <- stocks_df %>% group_by(Company, Year) %>% summarise(Last_Close = last(Close))

# Calculate the returns per year
returns <- merge(last_close, first_close, by = c("Company", "Year"))
returns$Annual_Return <- (returns$Last_Close - returns$First_Close) / returns$First_Close
# Normalise returns
returns$Annual_Return <- scale(returns$Annual_Return)
#-------------------Keeping the necessary columns ------------------------------#
# Keep only 'Company', 'Year', and 'Annual_Return' columns
stocks_df <- returns[c("Company", "Year", "Annual_Return")]
#-----------------------------Unique Stocks-------------------------------------#
#Unique stocks in the Company column
unique_stocks <- unique(stocks_df$Company)
unique_stocks_num <- length(unique_stocks)
print(unique_stocks_num)

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

#Unique stocks in the Company column
unique_stocks <- unique(stocks_df$Company)
unique_stocks_num <- length(unique_stocks)
print(unique_stocks_num)
#-------------------------Select 5 random stocks-----------------------------#
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
stocks_df
#------------------------------Data Splitting---------------------------------#
# Splitting the data into training and testing sets based on the 'Year' column
train <- stocks_df %>% filter(Year %in% c(2019, 2020, 2021))
test <- stocks_df %>% filter(Year %in% c(2022, 2023))

# Printing the first few rows of the training and testing sets
print(train)
print(test)

#-------------------------EDA on Train Data------------------------------------#
ggplot(train, aes(x = Year, y = Annual_Return, fill = Company)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Annual Returns by Company",
       x = "Year",
       y = "Annual Return",
       fill = "Company") +
  theme_minimal()

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
train_avg_data <- calculate_average_annual_return(train)
train_avg_data
# Displaying the unique instances
cat("\nAverage Annual Return for each Stock:\n")
print(train_avg_data)

returns <- train_avg_data$Mean_Annual_Return
returns
str(returns)
#---------Calculating Excess Return---------#
# Pivot train_data to wide format
stock_wide <- train %>%
  spread(key = Company, value = Annual_Return)

# Subtract average annual return from each stock's annual return
for (Company in colnames(stock_wide)) {
  stock_wide[, Company] <- stock_wide[, Company] - train_avg_data[train_avg_data$Company == Company, "Mean_Annual_Return"]
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

constraint = function(weights) {
  boundary_constr = (sum(weights)-1)**2   #portfolio sums to 1
  
  for (i in 1:length(weights)) {
    boundary_constr = boundary_constr + 
      max(c(0,weights[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-weights[i]))**2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}

portfolio_fitness <- function(weights) {
  # Calculate Sharpe Ratio
  sharpe_ratio <- calculate_sharpe_ratio(returns, weights)
  
  # Calculate constraint value
  constr <- constraint(weights)
  
  # Use negative Sharpe Ratio to maximize since optimization algorithms typically minimize
  return (-sharpe_ratio)
}
#-------------------------------------------GA----------------------------------#
library(GA)
library("mcga")

#This function is used by the GA to compute or report the statistics of your interest after every generation.
#This function overrides the default functionality provided by gaMonitor().
monitor <- function(obj){
  # gaMonitor(obj)                      #call the default gaMonitor to print the usual messages during evolution
  iter <- obj@iter                      #get the current iteration/generation number 
  if (iter <= maxGenerations){          #some error checking
    fitness <- obj@fitness              #get the array of all the fitness values in the present population
    #<<- assigns a value to the global variable declared outside the scope of this function.    
    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)    
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }  
  else{                               #print error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}

runGA <- function(noRuns = 30, problem = "feature"){
  #Specify GA parameter values; using the default values below. 
  if (problem == "feature"){
    maxGenerations <<- 20    #<<- makes it a global variable. So it will be visible to other functions e.g. monitor()
    popSize = 200
    pcrossover = 0.8
    pmutation = 0.1
    type = "binary"
    crossover = 
      data <- getData()
    xx <- data[,-ncol(data)]
    yy <- data[,ncol(data)]
    fitness = featureFitness              #fitness function defined in feature-selection.R
  }
  else if (problem == "Portfolio"){
    maxGenerations <<- 500
    popSize = 50
    pcrossover = 0.8
    pmutation = 0.2
    type = "real-valued"
    crossover = gareal_waCrossover
    data = returns
    min = rep(0,5)
    max = rep(1,5)    
    fitness = portfolio_fitness   #fitness function defined in Dataset+Funcs.R
  }
  else {
    cat("invalid problem specified. Exiting ... \n")
    return()
  }
  #Set up what stats you wish to note.    
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) #stats of a single run
  resultsMatrix = matrix(1:maxGenerations, ncol = 1)  #stats of all the runs
  
  resultNames = character(length(statnames)*noRuns)
  resultNames[1] = "Generation"
  
  bestFitness <<- -Inf
  bestSolution <<- NULL
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    if (problem == "feature")
      GA <- ga(type=type, fitness = fitness, xx=xx, yy=yy, nBits = ncol(xx), 
               names = colnames(xx), seed=i, popSize = popSize, 
               pcrossover = pcrossover, pmutation = pmutation, 
               maxiter = maxGenerations, monitor= monitor)
    else if (problem == "tsp")
      GA <- ga(type = type, fitness = fitness, distMatrix = data, 
               min = min, max = max, popSize = popSize, maxiter = maxGenerations,
               pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i )
    else if (problem == "Portfolio")
      GA <- ga(type = type, fitness = fitness, 
               min = min, max = max, popSize = popSize, maxiter = maxGenerations,
               pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, crossover = crossover, seed = i )
    
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
  }
  colnames(resultsMatrix) = resultNames
  return (resultsMatrix)
}

getBestFitness<-function(){
  return(bestFitness)
}

getBestSolution<-function(){
  return(bestSolution)
}

#results_sp<- runGA(noRuns = 30, problem = "Portfolio")
#p1 <- parseData(results_sp, 2, 30)

#Retrieve the best fitness value and solution
#bestFitness_sp <- getBestFitness()
#bestSolution_sp <- getBestSolution()

#Print the values or use them as needed
#print(bestFitness_sp)
#print(bestSolution_sp)

#results_blx <- runGA(noRuns = 30, problem = "Portfolio")
#p2 <- parseData(results_blx, 2, 30)

#bestFitness_blx <- getBestFitness()
#bestSolution_blx <- getBestSolution()

#Print the values or use them as needed
#print(bestFitness_blx)
#print(bestSolution_blx)

#results_arith <- runGA(noRuns = 30, problem = "Portfolio")
#p3 <- parseData(results_arith, 2, 30)

#bestFitness_arith <- getBestFitness()
#bestSolution_arith <- getBestSolution()

#Print the values or use them as needed
#print(bestFitness_arith)
#print(bestSolution_arith)


