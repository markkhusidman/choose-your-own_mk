if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(quantmod)) install.packages("quantmod", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(data.table)
library(quantmod)

start <- as.Date("2021-03-08")
end <- as.Date("2023-03-09")

# Get data
getSymbols("GME", from = start, to = end)

# Drop GME.Adjusted
print(any(GME$GME.Close != GME$GME.Adjusted))
GME$GME.Adjusted <- NULL

# Visualize data
candleChart(GME, up.col = "blue", dn.col = "red", theme = "white")

# Make sure there are no missing values in data
print(any(is.na(GME)))

# Split data into training and holdout sets
train <- GME[1: 450,]
test <- GME[451: nrow(GME),]

# Calculate differenced data and convert data to data.table object
train <- as.data.table(diff(as.matrix(train)))

# Add lagged columns to data
add_lagged <- function(df, n){
  lag_names <- map_chr(1:3, ~ sprintf("lag%d", .))
  lag_names <- expand.grid(names(train), lag_names)
  lag_names <- apply(lag_names, 1, function(v){paste(v[1], v[2], sep = "_")})
  lag_names
}

