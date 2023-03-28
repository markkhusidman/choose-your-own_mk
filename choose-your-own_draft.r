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

# Calculate differenced data 
train <- diff(as.matrix(train))
candleChart(as.xts(train), up.col = "blue", dn.col = "red", theme = "white")

# Standardize data
train <- scale(train)
candleChart(as.xts(train), up.col = "blue", dn.col = "red", theme = "white", yrange = c(-6, 6))

# Convert data to data.table object
train <- as.data.table(train)

# Visualize distributions of columns
print(train[, 1:5] |> pivot_longer(everything()) |> ggplot(aes(value)) +
  geom_histogram(bins = 35) + facet_wrap(vars(name)))

# Add lagged columns to data
add_lagged <- function(dt, n){
  lag_names <- map_chr(1:n, ~ sprintf("lag%d", .))
  lag_names <- expand.grid(lag_names, names(train))
  lag_names <- apply(lag_names, 1, function(v){paste(v[2], v[1], sep = "_")})
  dt[, (lag_names) := shift(.SD, 1:n, type = "lag"), .SDcols = names(dt)]
}

add_lagged(train, 3)
