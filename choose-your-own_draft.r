if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(quantmod)) install.packages("quantmod", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("quantmod", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(data.table)
library(quantmod)
library(glmnet)

set.seed(42, sample.kind="Rounding")

# Load stock data using quantmod package
start <- as.Date("2021-03-08")
end <- as.Date("2023-03-09")
getSymbols("GME", from = start, to = end)

# Drop GME.Adjusted
print(sprintf("Are GME.Adjusted and GME.Close identical?: %s",
              all(GME$GME.Close == GME$GME.Adjusted)))
GME$GME.Adjusted <- NULL

# Make sure there are no missing values in data
print(sprintf("Any missing values in data?: %s",any(is.na(GME))))
print("--------------------------------------------------------")

# Visualize the initial data witch a candle chart
candleChart(GME, up.col = "blue", dn.col = "red", theme = "white")

# Split data into training and test sets
training <- GME[1: 450,]
test <- GME[451: nrow(GME),]

# Visualize training data 
candleChart(as.xts(training), up.col = "blue", dn.col = "red", theme = "white")

# Create baseline random walk model for training
observed <- as.numeric(GME$GME.Close[2:450])
baseline_pred_train <- as.numeric(GME$GME.Close[1:449])
baseline_rmse_train <- sqrt(mean((baseline_pred_train - observed)^2))
print(sprintf("Baseline training RMSE: %f",baseline_rmse_train))

# Calculate and visualize differenced training data 
training <- diff(as.matrix(training))
candleChart(as.xts(training), up.col = "blue", dn.col = "red", theme = "white")

# Standardize training data
train_sds <- apply(training, 2, sd)
train_means <- colMeans(training)
for(i in 1:5){training[,i] <- (training[,i] - train_means[i]) / train_sds[i]}

# Convert training data to data.table object
training <- as.data.table(training)

# Visualize distribution of values in training data
print(training[, 1:5] |> pivot_longer(everything()) |> ggplot(aes(value)) +
  geom_histogram(bins = 35) + facet_wrap(vars(name)))

# Add lagged columns to training data
add_lagged <- function(dt, n){
  # Define new column names
  lag_names <- map_chr(1:n, ~ sprintf("lag%d", .))
  lag_names <- expand.grid(lag_names, names(dt))
  lag_names <- apply(lag_names, 1, function(v){paste(v[2], v[1], sep = "_")})
  # Create lagged columns
  dt[, (lag_names) := shift(.SD, 1:n, type = "lag"), .SDcols = names(dt)]
}

add_lagged(training, 14)

# Add target column to training data
training[, target := shift(GME.Close, 1, type = "lead")]

# Drop rows with missing values from training data
training <- drop_na(training)

# Create separate training set with clipped outliers
training_clip <- training[apply(abs(training) <= 3, 1, all),]

# Visualize distribution of values in clipped training data
print(training_clip[, 1:5] |> pivot_longer(everything()) |> ggplot(aes(value)) +
        geom_histogram(bins = 35) + facet_wrap(vars(name)))


# Train models on both intact and clipped training sets

knn_intact <- train(select(training, -target), training[, target],
                trControl = trainControl("cv"),
                tuneGrid = data.frame(list(k = 1:75)),
                method = "knn")


enet_intact <- train(select(training, -target), training[, target],
               trControl = trainControl("cv"),
               method = "glmnet")
best_alpha_intact <- enet_intact$bestTune[,"alpha"]
lambda_cv_intact <- cv.glmnet(as.matrix(select(training, -target)), training[, target], 
                    alpha = best_alpha_intact)
best_lambda_intact <- lambda_cv_intact$lambda.min


knn_clipped <- train(select(training_clip, -target), training_clip[, target],
                trControl = trainControl("cv"),
                tuneGrid = data.frame(list(k = 1:75)),
                method = "knn")


enet_clipped <- train(select(training_clip, -target), training_clip[, target],
                trControl = trainControl("cv"),
                method = "glmnet")
best_alpha_clipped <- enet_clipped$bestTune[,"alpha"]
lambda_cv_clipped <- cv.glmnet(as.matrix(select(training_clip, -target)), training_clip[, target], 
                       alpha = best_alpha_clipped)
best_lambda_clipped <- lambda_cv_clipped$lambda.min

get_train_pred <- function(model, lambda = NULL){
  if(is.null(lambda)){
    pred <- predict(model$finalModel, as.matrix(select(training, -target)))
    
  }
  else{
    pred <- predict(model$finalModel, as.matrix(select(training, -target)),
                    s = lambda)[,1]
  }
  pred <- (pred * train_sds[1]) + train_means[1]
  prior_vals <- as.numeric(GME$GME.Close[16:449])
  pred <- prior_vals + pred
  observed <- GME$GME.Close[17:450]
  
  results <- data.frame(observed = as.numeric(observed), pred = pred, 
                        baseline = prior_vals,
                        ind = index(observed))
  results
}

print("--------------------")

knn_train_intact <- get_train_pred(knn_intact)
rmse <- sqrt(mean((knn_train_intact$pred - knn_train_intact$observed)^2))
print(sprintf("Intact KNN best tune: k = %s", knn_intact$bestTune))
print(sprintf("Intact KNN training RMSE: %f", rmse))

print(knn_train_intact[400:434,] |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point())

print("--------------------")

enet_train_intact <- get_train_pred(enet_intact, lambda = best_lambda_intact)
rmse <- sqrt(mean((enet_train_intact$pred - enet_train_intact$observed)^2))

print(sprintf("Intact elastic net best tune: alpha = %s, lambda = %s", 
              enet_intact$bestTune[1], enet_intact$bestTune[2]))
print(sprintf("Intact elastic net training RMSE: %f", rmse))

print(enet_train_intact[400:434,] |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point())

print("--------------------")

knn_train_clipped <- get_train_pred(knn_clipped)
rmse <- sqrt(mean((knn_train_clipped$pred - knn_train_clipped$observed)^2))
print(sprintf("Clipped KNN best tune: k = %s", knn_clipped$bestTune))
print(sprintf("Clipped KNN training RMSE: %f", rmse))

print(knn_train_clipped[400:434,] |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point())

print("--------------------")

enet_train_clipped <- get_train_pred(enet_clipped, lambda = best_lambda_clipped)
rmse <- sqrt(mean((enet_train_clipped$pred - enet_train_clipped$observed)^2))

print(sprintf("Clipped elastic net best tune: alpha = %s, lambda = %s", 
              enet_clipped$bestTune[1], enet_clipped$bestTune[2]))
print(sprintf("Clipped elastic net training RMSE: %f", rmse))

print(enet_train_clipped[400:434,] |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point())

print("--------------------------------------------------------")

# Create baseline model for testing
observed <- as.numeric(GME$GME.Close[452:505])
baseline_pred_test <- as.numeric(GME$GME.Close[451:504])
baseline_rmse_test <- sqrt(mean((baseline_pred_test - observed)^2))
print(sprintf("Baseline test RMSE: %f",baseline_rmse_test))


test <- diff(as.matrix(test))
# Scale test set based on mean and sd of training set
for(i in 1:5){test[,i] <- (test[,i] - train_means[i]) / train_sds[i]}

test <- as.data.table(test)
add_lagged(test, 14)
test <- drop_na(test)

get_test_pred <- function(model, lambda = NULL){
  if(is.null(lambda)){
    pred <- predict(model$finalModel, as.matrix(test))
    
  }
  else{
    pred <- predict(model$finalModel, as.matrix(test),
                    s = lambda)[,1]
  }
  # Remove last prediction due to lack of corresponding observation
  pred <- pred[1: length(pred) - 1]
  pred <- (pred * train_sds[1]) + train_means[1]
  prior_vals <- as.numeric(GME$GME.Close[466:504])
  pred <- prior_vals + pred
  observed <- GME$GME.Close[467:505]
  
  results <- data.frame(observed = as.numeric(observed), pred = pred, 
                        baseline = prior_vals,
                        ind = index(observed))
  results
}

print("--------------------")

knn_test_intact <- get_test_pred(knn_intact)
rmse <- sqrt(mean((knn_test_intact$pred - knn_test_intact$observed)^2))
print(sprintf("Intact KNN test RMSE: %f", rmse))

print(knn_test_intact |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point())


enet_test_intact <- get_test_pred(enet_intact, lambda = best_lambda_intact)
rmse <- sqrt(mean((enet_test_intact$pred - enet_test_intact$observed)^2))
print(sprintf("Intact elastic net test RMSE: %f", rmse))

print(enet_test_intact |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point())


knn_test_clipped <- get_test_pred(knn_clipped)
rmse <- sqrt(mean((knn_test_clipped$pred - knn_test_clipped$observed)^2))
print(sprintf("Clipped KNN test RMSE: %f", rmse))

print(knn_test_clipped |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point())


enet_test_clipped <- get_test_pred(enet_clipped, lambda = best_lambda_clipped)
rmse <- sqrt(mean((enet_test_clipped$pred - enet_test_clipped$observed)^2))
print(sprintf("Clipped elastic net test RMSE: %f", rmse))

print(enet_test_clipped |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point())

