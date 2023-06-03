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

# Load GME data using quantmod package
start <- as.Date("2021-01-12")
end <- as.Date("2023-05-06")
getSymbols("GME", from = start, to = end)

# Drop GME.Adjusted
print(sprintf("Are GME.Adjusted and GME.Close identical?: %s",
              all(GME$GME.Close == GME$GME.Adjusted)))
GME$GME.Adjusted <- NULL

# Make sure there are no missing values in data
print(sprintf("Any missing values in data?: %s",any(is.na(GME))))
print("--------------------------------------------------------")

# Visualize the initial data witch a candle chart
candleChart(GME, up.col = "blue", dn.col = "red", theme = "white", 
            name = "Initial GME Data")

# Split data into training and test sets
training <- GME[1: 450,]
test <- GME[451: nrow(GME),]

# Visualize training data with a candle chart
candleChart(as.xts(training), up.col = "blue", dn.col = "red", theme = "white", 
            name = "Raw Training Data")

# Evaluate baseline random walk model on training set
observed <- as.numeric(GME$GME.Close[17:450])
baseline_pred_train <- as.numeric(GME$GME.Close[16:449])
baseline_rmse_train <- sqrt(mean((baseline_pred_train - observed)^2))
print(sprintf("Baseline training RMSE: %f",baseline_rmse_train))

# Calculate and visualize differenced training data
training <- diff(as.matrix(training))
candleChart(as.xts(training), up.col = "blue", dn.col = "red", theme = "white", 
            name = "Differenced Training Data")

# Standardize training data
train_sds <- apply(training, 2, sd)
train_means <- colMeans(training)
for(i in 1:5){training[,i] <- (training[,i] - train_means[i]) / train_sds[i]}

# Convert training data to data.table object
training <- as.data.table(training)

# Visualize distribution of values in training data with histograms
print(training |> pivot_longer(everything()) |> ggplot(aes(value)) +
  geom_histogram(bins = 35) + facet_wrap(vars(name)) + 
    ggtitle("Histograms of Training Data"))

# Visualize outliers in training data with box plots
print(training |> pivot_longer(everything()) |> ggplot(aes(value)) +
        geom_boxplot() + facet_wrap(vars(name)) + 
        ggtitle("Box Plots of Training Data"))

# Compare distribution of values in training data to normal distribution
print(training |> pivot_longer(everything()) |> 
        ggplot(aes(sample = value)) +
        geom_qq() + geom_abline() + facet_wrap(vars(name)) + 
        ggtitle("QQ Plots of Training Data"))

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

# Visualize distribution of values in clipped training data with histograms
print(training_clip[, 1:5] |> pivot_longer(everything()) |> ggplot(aes(value)) +
        geom_histogram(bins = 35) + facet_wrap(vars(name)) + 
        ggtitle("Histograms of Clipped Training Data"))


# Convert raw model outputs into Closing Price predictions during training
get_train_pred <- function(model, lambda = NULL){
  if(is.null(lambda)){
    # If labmda is null, assum k-NN model
    pred <- predict(model, as.matrix(select(training, -target)))
    
  }
  else{
    # If lambda is not null, assume elastic net model
    pred <- predict(model, as.matrix(select(training, -target)),
                    s = lambda)[,1]
  }
  # Unscale raw model outputs
  pred <- (pred * train_sds[1]) + train_means[1]
  # Add unscaled outputs to Closing Price occuring right before predicted value
  prior_vals <- as.numeric(GME$GME.Close[16:449])
  pred <- prior_vals + pred
  # Define observed values for comparison to predictions
  observed <- GME$GME.Close[17:450]
  
  # Baseline is equal to predictions using the baseline model
  results <- data.frame(pred = pred, observed = as.numeric(observed),  
                        baseline = prior_vals,
                        ind = index(observed))
  results
}


# Tune models on both intact and clipped training sets

print("--------------------")

# Tune intact k-NN model
knn_intact <- train(select(training, -target), training[, target],
                trControl = trainControl("cv"),
                tuneGrid = data.frame(list(k = 1:200)),
                method = "knn")
print(sprintf("Intact k-NN best tune: k = %s", knn_intact$bestTune))

# Confirm best value of k for intact k-NN
print(knn_intact$results |> ggplot(aes(k, RMSE)) + geom_line() + geom_point() + 
        ggtitle("Intact k-NN Tuning Results"))

# Evaluate best intact k-NN model on intact training set
knn_train_intact <- get_train_pred(knn_intact$finalModel)
rmse <- sqrt(mean((knn_train_intact$pred - knn_train_intact$observed)^2))
print(sprintf("Intact k-NN training RMSE: %f", rmse))

# Visualize training set predictions yielded by intact k-NN model
print(knn_train_intact[400:434,] |> 
        pivot_longer(cols = c("observed", "pred", "baseline")) |> 
        ggplot(aes(ind, value, color = name)) + geom_line() + geom_point() + 
        labs(title="Intact k-NN Training Evaluation", x="Date", 
             y="Closing Price ($)"))

print("--------------------")

# Tune clipped k-NN model
knn_clipped <- train(select(training_clip, -target), training_clip[, target],
                     trControl = trainControl("cv"),
                     tuneGrid = data.frame(list(k = 1:200)),
                     method = "knn")
print(sprintf("Clipped k-NN best tune: k = %s", knn_clipped$bestTune))

# Confirm best value of k for clipped k-NN
print(knn_clipped$results |> ggplot(aes(k, RMSE)) + geom_line() + geom_point() + 
        ggtitle("Clipped k-NN Tuning Results"))

# Evaluate best clipped k-NN model on intact training set
knn_train_clipped <- get_train_pred(knn_clipped$finalModel)
rmse <- sqrt(mean((knn_train_clipped$pred - knn_train_clipped$observed)^2))
print(sprintf("Clipped k-NN training RMSE: %f", rmse))

# Visualize training set predictions yielded by clipped k-NN model
print(knn_train_clipped[400:434,] |> 
        pivot_longer(cols = c("observed", "pred", "baseline")) |> 
        ggplot(aes(ind, value, color = name)) + geom_line() + geom_point() + 
        labs(title="Clipped k-NN Training Evaluation", x="Date", 
             y="Closing Price ($)"))

print("--------------------")

# Tune intact elastic net model
enet_intact <- train(select(training, -target), training[, target],
               trControl = trainControl("cv"),
               method = "glmnet")
best_alpha_intact <- enet_intact$bestTune[,"alpha"]

# Use in-depth search to find best lambda for intact elastic net
lambda_cv_intact <- cv.glmnet(as.matrix(select(training, -target)), training[, target], 
                    alpha = best_alpha_intact)
best_lambda_intact <- lambda_cv_intact$lambda.min

print(sprintf("Intact elastic net best tune: alpha = %s, lambda = %s", 
              enet_intact$bestTune[1], best_lambda_intact))

# Evaluate best intact elastic net model on intact training set
enet_train_intact <- get_train_pred(enet_intact$finalModel, 
                                    lambda = best_lambda_intact)
rmse <- sqrt(mean((enet_train_intact$pred - enet_train_intact$observed)^2))
print(sprintf("Intact elastic net training RMSE: %f", rmse))

# Visualize training set predictions yielded by intact elastic net model
print(enet_train_intact[400:434,] |> 
        pivot_longer(cols = c("observed", "pred", "baseline")) |> 
        ggplot(aes(ind, value, color = name)) + geom_line() + geom_point() + 
        labs(title="Intact EN Training Evaluation", x="Date", 
             y="Closing Price ($)"))

print("--------------------")

# Tune clipped elastic net model
enet_clipped <- train(select(training_clip, -target), training_clip[, target],
                trControl = trainControl("cv"),
                method = "glmnet")
best_alpha_clipped <- enet_clipped$bestTune[,"alpha"]

# Use in-depth search to find best lambda for clipped elastic net
lambda_cv_clipped <- cv.glmnet(as.matrix(select(training_clip, -target)), training_clip[, target], 
                       alpha = best_alpha_clipped)
best_lambda_clipped <- lambda_cv_clipped$lambda.min

print(sprintf("Clipped elastic net best tune: alpha = %s, lambda = %s", 
              enet_clipped$bestTune[1], best_lambda_clipped))

# Evaluate best clipped elastic net model on intact training set
enet_train_clipped <- get_train_pred(enet_clipped$finalModel, 
                                     lambda = best_lambda_clipped)
rmse <- sqrt(mean((enet_train_clipped$pred - enet_train_clipped$observed)^2))
print(sprintf("Clipped elastic net training RMSE: %f", rmse))

# Visualize training set predictions yielded by clipped elastic net model
print(enet_train_clipped[400:434,] |> 
        pivot_longer(cols = c("observed", "pred", "baseline")) |> 
        ggplot(aes(ind, value, color = name)) + geom_line() + geom_point() + 
        labs(title="Clipped EN Training Evaluation", x="Date", 
             y="Closing Price ($)"))

print("--------------------------------------------------------")

# Difference test data
test <- diff(as.matrix(test))
# Scale test data using means and SDs from training data
for(i in 1:5){test[,i] <- (test[,i] - train_means[i]) / train_sds[i]}

test <- as.data.table(test)
# Add lagged columns to test data
add_lagged(test, 14)
test <- drop_na(test)

# Evaluate baseline random walk model on test set
observed <- as.numeric(GME$GME.Close[467:583])
baseline_pred_test <- as.numeric(GME$GME.Close[466:582])
baseline_rmse_test <- sqrt(mean((baseline_pred_test - observed)^2))
print(sprintf("Baseline test RMSE: %f",baseline_rmse_test))

# Convert raw model outputs into Closing Price predictions during testing
get_test_pred <- function(model, lambda = NULL){
  if(is.null(lambda)){
    pred <- predict(model, as.matrix(test))
    
  }
  else{
    pred <- predict(model, as.matrix(test),
                    s = lambda)[,1]
  }
  # Remove last prediction due to lack of corresponding observation
  pred <- pred[1: length(pred) - 1]
  pred <- (pred * train_sds[1]) + train_means[1]
  prior_vals <- as.numeric(GME$GME.Close[466:582])
  pred <- prior_vals + pred
  observed <- GME$GME.Close[467:583]
  
  results <- data.frame(pred = pred, observed = as.numeric(observed),  
                        baseline = prior_vals,
                        ind = index(observed))
  results
}

print("--------------------")

# Evaluate intact k-NN model on testing data
knn_test_intact <- get_test_pred(knn_intact$finalModel)
rmse <- sqrt(mean((knn_test_intact$pred - knn_test_intact$observed)^2))
print(sprintf("Intact k-NN test RMSE: %f", rmse))

# Visualize intact k-NN model on testing data
print(knn_test_intact |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point() + 
        labs(title="Intact k-NN Testing Evaluation", x="Date", 
             y="Closing Price ($)"))


# Evaluate clipped k-NN model on testing data
knn_test_clipped <- get_test_pred(knn_clipped$finalModel)
rmse <- sqrt(mean((knn_test_clipped$pred - knn_test_clipped$observed)^2))
print(sprintf("Clipped k-NN test RMSE: %f", rmse))

# Visualize clipped k-NN model on testing data
print(knn_test_clipped |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point() + 
        labs(title="Clipped k-NN Testing Evaluation", x="Date", 
             y="Closing Price ($)"))


# Evaluate intact elastic net model on testing data
enet_test_intact <- get_test_pred(enet_intact$finalModel, 
                                  lambda = best_lambda_intact)
rmse <- sqrt(mean((enet_test_intact$pred - enet_test_intact$observed)^2))
print(sprintf("Intact elastic net test RMSE: %f", rmse))

# Visualize intact elastic net model on testing data
print(enet_test_intact |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point() + 
        labs(title="Intact EN Testing Evaluation", x="Date", 
             y="Closing Price ($)"))


# Evaluate clipped elastic net model on testing data
enet_test_clipped <- get_test_pred(enet_clipped$finalModel, 
                                   lambda = best_lambda_clipped)
rmse <- sqrt(mean((enet_test_clipped$pred - enet_test_clipped$observed)^2))
print(sprintf("Clipped elastic net test RMSE: %f", rmse))

# Visualize clipped elastic net model on testing data
print(enet_test_clipped |> pivot_longer(cols = c("observed", "pred", "baseline"))
      |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point() + 
        labs(title="Clipped EN Testing Evaluation", x="Date", 
             y="Closing Price ($)"))

