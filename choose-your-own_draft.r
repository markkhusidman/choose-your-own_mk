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
training <- GME[1: 450,]
test <- GME[451: nrow(GME),]

# Calculate differenced data 
training <- diff(as.matrix(training))
candleChart(as.xts(training), up.col = "blue", dn.col = "red", theme = "white")

# Create baseline model
observed <- as.numeric(GME$GME.Close[2:450])
baseline_pred_train <- as.numeric(GME$GME.Close[1:449])
baseline_rmse_train <- sqrt(mean((baseline_pred_train - observed)^2))
print(baseline_rmse_train)

# Standardize data
train_sds <- apply(training, 2, sd)
train_means <- colMeans(training)
for(i in 1:5){training[,i] <- (training[,i] - train_means[i]) / train_sds[i]}
candleChart(as.xts(training), up.col = "blue", dn.col = "red", theme = "white", yrange = c(-6, 6))

# Convert data to data.table object
training <- as.data.table(training)

# Visualize distributions of columns
print(training[, 1:5] |> pivot_longer(everything()) |> ggplot(aes(value)) +
  geom_histogram(bins = 35) + facet_wrap(vars(name)))

# Add lagged columns to data
add_lagged <- function(dt, n){
  lag_names <- map_chr(1:n, ~ sprintf("lag%d", .))
  lag_names <- expand.grid(lag_names, names(dt))
  lag_names <- apply(lag_names, 1, function(v){paste(v[2], v[1], sep = "_")})
  dt[, (lag_names) := shift(.SD, 1:n, type = "lag"), .SDcols = names(dt)]
}

add_lagged(training, 14)

# Add target column
training[, target := shift(GME.Close, 1, type = "lead")]

# Drop missing values
training <- drop_na(training)

# Drop outliers
training_clip <- training[apply(training < 5, 1, all),]

# Train models

model <- train(select(training, -target), training[, target],
                trControl = trainControl("cv"),
                tuneGrid = data.frame(list(k = 22)),
                method = "knn")


model2 <- train(select(training, -target), training[, target],
               trControl = trainControl("cv"),
               method = "glmnet")
best_alpha <- model2$bestTune[,"alpha"]
lambda_cv <- cv.glmnet(as.matrix(select(training, -target)), training[, target], 
                       alpha = best_alpha)
best_lambda <- lambda_cv$lambda.min

model3 <- train(select(training_clip, -target), training_clip[, target],
                trControl = trainControl("cv"),
                tuneGrid = data.frame(list(k = 22)),
                method = "knn")

model4 <- train(select(training_clip, -target), training_clip[, target],
                trControl = trainControl("cv"),
                method = "glmnet")
best_alpha <- model4$bestTune[,"alpha"]
lambda_cv <- cv.glmnet(as.matrix(select(training_clip, -target)), training_clip[, target], 
                       alpha = best_alpha)
best_lambda <- lambda_cv$lambda.min



# Evaluate model
models <- list(model, model2, model3, model4)
for(mdl in models){
  pred <- ifelse("lambda" %in% names(mdl$bestTune),
                 predict(mdl$finalModel, as.matrix(select(training, -target)),
                                 s = best_lambda)[,1],
                 predict(mdl$finalModel, as.matrix(select(training, -target))))
  
  # pred <- predict(mdl$finalModel, select(training, -target))
  pred <- (pred * train_sds[1]) + train_means[1]
  pred <- as.numeric(GME$GME.Close[16:449]) + pred
  observed <- as.numeric(GME$GME.Close[17:450])
  print(sqrt(mean((pred - observed)^2)))
  
  results <- data.frame(observed = observed, pred = pred, 
                        baseline = as.numeric(GME$GME.Close[16:449]),
                        ind = index(GME$GME.Close[17:450]))
  
  print(results[35:65,] |> pivot_longer(cols = c("observed", "pred"))
        |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point())
  
  print(results[400:434,] |> pivot_longer(cols = c("observed", "pred"))
        |> ggplot(aes(ind, value, color = name)) + geom_line() + geom_point())
}



# Create testing baseline model
observed <- as.numeric(GME$GME.Close[452:505])
baseline_pred_test <- as.numeric(GME$GME.Close[451:504])
baseline_rmse_test <- sqrt(mean((baseline_pred_test - observed)^2))
print(baseline_rmse_test)


test <- diff(as.matrix(test))
# Scale test set based on mean and sd of training set
for(i in 1:5){test[,i] <- (test[,i] - train_means[i]) / train_sds[i]}

test <- as.data.table(test)
add_lagged(test, 14)
test <- drop_na(test)

pred <- predict(model$finalModel, as.matrix(test))
# Remove last prediction due to lack of corresponding observation
pred <- pred[1: length(pred) - 1]
pred <- (pred * train_sds[1]) + train_means[1]
pred <- as.numeric(GME$GME.Close[466:504]) + pred
observed <- as.numeric(GME$GME.Close[467:505])
print(sqrt(mean((pred - observed)^2)))

pred <- predict(model2$finalModel, as.matrix(test),
                s = best_lambda)[,1]
# Remove last prediction due to lack of corresponding observation
pred <- pred[1: length(pred) - 1]
pred <- (pred * train_sds[1]) + train_means[1]
pred <- as.numeric(GME$GME.Close[466:504]) + pred
observed <- as.numeric(GME$GME.Close[467:505])
print(sqrt(mean((pred - observed)^2)))

pred <- predict(model3$finalModel, as.matrix(test))
# Remove last prediction due to lack of corresponding observation
pred <- pred[1: length(pred) - 1]
pred <- (pred * train_sds[1]) + train_means[1]
pred <- as.numeric(GME$GME.Close[466:504]) + pred
observed <- as.numeric(GME$GME.Close[467:505])
print(sqrt(mean((pred - observed)^2)))

pred <- predict(model4$finalModel, as.matrix(test),
                s = best_lambda)[,1]
# Remove last prediction due to lack of corresponding observation
pred <- pred[1: length(pred) - 1]
pred <- (pred * train_sds[1]) + train_means[1]
pred <- as.numeric(GME$GME.Close[466:504]) + pred
observed <- as.numeric(GME$GME.Close[467:505])
print(sqrt(mean((pred - observed)^2)))