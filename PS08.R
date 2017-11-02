library(tidyverse)
library(caret)

# Package for easy timing in R
library(tictoc)


# Demo of timer function --------------------------------------------------
# Run the next 5 lines at once
tic()
Sys.sleep(3) #puts computer to sleep for three seconds
timer_info <- toc()
runtime <- timer_info$toc - timer_info$tic

runtime


# Get data ----------------------------------------------------------------
# Accelerometer Biometric Competition Kaggle competition data
# https://www.kaggle.com/c/accelerometer-biometric-competition/data
train <- read_csv("~/PS08/train.csv")

# YOOGE!
dim(train)

# Time knn here -----------------------------------------------------------
# Should not be more than 2 minutes!

train1 <- train[sample(nrow(train), 200000),]
train2 <- train[sample(nrow(train), 400000),]
train3 <- train[sample(nrow(train), 600000),]
train4 <- train[sample(nrow(train), 800000),]
train5 <- train[sample(nrow(train), 1000000),]
train6 <- train[sample(nrow(train), 1200000),]
train7 <- train[sample(nrow(train), 1600000),]
train8 <- train[sample(nrow(train), 1800000),]
train9 <- train[sample(nrow(train), 2000000),]
train10 <- train[sample(nrow(train), 4000000),]
train11 <- train[sample(nrow(train), 7000000),]
train12 <- train[sample(nrow(train), 10000000),]

n_values <- c(200000,400000,600000,800000,1000000,1200000,
              1600000,1800000,2000000, 4000000, 7000000, 10000000)
k_values <- c(2,4,6,8,10,12,14,16,18,20,22,24)
model_formula <- as.formula(Device ~ X + Y + Z)

n <- c()
k <- rep(k_values, length(n_values))

for (i in 1:length(n_values)){
  x <- rep(n_values[i], length(k_values))
  n <- c(n, x)
}

runtimes <- c()

# Time knn here -----------------------------------------------------------
for (m in 1:length(n_values)) {
  go <- get(paste0("train",m))
  for (i in 1:length(k_values)) {
    tic()
    model_knn <- caret::knn3(model_formula, data=go, k = k_values[i])
    timer_info <- toc()
    runtime <- timer_info$toc - timer_info$tic
    runtimes <- c(runtimes, runtime)
  }
}

runtime_dataframe <- data.frame(n, as.factor(k), runtimes)


#Create a subset of the training data, then run the model by changing
#the values of k. Track down the differences in run time

# Plot your results ---------------------------------------------------------
# Think of creative ways to improve this barebones plot. Note: you don't have to
# necessarily use geom_point
runtime_plot <- ggplot(runtime_dataframe, aes(x=n, y=runtimes, col= k)) +
  geom_point()

runtime_plot
ggsave(filename="Vickie_Ip.png", width=16, height = 9)

# Runtime complexity ------------------------------------------------------
# Can you write out the rough Big-O runtime algorithmic complexity as a function
# of:
# -n: number of points in training set
# -k: number of neighbors to consider
# -d: number of predictors used? In this case d is fixed at 3

#To determine the trend of the runtimes, I decided to average out the runtimes for each n

avg_run <- c()
for (i in 1:length(n_values)) {
  avg_run[i] <- mean(runtimes[n == n_values[i]]) 
  
}

avg_df <- data.frame(n_values,as.factor(k), avg_run)
avg_runtime_plot <- ggplot(avg_df, aes(x=n_values, y=avg_run)) +
  geom_point( col="red") 
avg_runtime_plot
ggsave(filename="Vickie_Ip_avg.png", width=16, height = 9)

#Based on the avg_runtime_plot, it seems like runtime is roughly Big0(n^2)