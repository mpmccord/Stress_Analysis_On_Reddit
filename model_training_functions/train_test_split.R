# Randomly samples the data into a training and test set.
# @param x: a dataframe without the target variable
# @param y: the target variable
# @param size: the size of the variable
# @param seed: the seed to generate the random variable
# @return: a list of 4 values: training (x, y), test (x, y)
CreateTrainTestSplit <- function(x, y, size = 0.8, seed = 123) {
  inTrain <- createDataPartition(y, p = size, list = FALSE)[,1]
  
  x_train <- x[ inTrain, ]
  x_test  <- x[-inTrain, ]
  
  y_train <- y[ inTrain]
  y_test  <- y[-inTrain]
  my_list <- list(x_train, y_train, x_test, y_test)
  names(my_list) <- c("x_train", "y_train", "x_test", "y_test")
  return (my_list)

}