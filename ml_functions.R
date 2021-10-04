# Randomly samples the data into a training and test set.
# Parameters: my_data:dataframe or tibble
#             size: size of training data, 
#             train: whether the data is training or testing data
# 
CreateTrainTestSplit <- function(my_data, size = 0.8, train = TRUE, seed = 123) {
  ## 75% of the sample size
  smp_size <- floor(size * nrow(my_data))
  
  ## set the seed to make your partition reproducible
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(my_data)), size = smp_size)
  
  train_data <- my_data[train_ind, ]
  test_data <- my_data[-train_ind, ]
  if (train == TRUE) {
    return (train_data)
  } else {
    return (test_data)
  }
}