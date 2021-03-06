library(caret)
library(randomForest)
library(faux)
library(DataExplorer)
library(tidyverse)
source("model_training_functions/train_test_split.R")
# Below is a series of functions for creating RFE feature selection.

# To run this code:
# 1. Run RecodeNumericalVariables() on the full dataset (factor the categorical variables)
# 2. CreateRFEControl() (optional, if you want to control the rfe control)
# 3. Split data into train and test (see train_test_split for an example)
# 4. RunRFE() to get the rfe results.

# You can visualize the result 


# Function for converting categorical/non-scaled variables and recoding them
# @param: df: a dataframe
# @param cat_vars: the categorical variables not to scale
# @return: a dataframe with the numeric features scaled
RecodeNumericalVariables <- function(df, cat_vars) {
  df <- df %>%
    # Save categorical features as factors
    mutate_at(cat_vars, 
              as.factor) %>%
    # Center and scale numeric features
    mutate_if(is.numeric, scale)
  return (df)
}

# Function for defining the control of an rfe random forest function
# @param my_method: the external resampling method (default: repeatedcv)
# @param num_repeats: number of repeats
# @param num_folds: the number of folds
# @return: rfe control object
CreateRFEControl <- function(my_method = "repeatedcv", num_repeats = 5, num_folds = 10) {
  control <- rfeControl(functions = rfFuncs, # random forest
                        method = my_method,
                        repeats = num_repeats, # number of repeats
                        number = num_folds) # the number of folds
  return (control)
}



# Runs RFE Control on the model; by default, creates a new rfe control object.
# @param x_train: training features data
# @param y_train: target features
# @param rfeControl: rfe control object (by default: will be created)
# @return: results of rfe
RunRFE <- function(x_train, y_train, control = NULL, max_size=13) {
  if (is.null(control)) {
    control = CreateRFEControl()
  }
  return (rfe(x = x_train, 
             y = y_train, 
             sizes = c(1:max_size), 
             rfeControl = control))
} 

