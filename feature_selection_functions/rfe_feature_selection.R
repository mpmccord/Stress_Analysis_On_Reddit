library(caret)
library(randomForest)
library(faux)
library(DataExplorer)
library(tidyverse)
source("model_training_functions/train_test_split.R")

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
RunRFE <- function(x_train, y_train, rfeControl = CreateRFEControl(), max_size=13) {
  return (rfe(x = x_train, 
             y = y_train, 
             sizes = c(1:max_size), 
             rfeControl = control))
} 

