# Removes the label column and gets the correlated features from a dataframe or tibble.
# Parameters: df dataframe; label_col: a string: the name of the label column you need to exclude.
# Returns: correlation_matrix: matrix of correlations by column.
GetCorrelation <- function(df, label_col) {
  df_minus_label <- df %>%
    select(-label_col)
  correlation_matrix <- cor(df_minus_label)
  return (correlation_matrix)
}
# Uses GetCorrelatedFeatures to get the highly correlated features (in this case by a factor of 0.7)
# Parameters: df dataframe; 
#             label_col: a string: the name of the label column you need to exclude; 
#             cutoff, the maximum correlation allowed.
# Returns:    correlation_matrix: matrix of correlations by column.
SelectOverlyCorrelatedFeatures <- function(df, label_col, cutoff = 0.7) {
  correlation_matrix <- GetCorrelation(df, label_col = label_col)
  return (findCorrelation(correlation_matrix, cutoff = cutoff))
}
# Uses SelectOverlyCorrelatedFeatures given a cutoff to identify the heavily correlated columns
# and then removes them.
# Parameters: df dataframe; 
#             label_col: a string: the name of the label column you need to exclude; 
#             cutoff, the maximum correlation allowed.
# Returns:    temp: the dataframe with the removed features and dataframe.
RemoveHighlyCorrelatedFeatures <- function(df, label_col, cutoff = 0.7) {
  temp <- df %>%
    select(-label_col)
  
  heavily_correlated_features <- SelectOverlyCorrelatedFeatures(df, label_col, cutoff)
  temp = temp[-heavily_correlated_features]
  temp <- add_column(temp, select(df, label_col))
  return (temp)
}