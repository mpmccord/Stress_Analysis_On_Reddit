library(tidyverse)
library(tidytext)
library(stringr)
# This is a set of functions for getting the word frequency distribution and displaying them by label.
# Major functions:
#   - GetTopNWordFrequencies: counts the top n word frequency and sorts them in descending order.
#   - GetWordFrequenciesByCategory: runs GetTopNWordFrequencies on a subset of the data (i.e. label, subreddit)
#   - PlotFrequencyDistribution:  takes in the result of one of these two functions and creates a barplot of the word frequencies.

# From a function of text counts, counts the top n word frequencies
# Parameters: df, a tibble; text_col: the column where text is; n: the number of words you want to plot
# Returns: dataframe of word frequencies and counts.
GetTopNWordFrequencies <- function(df, text_col = "text", n = 20) {
  # Placeholder
  return(0)
}

# Uses GetTopNWordFrequencies to get a subset of the data filtered by group.
# Parameters: df: a  tibble
# Returns: dataframe of word frequencies and counts, separated by group.
GetWordFrequenciesByCategory <- function(df, my_col, n = 20) {
  # Placeholder
  return(0)
}