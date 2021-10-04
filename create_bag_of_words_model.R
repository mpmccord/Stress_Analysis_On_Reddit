library(tidytext)
library(tidyverse)
library(stringr)
# CleanText
# Takes in a dataframe with columns id, text, and creates a dataframe of words found in each id.
# Parameters: df: a dataframe that includes a text column of words and an id column
# Returns: cleaned_text: a long dataframe with the words found in df.
CleanText <- function(df) {
  # Creating list of words occuring in document
  # Select the text and the id to keep track of where the id occurred
  # Remove stopwords
  # Remove punctuation
  # Replace digits with d%d
  temp <- df %>%
    select(text, id) %>%
    unnest_tokens(word, text) %>%
    mutate(word = gsub('[[:punct:]]+','', word)) %>%
    mutate(word = gsub('\\<[[:digit:]]+\\>', '%d%', word)) %>%
    # mutate(word = gsub("id\\b", "text_id", word))
    anti_join(stop_words, by = c("word" = "word")) %>%
  return(temp)
}
# CreateBagOfWords
# Takes in a dataframe with columns id, text, and creates a dataframe of words found in each id.
# Parameters: df: a dataframe that includes a text column of words and an id column
# Returns: cleaned_text: a long dataframe with the words found in df.
CreateBagOfWords <- function(df) {
  cleaned_text <- CleanText(df)
  token_text <- cleaned_text %>%
    group_by(id) %>%
    count(word) %>%
    tibble() %>%
    pivot_wider(names_from = word, values_from = n, names_repair = "unique") %>%
    mutate_all(~replace(., is.na(.), 0))
  
  return (token_text)
}
# Similar to CreateBagOfWords() but generalizes better to untrained data. 
# In particular, the bottom 20% of words are replaced with "unknown"
# Parameters: 
#             df: a dataframe, 
#             smallest_count: the smallest number of occurrences you want to exclude (default: 1).
CreateBagOfWordsWithUnknowns <- function(df, rare_defn=10) {
  # Smallest count allowed: removing the words that only appear a certain number of times.
  # If we look at the base dataset, the median number of times a word appears is once.
  # Since this is not meaningful, we will only include the words that appear more than once.
  # First, let's get the words
  words_and_id = ReplaceRareWords(df)
  # Now, let's replace the rare words with the unknown marker
  
  # Now creating the dataframe of each id and each bag of words model.
  # We need to group by id (to preserve words by posts, 
  #                     count the word by corresponding id
  #                     pivot_wider so that we get the words as columns rather than rows
  #                     and then replace NAs with 0 (since if a word is NA, it does not appear))
  token_text <- words_and_id %>%
    group_by(id) %>%
    count(word) %>%
    tibble() %>%
    pivot_wider(names_from = word, values_from = n, names_repair = "unique") %>%
    mutate_all(~replace(., is.na(.), 0))
  
  return (token_text)
}
# Identifies the rare words based on a rare defn
# Parameters: df: a dataframe with a text column and id column
#             rare_defn: the minimum word count to not be classified as rare
GetRareWords <- function(df, rare_defn = 10) {
  rare_words <- CleanText(df) %>%
    count(word) %>%
    filter(n < rare_defn)
  return (rare_words$word)
}
# Based on get_rare words, 
# Parameters: df: a dataframe with a text column and id column
#             rare_defn: the minimum word count to not be classified as rare
ReplaceRareWords <- function(df, rare_defn = 10) {
  rare_words <- GetRareWords(df, rare_defn)
  rare_length <- length(rare_words)
  rare_words <- paste(rare_words, collapse = "| ")
  replacement <- "unk"
  words_and_id <- CleanText(df) %>%
    mutate(word = str_replace_all(word, rare_words, replacement = replacement))
  return (words_and_id)
}
