library(tidytext)
library(tidyverse)
# CleanText
# Takes in a dataframe with columns id, text, and creates a dataframe of words found in each id.
# Parameters: df: a dataframe that includes a text column of words and an id column
# Returns: cleaned_text: a long dataframe with the words found in df.
CleanText <- function(df) {
  # Creating list of words occuring in document
  # Select the text and the id to keep track of where the id occurred
  # Remove stopwords
  temp <- df %>%
    select(text, id) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = c("word" = "word"))
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
CreateBagOfWordsWithUnknowns <- function(df, smallest_count=1) {
  # Smallest count allowed: removing the words that only appear a certain number of times.
  # If we look at the base dataset, the median number of times a word appears is once.
  # Since this is not meaningful, we will only include the words that appear more than once.
  
  # First, let's get the words and group them by id
  words_and_id <- df %>%
    select(text, id) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = c("word" = "word")) #%>%
    #count(word)
  # mutate_all(~replace(., is.na(.), 0))
  
  # Now let's filter out the words that only appear once and mark them as "unknown"
  text_counts <- words_and_id %>%
    count(word)
  bottom_percentile_of_words <- filter(text_counts, n <= smallest_count)$word
  words_and_id <- words_and_id %>%
    mutate(word = str_replace_all(word, pattern = bottom_percentile_of_words, replacement = rep("unknown", length(bottom_percentile_of_words))))
  
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