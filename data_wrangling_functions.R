cleanText <- function(df) {
  cleaned_text <- df %>%
    select(text) %>%
    # Creating bag of words representation
    unnest_tokens(word, text) %>%
    # Removing stopwords
    anti_join(stop_words, by = c("word" = "word"))
  return(cleaned_text)
}