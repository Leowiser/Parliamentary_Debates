set.seed(123)  # For reproducibility
sampled_speeches <- df_clean %>%
  sample_n(20)  # Randomly sample 20 speeches

sampled_speeches <- df_clean[1:20, ]

# Extract the text column for easier manipulation
texts <- sampled_speeches$text

neat_text <- function(text) {
  temp <- tolower(text)
  temp <- gsub("[[:punct:][:blank:]]+", " ", temp)
  corp_temp <- corpus(temp)
  toks <- tokens(corp_temp)
  # toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove")
  return(toks)
}

texts_cleaned <- lapply(texts, neat_text)

# Convert tokens to a simple list format for easier manipulation
texts_cleaned_list <- lapply(texts_cleaned, as.list)


# Convert the sentiment dictionary to a list format
sentimentdict_list <- as.list(sentimentdict)

# Function to check sentiment for each word in the text
check_sentiment <- function(words, sentimentdict_list) {
  word_sentiments <- sapply(words, function(word) {
    if (word %in% sentimentdict_list$pos) {
      return("positive")
    } else if (word %in% sentimentdict_list$neg) {
      return("negative")
    } else {
      return("neutral")
    }
  })
  return(word_sentiments)
}

# Iterate over each text and print the words with their sentiment classification
for (i in 1:length(texts_cleaned_list)) {
  cat("\nSpeech", i, ":\n")
  words <- unlist(texts_cleaned_list[[i]])
  sentiments <- check_sentiment(words, sentimentdict_list)
  
  # Print each word with its sentiment
  for (j in 1:length(words)) {
    cat(words[j], ":", sentiments[j], "\n")
  }
}
