library("tidyverse")
library("httr")
library("jsonlite")
library("rvest")
library("xml2")
library("glue")
library("data.table")
library("rvest")
library("xml2")
library("glue")
library("data.table")
library("quanteda")
library("topicmodels")
library(readxl)
library(tm)
library(tidytext)
library(dplyr)
library(lubridate)    # for date conversion in data cleaning

setwd("C:/Users/LENOVO/Desktop/iskola stuff/mester stuff/Collecting and Analyzing Big Data for Social Sciences/Group Project")

#### Data cleaning ####
df_debates = read.csv('Parliamentary_Debates_Combined.csv', row.names = 1)

# Delete all the duplicates
df_debates <- distinct(df_debates)

n_distinct(df_debates$speech_id)
n_distinct(df_debates$speech_content)

# Delete rows that have no speech (NA-s)
df_clean <- df_debates[complete.cases(df_debates$speech_content), ]

# Format the debate_date column so that it contains dates not character strings
df_clean$debate_date <- dmy(df_clean$debate_date)


#### Simple sentiment analysis ####
poswords = "https://cssbook.net/d/positive.txt"
negwords = "https://cssbook.net/d/negative.txt"

pos = scan(poswords, what = "list")
neg = scan(negwords, what = "list")

sentimentdict = dictionary(list(pos = pos, neg = neg))

# Need to rename the column of the speeches to 'text' because the corpus() function from the following code needs that naming convention
names(df_clean)[names(df_clean) == "speech_content"] <- "text"

# Match and calculate the sentiments
sentiment_speech = df_clean %>% 
  corpus() %>% 
  tokens() %>% 
  dfm() %>% 
  dfm_lookup(sentimentdict) %>% 
  convert(to = "data.frame") %>%
  mutate(sent = pos-neg)

df_clean <- cbind(df_clean, sentiment_speech[,-1])

df_clean %>% 
  group_by(debate_title) %>% 
  summarise(mean_sent = mean(sent)) %>%
  ggplot(aes(x = mean_sent, y = debate_title)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  theme_minimal()

# This is probably incorrect, would need grouping like before imo
ggplot(df_clean, aes(x = debate_date, y = sent)) +
  geom_line(color='steelblue') +
  xlab("Year") +
  theme_minimal()


### POS tagging ####
library(udpipe)

# load text
text <- readLines("https://slcladal.github.io/data/testcorpus/linguistics06.txt", skipNul = T)
# clean data
text <- text %>%
  str_squish() 

# Download english model - enough to do once
m_eng <- udpipe_download_model(language = "english-ewt")

# Get the path of the downloaded model
m_eng_path <- m_eng$file_model

# Load the english model from the path --> no need to download every time
m_eng_loaded <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

# tokenise, tag, dependency parsing
text_anndf <- udpipe_annotate(m_eng_loaded, x = text) %>%
  as.data.frame() %>%
  select(-sentence)
# inspect
head(text_anndf, 10)

# POS tagging with real data
start_time <- Sys.time()
test <- udpipe_annotate(m_eng_loaded, x = df_clean$text) %>% 
  as.data.frame()
end_time <- Sys.time()

end_time-start_time

# Compare it with Spacy - the previous (UDPipe) is better, e.g. because of 'co-operation'. But sometimes space is better e.g. 'collaborating'
library(spacyr)
spacy_initialize(model = "en_core_web_sm")

test2 <- spacy_parse(df_clean$text, lemma = TRUE, pos = TRUE, tag = TRUE)

# So I will use UDPipe --> test df
# Need to map the POS tags (upos) to the 5 POS tags in the lexicon --> DOUBLE CHECK WITH WHOLE, BIGGER DATASET!!! (might be more POS tags than here)
unique(test$upos)
# NOUN --> n
# VERB --> v
# PUNCT --> DELETE (punctuations, doesn't have sentiment)
# CCONJ --> DELETE (e.g. and, or, but etc., they don't have sentiment)
# SCONJ --> DELETE (e.g. that, while, if, as, because etc., they don't have sentiment)
# NUM --> DELETE (numbers, they don't have sentiment)
# PROPN --> DELETE (proper nouns, e.g. Gentleman, Member, House, European etc., no sentiment)
# AUX --> DELETE (auxiliary, e.g. can, have, am etc., no sentiment)
# PRON --> DELETE (pronoun, e.g. I, he, she, it, anyone etc., no sentiment)
# ADJ --> a
# PART --> DELETE (particle, e.g. 's, not, to etc., no sentiment)
# DET --> DELETE (determiner, e.g. a, the, what etc., no sentiment)
# ADP --> DELETE (adposition, e.g. in, to, by, with etc., no sentiment)
# ADV --> r
# SYM --> DELETE (symbol, e.g. %, $ etc., no sentiment)
# INTJ --> u
# X --> DELETE

negating_words <- c("not", "no", "never", "neither", "nor", "less")
test %>%
  # Last 6 columns not needed
  select(-tail(names(.), 6)) %>%
  # Create 'valence' column as defined in the paper
  mutate(valence = ifelse(lag(lemma) %in% negating_words & lead(upos) == "PUNCT", 1, 0)) %>% view()
  # Keep relevant POS tags only
  filter(upos %in% c('NOUN', 'VERB', 'ADJ', 'ADV', 'INTJ')) -> test_filtered
  
  
  
  
test %>%
  # Create a column to indicate whether the lemma is a negating word
  mutate(is_negation = lemma %in% negating_words,
         
         # Create a column to indicate whether the word is punctuation based on the upos column
         is_punctuation = upos == "PUNCT") %>%
  
  # Create a running flag to identify the region between a negating word and punctuation
    mutate(punctuation_block = cumsum(is_punctuation),
          negation_block = cumsum(is_negation)) %>%
  # Group by sentence and negation_block to check within each block
    group_by(punctuation_block, negation_block) %>%
  
  # Check if any negation exists in the current block and assign valence accordingly
  mutate(valence = ifelse(any(is_negation), 1, 0)) %>% 
  
  # Ungroup to remove grouping structure
  ungroup() %>%
  # Clean up by removing temporary columns if desired
  select(-is_negation, -is_punctuation, -negation_block, -negation_block) %>% view()

  
  
  
  
  
  

# Load the lexicon from the paper trained on parliamentary speeches
sentiment_lexicon <- read.csv("lexicon-polarity.csv")

sentiment_lexicon %>%
  # Mapping
  mutate(pos1 = case_when(pos1 == 'n' ~ 'NOUN',
                          pos1 == 'v' ~ 'VERB',
                          pos1 == 'a' ~ 'ADJ',
                          pos1 == 'r' ~ 'ADV',
                          pos1 == 'u' ~ 'INTJ')) %>%
  # Remove inrelevant column
  select(-seed) -> sentiment_lexicon


test_filtered %>% 
  # Map polarity values
  left_join(sentiment_lexicon, by = c('lemma' = 'lemma', 'upos' = 'pos1')) %>%
  # Calculate sentiment based on paper
  mutate(sentiment = polarity*(1-valence)) %>% 
  # Calculate sum of sentiment for each speech and number of words that had a sentiment
  group_by(doc_id) %>% 
    summarise(sentiment_sum = sum(replace_na(sentiment, 0)),
              sentiment_count = sum(!is.na(sentiment) & sentiment != 0)) -> sentiments_by_speech


df_clean %>% 
  # Add the doc_id column to df_clean to join on it in the subsequent step
  mutate(doc_id = paste0('doc', row_number())) %>% 
  left_join(sentiments_by_speech, by = 'doc_id') %>%
  # Replace NA values with 0 in the sentiment_sum and sentiment_count columns
  mutate(across(c(sentiment_sum, sentiment_count), ~ replace_na(.x, 0))) -> df_clean


# Visualize
mblue <- "#002A48"
pblue <- "#3F5F75"

df_clean %>% 
  group_by(debate_date) %>% 
  summarise(average_sentiment = ifelse(sum(sentiment_count) == 0, 0, sum(sentiment_sum)/sum(sentiment_count))) %>%
  mutate(sentismooth = smooth.spline(average_sentiment, spar = 0.5)$y) %>%
  ggplot(aes(x = debate_date)) +
  geom_line(aes(y = average_sentiment), size = .1, colour=pblue) +
  geom_point(aes(y = average_sentiment), size = 6, colour=pblue, shape=1, stroke=.25) +
  geom_line(aes(y = sentismooth), size = 2, colour=mblue) +
  xlab("Date") +
  ylab('Emotional Polarity') +
  theme_minimal()




df_clean %>% 
  mutate(quarter = paste0(year(debate_date), "-Q", quarter(debate_date))) %>% 
  group_by(quarter) %>%
  summarise(average_sentiment = sum(sentiment_sum) / sum(sentiment_count)) %>% view()


df_clean %>% 
  mutate(quarter = paste0(year(debate_date), "-Q", quarter(debate_date))) %>% 
  group_by(quarter) %>%
  summarise(average_sentiment = ifelse(sum(sentiment_count) == 0, 0, sum(sentiment_sum) / sum(sentiment_count))) %>%
  mutate(standardized_sentiment = scale(average_sentiment), 
         sentismooth = smooth.spline(standardized_sentiment, spar = 0.5)$y) %>% 
  # Convert the quarter to a factor ordered by time
  mutate(quarter = factor(quarter, levels = unique(quarter))) %>% 
  ggplot(aes(x = quarter, group = 1)) +
  geom_line(aes(y = standardized_sentiment), size = .1, colour = pblue) +
  geom_point(aes(y = standardized_sentiment), size = 6, colour = pblue, shape = 1, stroke = .25) +
  geom_line(aes(y = sentismooth), size = 2, colour = mblue) +
  xlab("Date") +
  ylab('Emotional Polarity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df_clean %>% 
  group_by(year) %>%
  summarise(average_sentiment = ifelse(sum(sentiment_count) == 0, 0, sum(sentiment_sum) / sum(sentiment_count))) %>%
  mutate(standardized_sentiment = scale(average_sentiment), 
         sentismooth = smooth.spline(standardized_sentiment, spar = 0.5)$y) %>% 
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = standardized_sentiment), size = .1, colour = pblue) +
  geom_point(aes(y = standardized_sentiment), size = 6, colour = pblue, shape = 1, stroke = .25) +
  geom_line(aes(y = sentismooth), size = 2, colour = mblue) +
  xlab("Date") +
  ylab('Emotional Polarity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
