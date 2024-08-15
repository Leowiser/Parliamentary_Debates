install.packages(c("tidyverse", 
                   "httr", "jsonlite", "glue", 
                   "data.table"))
install.packages("quanteda")
install.packages("topicmodels")
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

links <- read_xlsx(path="C:/Users/leonw/OneDrive - KU Leuven/Desktop/British Parliament Debates.xlsx")


my_data <- read.delim("C:/Users/leonw/Downloads/UK’s Exit from the European Union 2023-04-24.txt")


base_url <- "https://hansard.parliament.uk"

# Extracting the links from the second row of the 'links' data frame
link_list <- as.character(links[, 2])
print(link_list[1])
# Initialize a data frame to store the results
results_df <- data.frame(
  contribution_id = character(),
  debate_date = character(),
  debate_title = character (),
  debate_id = character (),
  paragraphs = character(),
  primary_info = character(),
  secondary_info = character(),
  stringsAsFactors = FALSE
)

# Base URL (if needed for constructing full URLs from relative links)
base_url <- "https://hansard.parliament.uk"

# Loop through each row of the links data frame
for (i in 1:nrow(links)) {
  # Extract the debate ID (row number) and ensure it's an integer
  debate_id <- as.integer(i)
  
  # Extract the debate date from the first column
  debate_date <- as.character(links[i, 1])
  
  # Extract the link from the second column of the current row
  link <- as.character(links[i, 2])
  
  # Extract the debate title from the link (the part after the last '/')
  debate_title <- sub('.*/', '', link)
  
  tryCatch({
    # Read the HTML page for the current link
    page <- read_html(link)
    
    # Extract all contributions
    contributions <- page %>%
      html_nodes('.contribution')
    
    # Loop through each contribution
    for (contribution in contributions) {
      # Get the data-contribution-id
      contribution_id <- contribution %>%
        html_attr('data-contribution-id')
      
      # Get all <p> elements with the class hs_Para within the current contribution
      paragraphs <- contribution %>%
        html_nodes('p.hs_Para') %>%
        html_text(trim = TRUE)
      
      # Find the link to the member contributions page
      member_link <- contribution %>%
        html_node('a.attributed-to-details') %>%
        html_attr('href')
      
      if (!is.na(member_link)) {
        # Construct the full URL for the member page if necessary
        member_url <- paste0(base_url, member_link)
        # Read the member contributions page
        member_page <- read_html(member_url)
        
        # Extract primary info and secondary info from the member page
        primary_info <- member_page %>%
          html_node('.primary-info') %>%
          html_text(trim = TRUE)
        
        secondary_info <- member_page %>%
          html_node('.secondary-info') %>%
          html_text(trim = TRUE)
        
        # Store the results in the data frame with consistent data types
        results_df <- results_df %>%
          add_row(
            debate_id = debate_id,  # Integer type
            debate_date = debate_date,  # Character type
            debate_title = debate_title,  # Character type
            contribution_id = as.character(contribution_id),  # Character type
            paragraphs = paste(paragraphs, collapse = " "),  # Character type
            primary_info = primary_info,  # Character type
            secondary_info = secondary_info  # Character type
          )
      } else {
        message(sprintf("No member link found for contribution ID: %s", contribution_id))
      }
    }
  }, error = function(e) {
    message(sprintf("Failed URL: %s. Error: %s", link, e$message))
  })
}
# Print the results
print(results_df)


for(i in range(54)){
  page <- str(links[i,1])
  
  print(page)
}
  page <- read_html("link")
  # Extract all contributions
  contributions <- page %>%
    html_nodes('.contribution')
  member_link <- contribution %>%
    html_node('a.attributed-to-details') %>%
    html_attr('href')
  type(member_link)
  
  # Loop through each contribution
  for (contribution in contributions) {
    # Get the data-contribution-id
    contribution_id <- contribution %>%
      html_attr('data-contribution-id')
    
    # Get all <p> elements with the class hs_Para within the current contribution
    paragraphs <- contribution %>%
      html_nodes('p.hs_Para') %>%
      html_text(trim = TRUE)
    
    # Find the link to the member contributions page
    member_link <- contribution %>%
      html_node('a.attributed-to-details') %>%
      html_attr('href')
    
    if (!is.na(member_link)){
      # Construct the full URL
      member_url <- paste0(base_url, member_link)
      # Read the member contributions page with a timeout
      print(member_url)
      tryCatch({
      member_page <- read_html(member_url)
        
      # Extract primary info and secondary info from the member page
      primary_info <- member_page %>%
        html_node('.primary-info') %>%
        html_text(trim = TRUE)
        
      secondary_info <- member_page %>%
        html_node('.secondary-info') %>%
        html_text(trim = TRUE)
        
      # Store the results in the data frame
      results_df <- results_df %>%
        add_row(
          contribution_id = contribution_id,
          paragraphs = paragraphs,
          primary_info = primary_info,
          secondary_info = secondary_info
        )
      }, error = function(e) {
        message(sprintf("Failed URL: %s. Error: %s", member_url, e$message))
      })
      }
    else {
      message(sprintf("No member link found for contribution ID: %s", contribution_id))
    }
  }
}

install.packages("sentimentr")
install.packages("tidytext")

library(tidytext)
library(dplyr)
library(sentimentr)

# Perform sentiment analysis
sentiment_scores <- sentiment(results_df$paragraphs)

# Print sentiment scores
print(sentiment_scores)

results_df$paragraphs[3]


# Split the text into sentences
sentences <- get_sentences(results_df$paragraphs)

text <- "I apologise, Mr Deputy Speaker.
The Prime Minister is in step with the British public. A referendum is only right. The EU has fundamentally changed since we first joined in the early ’70s and it continues to change because of the eurozone crisis. The answer to the crisis from the eurozone capitals is more Europe—more political and economic integration. They have realised somewhat belatedly that they cannot have monetary union and save the euro without fiscal union, but that is not why we joined the EU. We joined for trade, not for politics.
No one can deny that the EU’s role in our daily lives, which some would describe as meddling, has grown over the decades and continues to grow, and yet we have not stopped to ask the fundamental question of whether that is in our best interests. The timing of the referendum is sensible in that it allows for a renegotiation so we can know what the “in” part of the referendum question is. I wish the Prime Minister well—it will be a hard road because the direction of travel is in the other direction—but I hope he can renegotiate a looser agreement or arrangement with the EU that focuses on trade and not on politics. He might well be able to do so, which would appeal to a great number of people in this country. I hope he does more than Prime Minister Harold Wilson did in 1975. He claimed he had renegotiated and repatriated a lot of powers, but under close scrutiny, it appeared to be a thin claim—it did not amount to a tin of beans.
Delaying the referendum a touch allows the eurozone crisis to play out and for a proper debate on the merit of membership. All in all, it is a sensible policy. It is right for the country. The British people will finally have their say, having been barred from having a genuine choice by the political establishment for probably more than 30 years, because all the main parties have looked in one direction.
That is good news, and we welcome it, and yet the policy is dependent on a Conservative victory in the 2015 general election. The Prime Minister made his
Toggle showing location of Column 321
promise as leader of the Conservative party. Legislation will be introduced immediately after a Conservative victory, so this has become a party political issue. As such, many are concerned that there is deep public mistrust of politicians who make promises about EU referendums, because too many have been broken in the past. We question whether the promise will be believed.
Many people remember Tony Blair’s promise on the EU constitution on the Lisbon treaty. We were promised a referendum and he failed to deliver. Instead, the EU constitution was copied and pasted into the Lisbon treaty and rammed through the House using the Labour Government’s majority. Even Gordon Brown knew"

data <- data.frame(text = text, stringsAsFactors = FALSE)

# Split the text into sentences
sentences <- get_sentences(data$text)
# Filter sentences mentioning the EU
eu_sentences <- sentences[grepl("EU", sentences)]
eu_sentences_df <- data.frame(sentence = eu_sentences, stringsAsFactors = FALSE)
# Perform sentiment analysis on EU sentences
eu_sentiment_scores <- sentiment(eu_sentences_df$c..I.apologise..Mr.Deputy.Speaker.....The.Prime.Minister.is.in.step.with.the.British.public....)

# Print EU sentiment scores
print(eu_sentiment_scores)
