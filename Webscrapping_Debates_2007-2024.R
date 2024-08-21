install.packages(c("tidyverse", 
                   "httr", "glue", "dplyr", "stringr"))
library(dplyr)
library("tidyverse")
library("httr")
library("rvest")
library("xml2")
library("glue")
library("rvest")
library("xml2")
library("glue")
library(stringr)

SearchTerms <- c("European+Union","European+Commission","European+Council", "Brexit")
TitleTerms <- c("European Union","European Commission","European Council", "Brexit")
StartDate = "2007-06-27"
EndDate = "2024-08-19"
base_url = "https://www.theyworkforyou.com"

# Initialize a data frame to store the results
results_df <- data.frame(
  speech_id = character(),
  debate_date = character(),
  debate_title = character (),
  speech_content = character(),
  speaker_name = character(),
  speaker_party = character(),
  stringsAsFactors = FALSE
)

# Initialize vectors to store all titles and links
all_titles <- c()
all_links <- c()

# Assuming SearchTerms, base_url, StartDate are already defined in your environment

for (i in seq_along(SearchTerms)) {
  link <- paste0(base_url, "/search/?q=", SearchTerms[i], "&phrase=&exclude=&from=", StartDate, "&to=", EndDate,"&person=&section=debates&column=")
  print(link)
  Sys.sleep(sample(3:4, 1))  # Random delay between 3 to 5 seconds
  
  # Error handling for the main page
  page <- tryCatch({
    read_html(link)
  }, error = function(e) {
    message("Error accessing main search page: ", link)
    return(NULL)
  })
  
  if (is.null(page)) next  # Skip to the next iteration if the page is NULL
  
  # Extract titles and links from the first page
  titles <- page %>%
    html_nodes(".search-result__title a") %>%  # Adjust the CSS selector if needed
    html_text()
  
  links <- page %>%
    html_nodes(".search-result__title a") %>%  # Adjust the CSS selector if needed
    html_attr("href")
  
  # Combine them with all_titles and all_links
  all_titles <- c(all_titles, titles)
  all_links <- c(all_links, links)
  
  # Get the final page number
  final_page_number <- page %>%
    html_nodes(".search-result-pagination a[title='Final page']") %>% 
    html_attr("href") %>% 
    str_extract("p=\\d+") %>% 
    str_remove("p=") %>% 
    as.numeric()
  
  # Loop through all subsequent pages and extract titles and links
  if (!is.na(final_page_number) && length(final_page_number) > 0&& final_page_number > 1) {
    for (j in 2:final_page_number) {
      next_link <- paste0(base_url, "/search/?q=", SearchTerms[i], "&from=", StartDate, "&to=", EndDate, "&p=", j)
      print(next_link)
      
      Sys.sleep(sample(3:4, 1))  # Random delay between 3 to 5 seconds
      
      # Error handling for subsequent pages
      next_page <- tryCatch({
        read_html(next_link)
      }, error = function(e) {
        message("Error accessing subsequent search page: ", next_link)
        return(NULL)
      })
      
      if (is.null(next_page)) next  # Skip to the next iteration if the page is NULL
      
      # Extract titles and links from the current page
      titles <- next_page %>%
        html_nodes(".search-result__title a") %>%
        html_text()
      
      links <- next_page %>%
        html_nodes(".search-result__title a") %>%
        html_attr("href")
      
      # Append them to all_titles and all_links
      all_titles <- c(all_titles, titles)
      all_links <- c(all_links, links)
    }
  } else {
    print("Only one page exists or final page number not found.")
  }
  
  # Filter links to only include ones with the search term, European, Europe which are not
  # Written answers.
  filtered_links <- all_links[
    grepl(paste(TitleTerms[i], "European", "Europe", sep="|"), all_titles, ignore.case = TRUE) & 
      !grepl("Written Answer", all_titles, ignore.case = TRUE)
  ]
  
  print(filtered_links)
  
  if (length(filtered_links) > 0){
    for (link_debate in filtered_links){
      Sys.sleep(sample(3:4,1))  # Random delay between 3 to 5 seconds
      
      # Find debate pages
      debate_page <- tryCatch({
        read_html(paste0(base_url,link_debate))
      }, error = function(e) {
        message("Error accessing debate page: ", link_debate)
        return(NULL)
      })
      
      if (is.null(debate_page)) next  # Skip to the next iteration if the page is NULL
      
      # Extract debate date
      debate_date <- debate_page %>%
        html_node("p.lead a") %>%
        html_text(trim = TRUE)
      
      # Extract the debate title
      debate_title <- debate_page %>%
        html_node("div.debate-header__content h1") %>%
        html_text(trim = TRUE)
      
      # Extract debate speeches
      speeches <- debate_page %>%
        html_nodes(".debate-speech")
      
      # Loop through each speech node to extract the required information
      for (speech in speeches) {
        # Get the speech ID
        speech_id <- speech %>% 
          html_attr("id")
        
        # Get the speaker's name
        speaker_name <- speech %>%
          html_node(".debate-speech__speaker .debate-speech__speaker__name") %>%
          html_text(trim = TRUE)
        
        # Get the speaker's position (if it exists)
        speaker_party <- speech %>%
          html_node(".debate-speech__speaker .debate-speech__speaker__position") %>%
          html_text(trim = TRUE)
        
        # Get the speech content
        speech_content <- speech %>%
          html_node(".debate-speech__content") %>%
          html_text(trim = TRUE)
        
        results_df <- results_df %>%
          add_row(
            speech_id = speech_id,  # Character type
            debate_date = debate_date,  # Character type
            debate_title = debate_title,  # Character type
            speech_content = speech_content,  # Character type
            speaker_name = speaker_name,  # Character type
            speaker_party = speaker_party  # Character type
          )
      }
    }
  } else{
    print("No debate found for the specific topics.")
  }
}

write.csv(results_df, "C:/Users/leonw/OneDrive - KU Leuven/2nd Semester/Collecting Big Data for Social Science/Final Assignment/Parliamentary_Debates.csv")

