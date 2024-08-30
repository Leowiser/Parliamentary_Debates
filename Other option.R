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

SearchTerm <- "(\"EU\"+OR+\"European+Commission\"+OR+\"European+Union\"+OR+\"European+Council\"+OR+\"Brexit\")"
TitleTerms <- c("European Union","European Commission","European Council", "Brexit")

StartDate <- c("2012-10-25", "2012-12-25", "2013-02-25", "2013-04-25", 
               "2013-06-25", "2013-08-25", "2013-10-25", "2013-12-25", 
               "2014-02-25", "2014-04-25", "2014-06-25", "2014-08-25", 
               "2014-10-25", "2014-12-25", "2015-02-25", "2015-04-25", 
               "2015-06-25", "2015-08-25", "2015-10-25", "2015-12-25", 
               "2016-02-25", "2016-04-25", "2016-06-25", "2016-08-25", 
               "2016-10-25", "2016-12-25", "2017-02-25", "2017-04-25", 
               "2017-06-25", "2017-08-25", "2017-10-25", "2017-12-25", 
               "2018-02-25", "2018-04-25", "2018-06-25", "2018-08-25", 
               "2018-10-25", "2018-12-25", "2019-02-25", "2019-04-25", 
               "2019-06-25", "2019-08-25", "2019-10-25", "2019-12-25", 
               "2020-02-25", "2020-04-25", "2020-06-25", "2020-08-25", 
               "2020-10-25", "2020-12-25", "2021-02-25", "2021-04-25", 
               "2021-06-25", "2021-08-25", "2021-10-25", "2021-12-25", 
               "2022-02-25", "2022-04-25", "2022-06-25", "2022-08-25", 
               "2022-10-25")

EndDate <- c("2012-12-25", "2013-02-25", "2013-04-25", "2013-06-25", 
             "2013-08-25", "2013-10-25", "2013-12-25", "2014-02-25", 
             "2014-04-25", "2014-06-25", "2014-08-25", "2014-10-25", 
             "2014-12-25", "2015-02-25", "2015-04-25", "2015-06-25", 
             "2015-08-25", "2015-10-25", "2015-12-25", "2016-02-25", 
             "2016-04-25", "2016-06-25", "2016-08-25", "2016-10-25", 
             "2016-12-25", "2017-02-25", "2017-04-25", "2017-06-25", 
             "2017-08-25", "2017-10-25", "2017-12-25", "2018-02-25", 
             "2018-04-25", "2018-06-25", "2018-08-25", "2018-10-25", 
             "2018-12-25", "2019-02-25", "2019-04-25", "2019-06-25", 
             "2019-08-25", "2019-10-25", "2019-12-25", "2020-02-25", 
             "2020-04-25", "2020-06-25", "2020-08-25", "2020-10-25", 
             "2020-12-25", "2021-02-25", "2021-04-25", "2021-06-25", 
             "2021-08-25", "2021-10-25", "2021-12-25", "2022-02-25", 
             "2022-04-25", "2022-06-25", "2022-08-25", "2022-10-25", 
             "2022-12-25")
base_url = "https://www.theyworkforyou.com"

for(i in 1:length(StartDate)){
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
    
  link <- paste0(base_url, "/search/?q=%28%22EU%22+OR+%22European+Commission%22+OR+%22European+Union%22+OR+%22European+Council%22+OR+%22Brexit%22%29+",StartDate[i],"..",EndDate[i],"+section%3Adebates+section%3Awhall&p=1")
    
  # Error handling for the main page
  page <- tryCatch({
    read_html(link)
  }, error = function(e) {
    message("Error accessing main search page: ", link)
    return(NULL)
  })
    
  # Extract titles and links from the first page
  titles <- page %>%
    html_nodes(".search-result__title a") %>%  # Adjust the CSS selector if needed
    html_text()
  
  links <- page %>%
    html_nodes(".search-result__title a") %>%  # Adjust the CSS selector if needed
    html_attr("href")
    
  # Filter links that start with "/debates"
  debate_links <- links[grep("^/debates", links)]
  
  clean_link <- gsub("#.*$", "", links)
    
  # Combine them with all_titles and all_links
  all_titles <- c(all_titles, titles)
  all_links <- c(all_links, clean_link)

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
      next_link <- paste0(base_url, "/search/?q=%28%22EU%22+OR+%22European+Commission%22+OR+%22European+Union%22+OR+%22European+Council%22+OR+%22Brexit%22%29+",StartDate[i],"..",EndDate[i],"+section%3Adebates+section%3Awhall&p=", j)
        
      #Sys.sleep(sample(3:4, 1))  # Random delay between 3 to 5 seconds
        
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
      
      # Filter links that start with "/debates"
      debate_links <- links[grep("^/debates", links)]
        
      clean_link <- gsub("#.*$", "", links)
        
      # Combine them with all_titles and all_links
      all_titles <- c(all_titles, titles)
      all_links <- c(all_links, clean_link)
    }
  } else {
    print("Only one page exists or final page number not found.")
  }
    
  # Filter links to only include ones with European, Europe, and EU in the title
  # which are not Written answers.
  filtered_links <- all_links[
    grepl(paste("European", "Europe", "EU", sep="|"), all_titles, ignore.case = TRUE) & 
      !grepl("Written Answer", all_titles, ignore.case = TRUE)
  ]
  
    for (link_debate in filtered_links){
      #Sys.sleep(sample(3:4,1))  # Random delay between 3 to 5 seconds
      
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
  
  output_file <- file.path("C:/Users/leonw/OneDrive - KU Leuven/2nd Semester/Collecting Big Data for Social Science/Final Assignment", paste0("Parliamentary_Debates_", i, ".csv"))
  write.csv(results_df, output_file, row.names = FALSE)
  print("Iteration completed")
  Sys.sleep(30)
}


setwd("C:/Users/leonw/OneDrive - KU Leuven/2nd Semester/Collecting Big Data for Social Science/Final Assignment")

csv_files <- list.files(pattern = "^Parliamentary_Debates_.*\\.csv$")

list_of_data_frames <- lapply(csv_files, read.csv)

# Step 4: Combine all data frames into one using do.call and rbind
combined_data_frame <- do.call(rbind, list_of_data_frames)

combined_data_frame$year <- as.integer(substr(combined_data_frame$debate_date, nchar(combined_data_frame$debate_date) - 4 + 1, nchar(combined_data_frame$debate_date)))

n_distinct(combined_data_frame$year)

df_clean <- distinct(combined_data_frame)

write.csv(df_clean, "C:/Users/leonw/OneDrive - KU Leuven/2nd Semester/Collecting Big Data for Social Science/Final Assignment/Parliamentary_Debates_Combined.csv")

n_distinct(df_clean$debate_title)
