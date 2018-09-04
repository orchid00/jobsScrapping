source(file = "scripts/01_words.R")

library(forcats)
library(tidyverse)

# set url for search ------------------------------------------------------
# Indeed Search Words
base_url <- "https://www.indeed.com/"
job_title <- "data+scientist"
location <- "United+States"

adv_url <- paste0(base_url, 
                  "jobs?as_and=&as_phr=&as_any=&as_not=&as_ttl=", job_title, 
                  "&as_cmp=&jt=all&st=employer&salary=&radius=100&l=", location,
                  "&fromage=any&limit=50&sort=&psf=advsrch")
cat(adv_url)


# Scrape the Search Result -----------------------------------------------------
# get the html file from search url
start_page <- read_html(adv_url)

# get the total job count
job_count <- start_page %>%
    html_node("#searchCount") %>%
    html_text() %>% 
    str_trim(side = "both")
job_count

job_count_total <- 
    unlist(str_split(job_count, " "))[4]
cat("Total job count: ", job_count_total)


# Get start page job URLs
links <- start_page %>%
    html_nodes("h2 a") %>%
    html_attr("href")

save(links, file = "results/jobs/50firstlinks.RData")

# Get result page links
page_links <- start_page %>%
    html_nodes(xpath = '//div[contains(@class,"pagination")]//a') %>%
    html_attr("href")


if_else(length(page_links) == 0, "Pages available: 0", 
        paste("Pages available: ", length(page_links)))


## Scrape for key words, can be changed
my_keywords <- c("data quality", "data management",
                 "ensure data", "data strategy", 
                 "data related", "data definitions",
                 "computer science", "life sciences",
                 "data governance", "business intelligence",
                 "data curation", "communication skills",
                 "metadata")


# Given running total dataframe and links to scrape skills and compute running total
ScrapeJobLinks <- function(res, job_links) {
    for (i in seq_along(job_links)) {
        job_url <- paste0(base_url, job_links[i])
        
        Sys.sleep(1)
        cat(paste0("Reading job ", i, "\n"))
        
        tryCatch({
            page_content <- getPageContent(job_url, as.character(i))
            bigrams <- getPageBigrams(page_content, as.character(i))
            names(bigrams) <- c("word", "n") # Change bigram for word to plot
            plotWords(bigrams, paste0("bigrams", as.character(i))) 
            mywords <- getPageWords(page_content, as.character(i))
            plotWords(mywords, paste0("words", as.character(i))) 
            text <- page_content
            text <- text$text
            df <- data.frame(skill = my_keywords, 
                             count = ifelse(str_detect(text, my_keywords), 1, 0))
            res$running$count <- res$running$count + df$count
            res$num_jobs <- res$num_jobs + 1
        }, error = function(e) {
            cat("ERROR :", conditionMessage(e), "\n")
        })
    }
    return(res)
}


## Actual Scraping

# Create running total dataframe
running <- data.frame(skill = my_keywords, count = rep(0, length(my_keywords)))


# Since the indeed only display max of 20 pages from search result, we cannot use job_count but need to track by creating a num_jobs
num_jobs <- 0

# Here is our results object that contains the two stats
results <- list("running" = running, "num_jobs" = num_jobs)

if (job_count != 0) {
  cat("Scraping jobs in Start Page\n")
  results <- ScrapeJobLinks(results, links)
}

# if (length(page_links) > 0) {
#     for (p in seq_along(page_links)[-1]) {
#         cat("Moving to Next 50 jobs\n")
# 
#         # Navigate to next page
#         new_page <- read_html(paste0(base_url, page_links[p]))
#         
#         # Get new page job URLs
#         links <- new_page %>%
#         html_nodes("h2 a") %>%
#         html_attr("href")
#         
#         # Scrap job links
#         results <- ScrapeJobLinks(results, links)
#          
#     }
# }



# running total
results$running <- 
    results$running %>% 
    arrange(desc(count))

results$running %>% 
    knitr::kable(caption = "Table 1: Skills per count")


#It's more informative to calculate the percentage of apperances, and visualize it using ggplot.


# # running total count as percentage
# results$running$count <- results$running$count / results$num_jobs
# 
# # Reformat the Job Title and Location to readable form
# jt <- str_replace_all(job_title, '\\+|\\\"', " ")
# loc <- str_replace_all(location, "\\%2C+|\\+", " ")
# 
# # Visualization
# results$running %>% 
#     mutate(skill = fct_reorder(skill, -count)) %>% 
#     ggplot(aes(x = skill, y = count)) + 
#       geom_col(fill = "chocolate2") +
#       labs(x = "Skill", y = "Occurrences (%)", 
#            title = paste0("Skill occurrences(%) for ", jt, " in ", loc)) +
#       scale_colour_Publication() + 
#       theme_Publication() +
#       theme(axis.text.x = element_text(angle = 60, hjust = 1))
    

