
# load libraries ----------------------------------------------------------
library(rvest)
library(tidyverse)
library(tidytext)
library(gridExtra)

source("scripts/00_theme.R")

# get page content as tbl ------------------------------------------------------

getPageContent <- function(url, num){
    # "https://www.indeed.com/rc/clk?jk=788e7e311656fc54&fccid=09fad757f3449fa5&vjs=3"
    job_html <- read_html(url)

    page_content <-
    job_html %>% 
        html_nodes(css = ".jobsearch-JobComponent-description") %>% 
        html_text(trim = FALSE) %>% 
        str_replace_all(regex("\r\n|\n|\t|\r|,|/|<|>|\\.|\\:"), " ") %>%  # clean text
        tibble(doc = as.numeric(num), text = .) # convert character to a tibble
    
    save(page_content, file = paste0("results/jobs/RData/", num, ".RData" ))
    write_csv(page_content, path = paste0("results/jobs/", num, ".csv"))
    return(page_content)
}

# # sentences --------------------------------------------------------------------
# getPageSentences <- function(page_content, nam){
#     collected_sentences <- 
#         page_content %>% 
#         unnest_tokens(sentence, text, token = "sentences")
#     write_csv(collected_sentences, paste0("results/sentences/", nam, ".csv"))
#     #return(collected_sentences)
# }

# bigrams ----------------------------------------------------------------------
getPageBigrams <- function(page_content, nam){
    collected_bigrams <- 
        page_content %>% 
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%  
        filter(!str_detect(bigram, pattern = "[\\d]")) %>% # remove numbers
        separate(bigram, c("word1", "word2"), sep = " ") %>% 
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>% 
        count(word1, word2, sort = TRUE) %>% 
        unite(bigram, word1, word2, sep = " ")

    
    write_csv(collected_bigrams, paste0("results/bigrams/", nam, ".csv"))
    return(collected_bigrams)
}


# words --------------------------------------------------------------------
getPageWords <- function(page_content, nam){
    words_counts <- 
        page_content %>%
        unnest_tokens(word, text) %>% 
        anti_join(stop_words) %>% 
        group_by(word) %>%
        count(sort = TRUE) %>%
        ungroup()
    
    write_csv(words_counts, paste0("results/words/", nam, ".csv"))
    return(words_counts)
}


# plot top words ---------------------------------------------------------------
plotWords <- function(words_counts, nam){
    p <- 
        words_counts %>%
        head(10) %>%
        ggplot(aes(x = fct_reorder(word, n), y = n)) +
        geom_bar(stat = "identity", width = 0.5) + 
        xlab(NULL) +
        coord_flip() +
        ylab("Word Frequency") +
        ggtitle("Most Common Corpus Words")
    
    p + scale_colour_Publication() + theme_Publication()
    
    ggsave(filename = paste0("results/figures/", nam, ".png"),
        width = 5, height = 6)

}


# url <- "https://www.indeed.com/rc/clk?jk=788e7e311656fc54&fccid=09fad757f3449fa5&vjs=3"
# page_content <- getPageContent(url, 54)
# page_content
# # getPageSentences(page_content, "1")
# mywords <- getPageWords(page_content, "1")
# plotWords(mywords, "1")


