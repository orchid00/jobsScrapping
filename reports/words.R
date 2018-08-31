
# load libraries ----------------------------------------------------------

library(rvest)
library(tidyverse)
library(tidytext)

# get page content as tbl ------------------------------------------------------

getPageContent <- function(url, num){
    # "https://www.indeed.com/rc/clk?jk=788e7e311656fc54&fccid=09fad757f3449fa5&vjs=3"
    job_url <- read_html(url)

    page_content <-
    job_url %>% 
        html_nodes(css = ".jobsearch-JobComponent") %>% 
        html_text() %>% 
        str_replace_all("[\r\n]" , "") %>%  # clean end of line
        tibble(doc = num, text = .) # convert character to a tibble
    return(page_content)
}

# sentences --------------------------------------------------------------------
getPageSentences <- function(page_content, nam){
    collected_sentences <- 
        page_content %>% 
        unnest_tokens(sentence, text, token = "sentences")
    write_csv(collected_sentences, paste0("results/sentences/", nam, ".csv"))
    #return(collected_sentences)
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
    words_counts %>%
        top_n(10) %>%
        ggplot(aes(x = fct_reorder(word, n), y = n)) +
        geom_bar(stat = "identity", width = 0.5) + 
        xlab(NULL) +
        coord_flip() +
        ylab("Word Frequency") +
        ggtitle("Most Common Corpus Words") +
        theme_minimal() +
        theme(legend.position = "none", 
              text = element_text(size = 16, family = "serif"))
    ggsave(filename = paste0("results/figures/", nam, ".png"))
}



# url <- "https://www.indeed.com/rc/clk?jk=788e7e311656fc54&fccid=09fad757f3449fa5&vjs=3"
# page_content <- getPageContent(url, 54)
# page_content
# getPageSentences(page_content, "1")
# mywords <- getPageWords(page_content, "1")
# plotWords(mywords, "1")

