
# load libraries ----------------------------------------------------------

library(rvest)
library(tidyverse)
library(tidytext)

# get page content as tbl ------------------------------------------------------

getPageContent <- function(url){
    # "https://www.indeed.com/rc/clk?jk=788e7e311656fc54&fccid=09fad757f3449fa5&vjs=3"
    job_url <- read_html(url)

    page_content <-
    job_url %>% 
        html_nodes(css = ".jobsearch-JobComponent") %>% 
        html_text() %>% 
        str_replace_all("[\r\n]" , "") %>%  # clean end of line
        tibble(doc = 1, text = .) # convert character to a tibble
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
    #return(words_counts)
}


# plot top words ---------------------------------------------------------------
plotWords <- function(words_counts, nam){
    words_counts %>%
        top_n(n = 10) %>%
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
# page_content <- getPageContent(url)
# page_content
# getPageSentences(page_content, "1")
# getPageWords(page_content, "1")
# plotWords(mywords, "1")





# using xpath
#job_url %>% 
#    html_nodes(xpath = '/html/body/div[1]/div[3]/div[3]/div/div/div[1]/div[1]') %>% 
#    html_text()


# job_url <- "https://www.indeed.com/rc/clk?jk=788e7e311656fc54&fccid=09fad757f3449fa5&vjs=3"
# 
# tryCatch({
#     html <- read_html(job_url)
#     text <- html_text(html)
#     text <- clean.text(text)
#     df <- data.frame(skill = KEYWORDS, count = ifelse(str_detect(text, KEYWORDS), 1, 0))
#     res$running$count <- res$running$count + df$count
#     res$num_jobs <- res$num_jobs + 1
# }, error = function(e) {
#     cat("ERROR :", conditionMessage(e), "\n")
# })





# test1 -------------------------------------------------------------------


# require(tm)
# # convert list into corpus 
# mycorpus <- Corpus(VectorSource(pages))
# # prepare to remove stopwords, ie. common words like 'the'
# skipWords <- function(x) removeWords(x, stopwords("english"))
# # prepare to remove other bits we usually don't care about
# funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
# # do it
# a <- tm_map(mycorpus, FUN = tm_reduce, tmFuns = funcs)
# # make document term matrix
# mydtm <- DocumentTermMatrix(a, control = list(wordLengths = c(3,10)))
# 
# 
# inspect(mydtm)
# # you can assign it to a data frame for more convenient viewing
# my_df <- inspect(mydtm)
# my_df
# 
# sort(apply(mydtm, 2, sum))
# 
# 

# content <- 
# pages %>% 
#     map_df(~(.) %>% as_tibble) 
# content
# colnames(content) <-  "text"
# 
# content
# 
# content %>%
#     unnest_tokens(word, text) %>% 
#     anti_join(stop_words) %>% 
#     group_by(word) %>%
#     count(sort = TRUE) %>%
#     ungroup() %>%
#     top_n(10) %>%
#     ggplot(aes(x = fct_reorder(word, n), y = n)) +
#     geom_bar(stat = "identity", width = 0.5) + 
#     xlab(NULL) +
#     coord_flip() +
#     ylab("Word Frequency") +
#     ggtitle("Most Common Corpus Words") +
#     theme(legend.position = "none")
