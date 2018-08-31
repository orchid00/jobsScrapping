library(XML)
library(RCurl)
library(tidytext)
library(janeaustenr)
library(tidyverse)

url_link <- "https://www.indeed.com/rc/clk?jk=788e7e311656fc54&fccid=09fad757f3449fa5&vjs=3"
blog <- getURL(url_link)
blog
blog %>% 
    str_split(pattern = "[\]")

new_URL <- "https://us.conv.indeed.com/rc/clk?jk=788e7e311656fc54&ctk=1cm5m4d0a0m6k49v&t=cr&rctype=oth&orgclktk=1cm5m4d0o0m6k1g6&vjs=3&wwwho=4m_xAU4HGQbAlJdZTkhlA8hEG_rFObn1"
blog <- getURL(new_URL)
blog

tmp <- htmlParse(getURI("https://jobs.tdameritrade.com/job/-/-/1121/8389790"))
content <- xpathSApply(tmp, '//div[@class="descrip-socialshare"]', xmlValue)

nont <- c("\n", "\t", "\r")
pages <- gsub(paste(nont, collapse = "|"), " ", content)


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

content <- 
pages %>% 
    map_df(~(.) %>% as_tibble) 
content
colnames(content) <-  "text"

content

content %>%
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    group_by(word) %>%
    count(sort = TRUE) %>%
    ungroup() %>%
    top_n(10) %>%
    ggplot(aes(x = fct_reorder(word, n), y = n)) +
    geom_bar(stat = "identity", width = 0.5) + 
    xlab(NULL) +
    coord_flip() +
    ylab("Word Frequency") +
    ggtitle("Most Common Corpus Words") +
    theme(legend.position = "none")
