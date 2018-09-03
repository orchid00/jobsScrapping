#if (!require("pacman")) install.packages("pacman")
#pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr")
##https://github.com/trinker/sentimentr

library(sentimentr)

# get sequences again
mytext <- get_sentences(mysentences$sentence)

mytext <- sentiment(mytext)

summary(mytext$sentiment)


mytext %>% 
    sentiment_by() %>% 
    highlight()


set.seed(123)
#devtools::install_github('thomasp85/ggraph')

library(widyr)
library(ggraph)

load("results/RData/mysentences.RData")

