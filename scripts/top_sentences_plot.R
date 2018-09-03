library(wordcloud)
library(tidytext)

top_sentences <- read_csv("results/top_sentences.csv")

top_sentences %>%
    select(sentence, repetitions = n) %>% 
    #anti_join(stop_words, by = c("sentence" = "word")) %>% 
    count(sentence) %>% 
    arrange(desc(n)) %>%
    with(wordcloud(sentence, n))
# . could not be fit on page. It will not be plotted.