# libraries ---------------------------------------------------------------
library(tidyverse)


ReadMyDataFiles <- function(pattern = pattern, path = "./") {
    map(list.files(path = path, pattern = pattern, full.names = TRUE), 
        read_csv)
}

safely_ReadMyDataFiles <- safely(ReadMyDataFiles)

check1 <- safely_ReadMyDataFiles(pattern = ".csv", path = "results/sentences/")

allsentences <- 
    check1$result %>%
    map_df(~(.))

View(allsentences)


rm(check1)

mysentences <-
allsentences %>% 
    arrange(sentence) %>% 
    group_by(sentence) %>% 
    count(sort = TRUE) %>%
    ungroup()

View(mysentences)

write_csv(mysentences, paste0("results/", "top_sentences.csv"))

library(wordcloud)
library(tidytext)


mysentences %>%
    select(sentence, repetitions = n) %>% 
    #anti_join(stop_words, by = c("sentence" = "word")) %>% 
    count(sentence) %>% 
    arrange(desc(n)) %>%
    with(wordcloud(sentence, n))
