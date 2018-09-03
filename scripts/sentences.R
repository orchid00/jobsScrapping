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

write_csv(allsentences, "results/sentences/allsentences/allsentences.csv")
save(allsentences, file = "results/RData/allsentences.RData")

class(allsentences)
dim(allsentences)

# test for duplicated sentences
allsentences %>% 
    distinct(sentence) %>% 
    dim()

mysentences <-
allsentences %>% 
    distinct(sentence, .keep_all = TRUE) %>% 
    mutate_at(vars(sentence), ~str_extract(., "^[^>]+[A-Za-z\\d]")) %>% 
    mutate_at(vars(sentence), ~str_replace_all(., "[\\d]+$", "")) %>% # Rremove numbers at the end
    mutate_at(vars(sentence), ~str_replace_all(., "responsibilities:", "responsibilities: ")) %>%
    mutate_at(vars(sentence), ~str_replace_all(., "qualifications:", "qualifications: ")) %>%
    mutate_at(vars(sentence), ~str_replace_all(., "#", "")) %>% 
    mutate_at(vars(sentence), ~str_squish(.)) %>%  # reduced repeated white spaces in a string 
    count(sentence, doc) %>% 
    arrange(sentence) 

dim(mysentences)
View(mysentences)

write_csv(mysentences, paste0("results/sentences/allsentences/", "top_sentences.csv"))
save(mysentences, file = "results/RData/mysentences.RData")

class(mysentences)
dim(mysentences)

