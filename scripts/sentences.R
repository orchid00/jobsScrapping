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


# load sentences

load("results/RData/allsentences.RData")

key_words_from_sentences <- 
    mysentences %>% 
    unnest_tokens(word, sentence) %>% 
    anti_join(stop_words) %>% 
    group_by(word) %>%
    count(sort = TRUE) %>%
    ungroup()

dim(key_words_from_sentences)
head(key_words_from_sentences)
View(key_words_from_sentences)


key_groups_from_sentences <- 
    mysentences %>% 
    unnest_tokens(ngram, sentence, token = "ngrams", n = 8) %>% 
    count(ngram, sort = TRUE) %>% 
    arrange(ngram)


dim(key_groups_from_sentences)
head(key_groups_from_sentences)
tail(key_groups_from_sentences)
View(key_groups_from_sentences)


abilities <-
    key_groups_from_sentences %>% 
    filter(str_detect(ngram, "abilit"))

abilities
View(abilities)

performance_ <-
    key_groups_from_sentences %>% 
    filter(str_detect(ngram, "perform"))

performance_
View(performance_)


##
tidy_text <- mysentences %>%
    mutate(section = str_replace_all(sentence, "\\n", "")) %>%
    unnest_tokens(
        output = sentence,
        input = sentence,
        token = "sentences",
        drop = FALSE,
        to_lower = FALSE
    ) %>%
    mutate(
        regex = str_replace_all(sentence, "\\(", "\\\\("),
        regex = str_replace_all(regex, "\\)", "\\\\)"),
        regex = str_replace_all(regex, "\\.", "\\\\.")
    ) %>%
    mutate(
        start = str_locate(sentence, regex)[, 1],
        end = str_locate(sentence, regex)[, 2]
    ) %>%
    select(sentence, start, end) %>%
    arrange(desc(end)) %>% 
    print()