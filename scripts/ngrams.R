# https://www.tidytextmining.com/ngrams.html

# libraries ---------------------------------------------------------------
library(tidyverse)


ReadMyDataFiles <- function(pattern = pattern, path = "./") {
    map(list.files(path = path, pattern = pattern, full.names = TRUE), 
        read_csv)
}

safely_ReadMyDataFiles <- safely(ReadMyDataFiles)

check1 <- safely_ReadMyDataFiles(pattern = ".csv", path = "results/bigrams/")

allbigrams <- 
    check1$result %>%
    map_df(~(.))

#View(allbigrams)
class(allbigrams)
dim(allbigrams)


rm(check1)

write_csv(allbigrams, "results/bigrams/allbigrams/allbigrams.csv")
save(allbigrams, file = "results/RData/allbigrams.RData")


# test for duplicated bigrams
allbigrams %>% 
    distinct(bigram) %>% 
    dim()

mybigrams <-
    allbigrams %>% 
    filter(!str_detect(bigram, pattern = "[\\d]")) %>%  # remove if any words are numbers
    mutate(oc = 1) %>% # uniform bigrams counts 1 per file
    arrange(bigram) %>% 
    group_by(bigram) %>% 
    summarise(n = sum(oc)) %>% 
    ungroup() %>% 
    arrange(desc(n))

dim(mybigrams)
head(mybigrams, n = 30)

mybigrams %>% print(n = 20)


# running total count as percentage
mybigrams$n <- mybigrams$n / 50

# Reformat the Job Title and Location to readable form
jt <- str_replace_all(job_title, '\\+|\\\"', " ")
loc <- str_replace_all(location, "\\%2C+|\\+", " ")

jt <- "Data steward"
loc <- "United States"
source("scripts/00_theme.R")

# Visualisation
mybigrams %>% head(n = 40) %>% 
    mutate(skill = fct_reorder(bigram, n)) %>% # -n
    ggplot(aes(x = skill, y = n)) + 
    geom_col(fill = "chocolate2") +
    labs(x = "Skill", y = "Occurrences (%)", 
         title = paste0("Skill occurrences(%) for ", jt, " in ", loc)) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
    scale_colour_Publication() + 
    theme_Publication() +
    theme(axis.text.x = element_text()) # angle = 90,hjust = 1, vjust = 0.5


# # Visualisation 2
mybigrams %>% head(n = 20) %>% 
    mutate(skill = fct_reorder(bigram, n),
           n = n * 100) %>% # -n
    ggplot(aes(x = skill, y = n)) + 
    geom_segment( aes(x = skill, xend = skill, y = 0, yend = n),
                  color = "skyblue") +
    geom_point( size = 4, colour = "chocolate2") +
    geom_text(aes(label = paste0(n, "%")), size = 3, vjust = 0, nudge_y = 2, nudge_x = -0.1) +
    theme_light() +
    coord_flip() +
    theme( panel.grid.major.y = element_blank(),
           panel.grid.minor  = element_blank(),
           panel.border = element_blank()
    ) +
    labs(x = "Skill", y = "Occurrences (%)", 
         title = paste0("Skill occurrences(%) for ", jt, " in ", loc))
