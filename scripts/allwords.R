
# libraries ---------------------------------------------------------------
library(tidyverse)

 
ReadMyDataFiles <- function(pattern = pattern, path = "./") {
    map(list.files(path = path, pattern = pattern, full.names = TRUE), 
        read_csv)
}

safely_ReadMyDataFiles <- safely(ReadMyDataFiles)

check1 <- safely_ReadMyDataFiles(pattern = ".csv", path = "results/words")

allfiles <- 
    check1$result %>%
      map_df(~(.) %>% 
          filter(!grepl("[[:digit:]]", word)))

head(allfiles)

# after checking remove the big list to free space
rm(check1)

allfilesg <- 
    allfiles %>% 
      arrange(word) %>% 
      mutate(n = 1) %>%  #one occurance per word
      group_by(word) %>% 
      summarise(n = sum(n)) %>%
      ungroup() %>% 
      filter(n > 10) %>% 
      arrange(desc(n)) 

head(allfilesg)


# running total count as percentage
allfilesg$n <- allfilesg$n / 48

# Reformat the Job Title and Location to readable form
jt <- str_replace_all(job_title, '\\+|\\\"', " ")
loc <- str_replace_all(location, "\\%2C+|\\+", " ")

#jt <- "Data steward"
#loc <- "United States"
#source("scripts/00_theme.R")

# # Visualisation 2
allfilesg %>% head(n = 20) %>% 
    mutate(skill = fct_reorder(word, n),  # -n
               n = round(n * 100)) %>% 
    ggplot(aes(x = skill, y = n)) + 
    geom_segment( aes(x = skill, xend = skill, y = 0, yend = n),
                  color = "skyblue") +
    geom_point( size = 4, colour = "chocolate2") +
    geom_text(aes(label = paste0(n, "%")), size = 3, vjust = 0, nudge_y = 4, nudge_x = -0.2) +
    theme_light() +
    coord_flip() +
    theme( panel.grid.major.y = element_blank(),
           panel.grid.minor  = element_blank(),
           panel.border = element_blank()
    ) +
    labs(x = "Skill", y = "Occurrences (%)", 
         title = paste0("Skill occurrences(%) for ", jt, " in ", loc))

ggsave(filename = "figures/top20words.pdf")

