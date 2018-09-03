
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

View(allfiles)

# after checking remove the big list to free space
rm(check1)

allfilesg <- 
    allfiles %>% 
      group_by(word) %>% 
      summarise(n = sum(n)) %>% 
      filter(n > 10) %>% 
      arrange(desc(n)) 

View(allfilesg)


