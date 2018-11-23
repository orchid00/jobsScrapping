#! /usr/bin/env Rscript

#
#   Description: Setting up the environment before running analysis
#                Run this script first
#   Date: 2018, November 23
#   Paula Andrea Martinez
#   ORCID iD 0000-0002-8990-1985


# To run the analysis some packages need to be previously installed.
# Check installed packages before installing new packages.
list.of.packages <- c("knitr", "tidyverse", "DBI", "ggmap", "likert",
                      "mapproj", "RColorBrewer", "forcats", "extrafont",
                      "scales", "wordcloud", "tm", "SnowballC", "grid",
                      "ggthemes", "here", "rvest", "tidytext", "gridExtra")


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://cran.rstudio.com/")
rm(list.of.packages, new.packages)


