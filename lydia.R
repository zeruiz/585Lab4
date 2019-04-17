library(jsonlite)
library(tidyverse)

url <- "https://data.iowa.gov/resource/m3tr-qhgy.json"
url2 <- "https://data.iowa.gov/resource/m3tr-qhgy.json?county=STORY"
story <- tibble::as_tibble(fromJSON(url2))
story

# clean data

#read in data
dat <- readr::read_csv("data/Iowa_Liquor_Sales-Story.csv")
