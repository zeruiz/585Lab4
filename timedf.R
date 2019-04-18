part <- jsonlite::read_json("https://data.iowa.gov/resource/m3tr-qhgy.json")

data <- read.csv("~/Desktop/IowaLiquorSalesStory.csv")

library(tidyverse)
library(stringr)
# Create a subset of interested variables.
subdat <- data %>% 
  select(Date, 
         Store.Name, 
         Store.Location, 
         Category.Name, 
         Sale..Dollars.,
         Volume.Sold..Liters.)
# Extract the time information from MM/DD/YYYY format using regular expressions.
time <- subdat$Date %>% 
  str_extract_all("[0-9]{1,}") %>% 
  purrr::map(.f=function(x) as.numeric(x))
# Extract the information from list and create a data frame.
mdydf <- data.frame(
  Month = time %>% purrr::map(.f=function(x) x[1]) %>% unlist(),
  Day = time %>% purrr::map(.f=function(x) x[2])%>% unlist(),
  Year = time %>% purrr::map(.f=function(x) x[3])%>% unlist()
)
