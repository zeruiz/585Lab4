## Creating two new columns to show latitude and longitude

library(tidyverse)
library(dplyr)

data <- read.csv("C:/Users/ykh/Downloads/story-sales/Iowa_Liquor_Sales-Story.csv")
head(data)

data_f = data %>% 
  mutate(xy= str_match_all(data$Store.Location, "(?<=\\().+?(?=\\))"),
         lat = lapply(xy, function(x) sapply(strsplit(as.character(x), ','),'[',1)),
         long = lapply(xy, function(x) sapply(strsplit(as.character(x), ','),'[',2)))

head(data_f)
