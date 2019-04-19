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

### playing around with spatial data

load(file = "shiny/subdat.rda")

# Many of the coordinations of the addresses are similar but not exactly, blerg
address <- subdat %>%
  group_by(Lat, Lon, StoreName) %>%
  distinct(Address)%>%
  arrange(StoreName)


# getting map data 
library(maps)
story <- map_data("county") %>%
  filter(region == "iowa", subregion == "story")
story %>%
  ggplot(aes(lat, long, group = group))+
  geom_polygon(color = "black", fill = "white")+
  geom_point(data = address,aes(Lat , Lon, group = StoreName))