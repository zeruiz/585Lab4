library(jsonlite)
library(tidyverse)
library(lubridate)


#redaing data from API

url <- "https://data.iowa.gov/resource/m3tr-qhgy.json?county=STORY"
story <- data.frame(fromJSON(url))

#reading data from csv
data <- readr::read_csv('data/Iowa_Liquor_Sales-Story.csv')
names(data) <- stringr::str_replace_all(names(data),"\\s","") 
names(data) <- stringr::str_replace_all(names(data),"[\\(\\)]","")
names(data)
head(data$StoreLocation)

# clean data

sub_data <- data %>%
  select(StoreLocation,Date,VolumeSoldLiters,SaleDollars,CategoryName) %>%
  separate(StoreLocation,c("address","citypluszip","coordinate"),"\\n",extra="merge") %>%
  separate(citypluszip,c("city","zipcode")," (?=[^ ]+$)",extra="merge") %>%
  mutate(coordinate=stringr::str_replace_all(coordinate,"[\\(\\)]","") ,
         city=stringr::str_replace_all(stringr::str_to_lower(city),"\\s",""),
         Date=lubridate::mdy(Date),
         Year=lubridate::year(Date),
         Month=lubridate::month(Date,label=TRUE,abbr=TRUE),
         DayOfWeek=lubridate::wday(Date, label=TRUE),
         CategoryName=stringr::str_to_lower(CategoryName)
         ) %>%
  separate(coordinate,c("lat","lon"),",") %>%
  mutate_at(vars(city, zipcode, CategoryName, Year),factor)%>%
  mutate_at(vars(lat, lon), as.numeric)
unique(sub_data$zipcode)
unique(sub_data$city)

save(sub_data, file = "shiny_zahra/clean_data.rda")
  