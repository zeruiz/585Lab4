library(jsonlite)
library(tidyverse)
library(lubridate)
library(stringr)

# getting data through an API
url <- "https://data.iowa.gov/resource/m3tr-qhgy.json"
url2 <- "https://data.iowa.gov/resource/m3tr-qhgy.json?county=STORY"
story <- tibble::as_tibble(fromJSON(url2))
story

# cleaning data from a folder

#read in data
dat <- readr::read_csv("data/Iowa_Liquor_Sales-Story.csv")
# making names have no spaces or parantheses
names(dat) <- stringr::str_replace_all(names(dat), "\\s","")
names(dat) <- str_remove_all(names(dat),"[\\)\\(]")

subdat <- dat %>%
  #subsetting columns of interest
  select(StoreLocation, Date, City, StoreName, VolumeSoldLiters, SaleDollars,
         CategoryName)%>%
  separate(StoreLocation, c("Address", "Coords"),"\\(", extra = "merge")%>%
  separate(Coords, c("Lat", "Lon"), ",")%>%
  mutate(Lon = str_replace(Lon, "\\)", ""),
         Date = lubridate::mdy(Date),
         CategoryName = str_to_lower(CategoryName),
         City = str_replace(str_to_lower(City), "\\s", ""),
         Address = str_to_lower(Address),
         StoreName = str_replace(StoreName, "/.{1,}", ""),
         Year = lubridate::year(Date),
         Month = lubridate::month(Date, label = TRUE, abbr = TRUE))%>%
        # make columns into factors
  mutate_at(vars(Address, StoreName, CategoryName, City), forcats::as_factor)%>%
  mutate_at(vars(Lat, Lon), as.numeric)

# saving .rda in shiny folder 
save(subdat, file = "shiny/subdat.rda")

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

map('county', 'iowa')
