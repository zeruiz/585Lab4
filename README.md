# 585Lab4

Here is the [link](https://github.com/zeruiz/585Lab4) to our github repo

### 1. Getting data using an API

```{r, message=FALSE}
library(jsonlite)
library(tidyverse)
url <- "https://data.iowa.gov/resource/m3tr-qhgy.json?county=STORY"
story_ex <- tibble::as_tibble(fromJSON(url)) # now it's a table ready for cleaning
story_df <- data.frame(fromJSON(url)) # another option : save as dataframe 
```

### 2. Cleaning up the full dataset
Please note - here we are cleaning the large csv, found in a data folder within our working directory (but not included in the repo because of it's size). We have saved an .rda file of the cleaned dataset in our shiny folder to use with the app. 

```{r, message = FALSE, warning = FALSE}
#reading in data
dat <- readr::read_csv("data/Iowa_Liquor_Sales-Story.csv")
# making names have no spaces or parantheses
names(dat) <- stringr::str_replace_all(names(dat), "\\s","")
names(dat) <- str_remove_all(names(dat),"[\\)\\(]")
subdat <- dat %>%
  #subsetting columns of interest
  select(StoreLocation, Date, City, StoreName, VolumeSoldLiters, SaleDollars,
         CategoryName)%>%
  # separating StoreLocation into address and Lat, Lon coordinates
  separate(StoreLocation, c("Address", "Coords"),"\\(", extra = "merge")%>%
  separate(Coords, c("Lat", "Lon"), ",")%>%
  mutate(Lon = str_replace(Lon, "\\)", ""),
         Date = lubridate::mdy(Date),
         CategoryName = str_to_lower(CategoryName),
         City = str_replace(str_to_lower(City), "\\s", ""),
         Address = str_to_lower(Address),
         StoreName = str_replace(StoreName, "/.{1,}", ""),
         Year = lubridate::year(Date),
         DayOfWeek=lubridate::wday(Date, label=TRUE),
         Month = lubridate::month(Date, label = TRUE, abbr = TRUE))%>%
        # make columns into factors
  mutate_at(vars(Address, StoreName, CategoryName, City), forcats::as_factor)%>%
  mutate_at(vars(Lat, Lon), as.numeric)
# saving .rda in shiny folder 
save(subdat, file = "shiny/subdat.rda")
```

### 3. Working with Shiny
Please find our code and app in the shiny folder. 
