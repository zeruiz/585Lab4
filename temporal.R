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

# Dataframe for temporal plot.
tdf <- subdat %>% 
  select(-Date, -Store.Location) %>% 
  cbind(., mdydf)

# Function for plotting Days within a month/ Months within a year vs. Sale..Dollars
income_mmddyyyy <- function(year, month=NULL){
  if(!is.null(month)){
    dat <- tdf %>% filter(Year == year, Month == month)
    n <- lubridate::days_in_month(month)
    income <- data.frame("Day"=c(1:n), "DayIncome"=rep(0, n))
    value <- dat %>% select(Day, Sale..Dollars.) %>% 
      group_by(Day) %>% summarise(DayIncome = sum(Sale..Dollars.))
    income$DayIncome[value$Day] <- value$DayIncome
    plot <- ggplot2::ggplot(income, aes(x=Day, y=DayIncome/10e5))+
      geom_point()+geom_line()+xlab("Day")+ylab("Income/ $million")+
      scale_x_discrete(limit = as.character(income$Day))+theme_bw()
  }
  else{
    dat <- tdf %>% filter(Year == year)
    income <- data.frame("Month"=c(1:12), "DayIncome"=rep(0, 12))
    value <- dat %>% select(Month, Sale..Dollars.) %>% 
    group_by(Month) %>% summarise(MonIncome = sum(Sale..Dollars.))
    income$MonIncome[value$Month] <- value$MonIncome
    plot <- ggplot2::ggplot(income,aes(x=Month, y=MonIncome/10e5))+
      geom_point()+geom_line()+xlab("Month")+ylab("Income/ $million")+
      scale_x_discrete(limit = as.character(income$Month))+theme_bw()
  }
  plot
}

