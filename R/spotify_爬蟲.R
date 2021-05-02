library(rvest)
library(tidyverse)
library(magrittr)
library(scales)
library(knitr)
library(lubridate)
library(tibble)


url <- "https://spotifycharts.com/regional/ca/daily/"
timevalues <- seq(as.Date("2018/01/01"), as.Date("2018/12/31"), by = "day")

unitedata<- function(x){
  full_url <- paste0(url, x)
  full_url
}

finalurl <- unitedata(timevalues)


SpotifyScrape <- function(x){
  page <- x
  rank <- page %>% read_html() %>% html_nodes('.chart-table-position') %>% html_text() %>% as.data.frame()
  track <- page %>% read_html() %>% html_nodes('strong') %>% html_text() %>% as.data.frame()
  artist <- page %>% read_html() %>% html_nodes('.chart-table-track span') %>% html_text() %>% as.data.frame()
  streams <- page %>% read_html() %>% html_nodes('td.chart-table-streams') %>% html_text() %>% as.data.frame()
  dates <- page %>% read_html() %>% html_nodes('.responsive-select~ .responsive-select+ .responsive-select .responsive-select-value') %>% html_text() %>% as.data.frame()
  
  #combine, name, and make it a tibble
  chart <- cbind(rank, track, artist, streams, dates)
  names(chart) <- c("Rank", "Track", "Artist", "Streams", "Date")
  chart <- as.tibble(chart)
  return(chart)
}


spotify <- map_df(finalurl, SpotifyScrape)

spotify %<>% 
  mutate(Artist = gsub("by ", "", Artist), 
         Streams = gsub(",", "", Streams), 
         Streams = as.numeric(Streams), 
         Date = as.Date(spotify$Date, "%m/%d/%Y"))


write.csv(spotify,"spotify.csv")
