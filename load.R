library(tidyverse)
library(jsonlite)
library(janitor)
library(lubridate)

itineraries_raw <- fs::dir_ls("data/source/github--lchski--pm.gc.ca-news-data/pages/", glob = "*.json") %>%
  map_dfr(read_json, .id = "source_file") %>%
  filter(str_detect(path, "^/en/news/itineraries/"))

itineraries <- itineraries_raw %>%
  clean_names %>%
  select(-source_file) %>%
  mutate(page_title = str_remove(page_title, fixed(" | Prime Minister of Canada"))) %>%
  filter(! str_detect(page_title, fixed("deputy", ignore_case = TRUE))) %>% # remove Deputy PM's itineraries, for the period they were posted to this site
  filter(! path %in% c( # remove other items errantly in itineraries
    "/en/news/itineraries/2016/01/28/media-advisory-prime-minister-will-visit-community-la-loche",
    "/en/news/itineraries/2016/02/07/prime-minister-justin-trudeau-make-important-announcement"
  )) %>%
  filter(! path %in% c( # remove itineraries that double up (for ease of parsing)
    "/en/news/itineraries/2017/05/23/itinerary-wednesday-may-24-and-thursday-may-25-2017",
    "/en/news/itineraries/2017/05/25/itinerary-friday-may-26-and-saturday-may-27-2017",
    "/en/news/itineraries/2017/12/02/itinerary-december-3-4-2017"
  )) %>%
  mutate(
    posted_date = str_remove(path, "^/en/news/itineraries/"),
    posted_date = str_sub(posted_date, end = 10),
    posted_date = as_date(posted_date)
  ) %>%
  mutate(
    date = str_remove_all(page_title, regex("for|events?|public|itinerary|Prime Minister’s|[a-z]+day|,", ignore_case = TRUE)), # remove non-month-day-year words
    date = str_squish(date),
    date = case_when( # fix errant extractions, best we know
      path == "/en/news/itineraries/2017/10/10/itinerary-tuesday-october-10" ~ "October 10 2017",
      TRUE ~ date
    ),
    date = mdy(date)
  )
