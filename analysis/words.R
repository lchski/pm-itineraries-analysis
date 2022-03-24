source("load.R")

library(tidytext)
library(textclean)

itineraries_cleaned <- itineraries %>%
  mutate(
    text = replace_html(article_html),
    text = replace_non_ascii(text),
    text = replace_white(text)
  ) %>%
  select(-article_html)

# find potential textclean issues
itineraries_cleaned %>%
  slice_sample(prop = 0.05) %>%
  pull(text) %>%
  check_text()

itinerary_words <- itineraries_cleaned %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords())

itinerary_words %>%
  count(word, sort = TRUE)

itinerary_words %>%
  filter(str_detect(word, "fire")) %>% # captures "fire", "wildfire", "firefighter", etc
  mutate(month = month(date, label = TRUE)) %>%
  # distinct() %>% # use if you want to see just how many itineraries mention "fire", versus how many times it's mentioned
  ggplot(aes(x = month)) +
  geom_bar()
