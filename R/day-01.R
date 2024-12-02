library(here)
library(tidytable)
library(janitor)

here("data", "input-day01.txt") %>% 
  read.delim(header = FALSE,
             col.names = c("lists")) %>% 
  remove_empty("cols") %>% 
  separate("lists", paste0("list_", 1:2)) %>% 
  map(as.integer) %>%
  map(sort) %>% 
  bind_cols() %>% 
  summarise(
    distance = sum(abs(list_1 - list_2))
  )
