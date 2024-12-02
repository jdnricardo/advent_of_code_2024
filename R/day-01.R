library(here)
library(tidytable)
library(janitor)

# Find where I downloaded file
here("data", "input-day01.txt") %>% 
# Read in as single column
  read.delim(header = FALSE,
             col.names = c("lists")) %>% 
  # Separate into two columns
  separate("lists", paste0("list_", 1:2)) %>% 
  # Convert type
  map(as.integer) %>%
  # Sort
  map(sort) %>% 
  # Recreate data.frame structure
  bind_cols() %>% 
  # Compute distance
  summarise(
    distance = sum(abs(list_1 - list_2))
  )
