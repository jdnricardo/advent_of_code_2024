library(here)
library(tidytable)
library(collapse)
library(bench)

get_day1_input <- function() {
  here("data", "input-day01.txt") %>%
# Read in as single column
  read.table(header = FALSE,
             col.names = c("list_1",
                           "list_2"))
}

# Part 1

# Find where I downloaded file
tidytable_day1 <- function() { 
  get_day1_input() %>%
  # Sort
  map(sort) %>% 
  # Recreate data.frame structure
  bind_cols() %>% 
  # Compute distance
  summarise(
    distance = sum(abs(list_1 - list_2))
  ) %>%
  as.integer()
}

collapse_day1 <- function() {
  get_day1_input() %>%
  fmutate(
    list_1 = sort(list_1),
    list_2 = sort(list_2)
  ) %>%
  fmutate(
    distance = abs(list_1 - list_2), .keep = "none"
  ) %>%
  fsum() %>%
  as.integer()
}

  bench::mark(
    tidytable_day1(),
    collapse_day1()
  )

# Part 2

tidytable_day1_p2 <- function() {
  get_day1_input() %>%
  filter(list_2 %in% list_1) %>%
  count(list_2) %>%
  summarise(
    sim_score = sum(n*list_2),
  ) %>%
  as.integer()
}

collapse_day1_p2 <- function() {
  get_day1_input() %>%
  fsubset(list_2 %in% list_1) %>%
  fcount(list_2) %>%
  fmutate(
    sim_score = N*list_2, .keep = "none"
  ) %>%
  fsum() %>%
  as.integer()
}

bench::mark(
    tidytable_day1_p2(),
    collapse_day1_p2()
  )
