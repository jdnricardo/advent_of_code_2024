library(here)
library(tidytable)
library(purrr)
library(collapse)
library(bench)

get_day2_input <- function() {
  here("data", "input-day02.txt") %>%
# Read in as single column
  readLines() %>%
  map(\(x) strsplit(x, " ")) %>%
  map(unlist) %>%
  map(as.numeric)
}

get_day2_df <- function() {
  here("data", "input-day02.txt") %>%
    # Read in as single column
    read.delim(header = FALSE,
               col.names = "d") %>% 
    mutate(n = 1:n())
}

# Checks for monotonic lists
day2_diff <- get_day2_input() %>%
  fdiff() %>% 
  map(na.omit)

# levels are either all increasing or all decreasing
check_one <- function(x) {
  all(x > 0) || all(x < 0)
}

# any two adjacent levels differ by at least one and at most three
check_two <- function(x) {
    abs_x <- abs(x)
    
    all(between(abs_x, 1, 3))
}

is_safe <- function(x) {
  (all(x > 0) || all(x < 0)) && all(between(abs(x), 1, 3))
}

# Need to improve at seeing recursion cases
is_damp_safe <- function(x) {
  is_safe(x) || any(map_lgl(seq_along(x), \(i) is_safe(x[-i])))
}

# Part 1
day2_diff %>%
  map(is_safe) %>%
  unlist() %>%
  fsum()

# Part 2

# pasting in a correct solution for QC
# I keep getting 716 when the answer is 717
# Source: https://gist.github.com/mathesong/78636de9e1e87b94eadb1b4572e7128c
source(here("R", "day-02_trial.R"))

damp_diff <- get_day2_df() %>% 
  mutate(diff = strsplit(d, " ") %>%
           map(as.numeric) %>%
           fdiff() %>% 
           map(na.omit) %>% 
           map(as.numeric),
         damp_safe = map_lgl(diff, is_damp_safe)) %>% 
  left_join(select(input, -safe, -n),
            by = c("d"))

sum(damp_diff$damp_safe)

damp_checks <- damp_diff %>% 
  filter(damp_safe != safe_tol)
  

