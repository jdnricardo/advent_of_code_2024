library(here)
library(tidytable)
library(stringr)
library(rlang)
library(collapse)

get_day3_input <- function() {
  here("data", "input-day03.txt") %>%
    readLines()
}

base_mul_str <- "mul\\([0-9]{1,3}\\,[0-9]{1,3}\\)"

catch_muls <- function(x) {
  str_extract_all(
    x, base_mul_str
  )
}

run_muls <- function(x) {
  x %>% 
    catch_muls() %>% 
    unlist() %>% 
    str_remove("^mul") %>% 
    str_remove_all("[\\(|\\)]") %>% 
    str_replace("\\,", "\\*") %>% 
    map(parse_expr) %>% 
    map(eval) %>% 
    flatten_int() %>% 
    fsum()
  
}

# Part 1
get_day3_input() %>% 
  paste0(collapse = "") %>% 
  run_muls()

# Part 2
get_day3_input() %>% 
  paste0(collapse = "") %>% 
  str_remove_all("don\\'t\\(\\).*?do\\(\\)") %>% 
  run_muls()
