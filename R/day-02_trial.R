# https://gist.github.com/mathesong/78636de9e1e87b94eadb1b4572e7128c
library(tidyverse)
library(readr)
library(here)

input <- read_delim(here("data", "input-day02.txt"), delim="\t", col_names = "d")

##### Part 1 #####

report_safe <- function(report) {
  
  vals <- as.numeric(str_split(report, " ")[[1]])
  difs <- diff(vals)
  
  if( all(difs < 0) | all(difs > 0) ) {
    absdifs <- abs(difs)
    if( all(absdifs > 0) & all(absdifs < 4) ) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# rowwise causing issues here, so I'll group instead

input <- input %>% 
  mutate(n = 1:n()) %>% 
  mutate(safe = map_lgl(d, report_safe),
         .by = "n")

sum(input$safe)


##### Part 2 #####

values_safe <- function(vals) {
  difs <- diff(vals)
  if( all(difs < 0) | all(difs > 0) ) {
    absdifs <- abs(difs)
    if( all(absdifs > 0) & all(absdifs < 4) ) {
      return(TRUE)
    }
  }
  return(FALSE)
}

report_safe_tol <- function(report) {
  vals <- as.numeric(str_split(report, " ")[[1]])
  
  if(values_safe(vals)) {
    return(TRUE)
  }
  
  for(i in 1:length(vals)) {
    vals_tol <- vals[-i]
    
    if(values_safe(vals_tol)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

input <- input %>% 
  mutate(safe_tol = map_lgl(d, report_safe_tol),
         .by = "n")

sum(input$safe_tol)

