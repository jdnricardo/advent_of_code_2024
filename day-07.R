library(here)
library(tidytable)
library(collapse)
library(purrr)
library(stringr)
library(stringi)
library(RcppAlgos) # for permutations

get_day7_input <- function() {
  input <- here("data", "input-day07-a.txt") %>% 
    readLines()
}

eqs_blank <- get_day7_input()

targets <- str_extract(eqs_blank, "^\\d+") %>% 
  as.numeric()

eqs_pieces <- str_remove(eqs_blank, "^\\d+:\\s")

ops_combns <- eqs_pieces %>% 
  map(\(x) str_extract_all(x, "\\s", simplify = TRUE)) %>% 
  map_int(length) %>% 
  map(\(x) permuteGeneral(c("+","*"), x,
                          repetition = TRUE,
                          Parallel=TRUE,
                          nThreads=4) %>% 
        as_tidytable())

eqs_terms <- str_split(eqs_pieces, pattern = "\\s")

trial_list <- list(eqs_terms[1],
                   ops_combns[1],
                   targets[1])

## Numbers applied to a matrix of possible operators
all_attempts <- pmap(trial_list,
                     \(x, y, z) {
                       
                       # Turns matrix into list we map through
                       ops_as_list <- y %>% array_branch(1)
                       
                       expr_by_row <- map(ops_as_list,
                                          \(ops_row) c(sapply(seq_along(x),
                                                              function(i) c(x[i], ops_row[i]))) %>% 
                                            # Drops any NAs after interleaving numbers and operators
                                            keep(\(op) !is.na(op)) %>% 
                                            str_flatten() %>% 
                                            str_replace("(\\d+[\\+\\*]\\d+)", "(\\1)"))
                       
                       while (any(map_lgl(expr_by_row, \(x) any(grepl("\\d+[\\*\\*]\\d+$", x))))) {
                         expr_by_row <- map(expr_by_row,
                                            \(x) str_replace(x, "(\\(\\d+[\\+\\*]\\d+\\)[\\*\\+]\\d+)", "(\\1)"))
                       }
                       
                       match_target <- map_lgl(expr_by_row,
                                               \(x) rlang::parse_expr(x) %>% 
                                                 eval() %>% 
                                                 { . == z})
                       
                       calibration_result <- if_else(any(match_target), z, 0)

                       calibration_result
                     }) %>% 
  unlist()

fsum(all_attempts)
