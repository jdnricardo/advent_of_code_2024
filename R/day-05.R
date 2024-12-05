library(here)
library(tidytable)
library(purrr)
library(stringr)

get_day5_input <- function() {
  here("data", "input-day05.txt") %>% 
    readLines()
}

input <- get_day5_input()

order_rules <- keep(input, \(x) grepl("\\|", x))
update_seq <- keep(input, \(x) grepl("\\,", x)) %>% 
  map(strsplit, ",") %>% 
  list_flatten() %>% 
  map(as.numeric)

pgs_before <- function(num, rules = order_rules) {
  
  keep(order_rules, \(x) grepl(paste0(num, "$"), x)) %>% 
    sub(paste0("\\|", num), "", .) %>% 
    map(as.numeric) %>% 
    unlist()
}

pgs_after <- function(num, rules = order_rules) {
  
  keep(order_rules, \(x) grepl(paste0("^", num), x)) %>% 
    sub(paste0(num, "\\|"), "", .) %>% 
    map(as.numeric) %>% 
    unlist()
}

check_order <- function(single_seq) {
  map_lgl(seq_along(single_seq),
          function(x) {
            if(x==length(single_seq)) {
              steps_after <- NULL
              steps_before <- single_seq[-x]
            } else if(x==1) {
              steps_after <- single_seq[-x]
              steps_before <- NULL
            } else {
              steps_after <- single_seq[(x+1):length(single_seq)]
              steps_before <- single_seq[1:(x-1)]
            }
            
            all(steps_after %in% pgs_after(single_seq[x])) &
              all(steps_before %in% pgs_before(single_seq[x]))
          }
  ) %>% 
    all()
}  

# Part 1
correct_order <- map_lgl(update_seq, check_order)

correct_updates <- update_seq[correct_order]

map_dbl(correct_updates, \(x) x[(length(x)%/%2)+1]) %>% 
  sum()

# Part 2
incorrect_updates <- update_seq[!correct_order]

# Recursive function from https://bsky.app/profile/acastroaraujo.bsky.social/post/3lcl7vgythk2q
fix_order <- function(x) {
  curr_seq_rules <- map_chr(combn(x, 2, simplify = FALSE), \(x) paste(x, collapse="|"))
  correct_rules <- curr_seq_rules %in% order_rules
  if(all(correct_rules)) return(x)
  
  wrong_rules <- curr_seq_rules[!correct_rules]
  wrong_rule <- wrong_rules[[1]]
  wrong_num <- str_split_1(wrong_rule, "\\|") %>% as.integer()
  swap_loc <- which(x %in% wrong_num)
  x[swap_loc] <- x[rev(swap_loc)]
  
  fix_order(x)
}

corrected_updates <- map(incorrect_updates, fix_order)

map_dbl(corrected_updates, \(x) x[(length(x)%/%2)+1]) %>% 
  sum()
