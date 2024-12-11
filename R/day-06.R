library(here)
library(purrr)
library(furrr)
library(stringr)
library(tidytable)

## Works on sample inputs but too slow on full puzzle

get_day6_input <- function() {
  input <- here("data", "input-day06-a.txt") %>% 
    read.delim(header=FALSE,
               col.names = "cols")
  
  separate(input, cols, paste0("col_", 1:nrow(input)), sep = "") %>%
    as_tidytable()
}

map_start <- get_day6_input()
map_working <- map_start

g_regex <- "[<>\\^v]"
g_grepl <- function(x) grepl(g_regex, x)

g_where <- function(dot_map) {
  g_x <- which(g_grepl(dot_map))
  if(!length(g_x)) return(c(0,0))
  
  g_y <- which(g_grepl(dot_map[[g_x]]))
  
  c(g_x, g_y)
}

g_start <- g_where(map_start)
path_hist_start <- list(str_flatten_comma(g_start, map_start[g_start[2]][[g_start[1]]]))

g_oob <- function(pos,
                  xlim = c(1,ncol(map_start)),
                  ylim = c(1,nrow(map_start))) {
  
  !between(pos[2], xlim[1], xlim[2]) | 
    !between(pos[1], ylim[1], ylim[2])
}

g_turn <- function(curr_dir) {
  case_when(curr_dir == "<" ~ "^",
            curr_dir == ">" ~ "v",
            curr_dir == "^" ~ ">",
            curr_dir == "v" ~ "<")
}

run_sim <- function(init_map,
                    loop_check = FALSE) {
  
  map_working <- init_map
  path_hist_working <- path_hist_start
  
  repeat {
    looped <- FALSE
    
    g_pos <- g_where(map_working)
    
    g_sq <-  map_working[g_pos[2]][[g_pos[1]]]
    
    g_dir_x <- case_when(g_sq == "<" ~ -1,
                         g_sq == ">" ~ 1,
                         .default = 0)
    
    g_dir_y <- case_when(g_sq == "^" ~ -1,
                         g_sq == "v" ~ 1,
                         .default = 0)
    
    g_dir <- c(g_dir_x, g_dir_y)
    
    new_pos <- c(g_pos[1]+g_dir[1],g_pos[2]+g_dir[2])
    
    if(g_oob(new_pos)) {
      map_working[g_pos[2]][[g_pos[1]]] <- "X"
      new_pos <- g_pos
      message("Guard left the arena")
      break
    }
    
    next_sq <- map_working[new_pos[2]][[new_pos[1]]]
    
    if(next_sq %in% c(".", "X")) {
      map_working[g_pos[2]][[g_pos[1]]] <- "X"
      map_working[new_pos[2]][[new_pos[1]]] <- g_sq
      
      new_path_hist <- str_flatten_comma(g_pos, g_sq)
      
      path_hist_working <- append(path_hist_working, 
                                  list(new_path_hist))
      
    } else if(next_sq == "#") {
      new_dir <- g_turn(g_sq)
      map_working[g_pos[2]][[g_pos[1]]] <- new_dir
      
      # message(paste(g_sq, "at c(", g_pos[1], ",", g_pos[2], ")"))
      
      if(loop_check &
         length(path_hist_working) > 1 &
         str_flatten_comma(g_pos, new_dir) %in% path_hist_working) {
        message("Formed a loop")
        looped <- TRUE
        break
      }
      
    }
  }
  
  list(map = map_working,
       path_hist = path_hist_working,
       looped = looped)
}

part_1 <- run_sim(map_start)

part_1[["map"]] %>% 
  pivot_longer() %>% 
  filter(value == "X") %>% 
  nrow()

part_1_path <- part_1[["map"]] %>% 
  mutate(row_num = row_number()) %>% 
  pivot_longer(matches("col_"),
               names_to = "col_num",
               names_prefix = "col_") %>% 
  filter(value == "X") %>% 
  mutate(across(matches("num"), as.numeric))

forms_loop <- function(x,y) {
  
  map_start_p2 <- map_start
  map_start_p2[y][[x]] <- "#"
  
  sim_result <- run_sim(map_start_p2, loop_check = TRUE)
  
  data.frame(start_pos = str_flatten_comma(c(x,y)),
             looped = sim_result$looped)
}

safe_loop_check <- safely(forms_loop)

plan(multisession, workers = 6)

part_2 <- future_map2(part_1_path$col_num,
                      part_1_path$row_num,
                      safe_loop_check,
                      .progress = TRUE) %>% 
  map(\(x) x$result) %>% 
  list_rbind()

sum(part_2$looped)