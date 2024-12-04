library(here)
library(tidytable)
library(collapse)
library(purrr)
library(stringr)
library(stringi)

# Part 1

# Rows
horiz <- here("data", "input-day04.txt") %>%
  read.delim(header = FALSE,
             col.names = "horiz_lines")

letter_grid <- horiz %>% 
  separate(horiz_lines, paste0("letter_", 1:140), sep = "") %>% 
  t() %>% 
  as_tidytable() %>% 
  setNames(paste0("row_", 1:140))

# For the diagonals going bottom left to top right in orig orientation
grid_rev <- mutate(horiz, horiz_lines = stri_reverse(horiz_lines)) %>% 
  separate(horiz_lines, paste0("letter_", 1:140), sep = "") %>% 
  t() %>% 
  as_tidytable() %>% 
  setNames(paste0("row_", 1:140))

# Cols
vert <- letter_grid %>% 
  # "Rows" in the original orientation
  unite(vert_lines, matches("row_\\d+"), sep = "")

# Diags
# https://stackoverflow.com/questions/48704928/diagonals-to-rows-in-data-frame#48705418
mat <- as.matrix(letter_grid)
diag_split <- lapply(split(mat, row(mat) - col(mat)), ts)

diag_up <- replace(letter_grid, 1:140,
                   t(do.call("cbind", diag_split[as.numeric(names(diag_split)) >= 0]))) %>% 
  # "Rows" in the original orientation
  unite(diag_lines, matches("row_\\d+"), sep = "", na.rm = TRUE)

diag_lo <- replace(letter_grid, 1:140,
                   t(do.call("cbind", diag_split[as.numeric(names(diag_split)) <= 0]))) %>% 
  # "Rows" in the original orientation
  unite(diag_lines, matches("row_\\d+"), sep = "", na.rm = TRUE)

mat_rev <- as.matrix(grid_rev)
diag_split_rev <- lapply(split(mat_rev, row(mat_rev) - col(mat_rev)), ts)

diag_rev_up <- replace(grid_rev, 1:140,
                       t(do.call("cbind", diag_split_rev[as.numeric(names(diag_split_rev)) >= 0]))) %>% 
  # "Rows" in the original orientation
  unite(diag_lines, matches("row_\\d+"), sep = "", na.rm = TRUE)

diag_rev_lo <- replace(grid_rev, 1:140,
                       t(do.call("cbind", diag_split_rev[as.numeric(names(diag_split_rev)) <= 0]))) %>% 
  # "Rows" in the original orientation
  unite(diag_lines, matches("row_\\d+"), sep = "", na.rm = TRUE)

# Forward
horiz_xmas <- map(horiz, \(x) stri_count_fixed(x, "XMAS"))
vert_xmas <- map(vert, \(x) stri_count_fixed(x, "XMAS"))
diag_up_xmas <- map(diag_up, \(x) stri_count_fixed(x, "XMAS"))
diag_lo_xmas <- map(diag_lo, \(x) stri_count_fixed(x, "XMAS"))
diag_rev_up <- map(diag_rev_up, \(x) stri_count_fixed(x, "XMAS"))
diag_rev_lo <- map(diag_rev_lo, \(x) stri_count_fixed(x, "XMAS"))

# Reverse
horiz_xmas_rev <- map(horiz, \(x) stri_reverse(x) %>% stri_count_fixed("XMAS"))
vert_xmas_rev <- map(vert, \(x) stri_reverse(x) %>% stri_count_fixed("XMAS"))
diag_up_xmas_rev <- map(diag_up, \(x) stri_reverse(x) %>% stri_count_fixed("XMAS"))
diag_lo_xmas_rev <- map(diag_lo, \(x) stri_reverse(x) %>% stri_count_fixed("XMAS"))
diag_rev_up_xmas_rev <- map(diag_rev_up, \(x) stri_reverse(x) %>% stri_count_fixed("XMAS"))
diag_rev_lo_xmas_rev <- map(diag_rev_lo, \(x) stri_reverse(x) %>% stri_count_fixed("XMAS"))

##### WRONG
list(horiz_xmas,
     vert_xmas,
     diag_up_xmas,
     diag_lo_xmas,
     diag_rev_up,
     diag_rev_lo,
     horiz_xmas_rev,
     vert_xmas_rev,
     diag_up_xmas_rev,
     diag_lo_xmas_rev,
     diag_rev_up_xmas_rev,
     diag_rev_lo_xmas_rev) %>% 
  unlist() %>% 
  fsum()
##### WRONG

# Kara Woo's source
# https://bsky.app/profile/karawoo.com/post/3lciisfrgpc2o
library(here)
library(readr)

dat <- here("data", "input-day04.txt") %>% 
  read_fwf(col_positions = fwf_widths(rep(1, 140)))|>
  as.matrix()

## part 1

## get horizontal, vertical, and diagonal lines in the matrix
h <- apply(dat, 1, paste, collapse = "")
v <- apply(dat, 2, paste, collapse = "")
d1 <- row(dat) - col(dat)
d2 <- row(dat) + col(dat)
diag1 <- split(dat, d1) |> map_chr(paste, collapse = "")
diag2 <- split(dat, d2) |> map_chr(paste, collapse = "")
all_ways <- c(h, v, diag1, diag2)

sum(str_count(all_ways, "XMAS"), str_count(all_ways, "SAMX"))

## part 2

## find the indices of the letter A when it's not in the outer rows & columns
a <- which(dat == "A", arr.ind = TRUE)
a_interior <- a[apply(a, 1, \(x) !any(x %in% c(1, 140))), ]

## get the diagonals crossing each A
get_diagonals <- function(idx, dat) {
  rows <- seq(idx["row"] - 1, idx["row"] + 1)
  cols <- seq(idx["col"] - 1, idx["col"] + 1)
  diags <- list(
    mapply(\(x, y) dat[x, y], rows, cols),
    mapply(\(x, y) dat[x, y], rows, rev(cols))
  )
  map_chr(diags, paste, collapse = "")
}

diags <- apply(a_interior, 1, get_diagonals, dat = dat, simplify = FALSE)

sum(map_lgl(diags, \(x) all(x %in% c("MAS", "SAM"))))
