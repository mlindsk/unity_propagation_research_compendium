library("fs")
library("dplyr")
library("glue")
library("readr")
library("stringr")
source("discretize.R")

qs <- function(dat) apply(dat, 1L, function(z) any(z == "?"))

path_raw <- "../raw"
datasets <- fs::dir("../raw")

for (x in datasets) {

  fp       <- fs::path(path_raw, x)
  path_dat <- fs::dir_ls(fp)[stringr::str_detect(fs::dir_ls(fp), "data")] %>% tail(n = 1)
  dat      <- readr::read_csv(path_dat, col_names = FALSE)

  # Parsing errors
  prob_dat <- problems(dat) # Should only occur in mushroom
  if (nrow(prob_dat) > 0L) {
    prob_rows <- prob_dat[, "row", drop = TRUE]
    dat <- dat[-prob_rows, ]
  }

  # Data specific  
  class_var <- ""

  # PARKINSON
  if (x == "parkinson") {
    dat <- dat %>%
      slice(-1) %>%
      select(-X1) %>%
      mutate_all(as.numeric) %>%
      mutate(X18 = as.character(X18)) %>%
      discretize()
    class_var = "X18"
  }
  
  if (x == "credit") {
    rm_idx <- !apply(dat, 1, function(x) "?" %in% x)
    dat <- dat[rm_idx, ]
    dat <- dat %>%
      mutate(
        X2  = as.numeric(X2),
        X3  = as.numeric(X3),
        X8  = as.numeric(X8),
        X11 = as.numeric(X11),
        X14 = as.numeric(X14),
        X15 = as.numeric(X15)
      ) %>%
      mutate_if(is.logical, as.character) %>%
      discretize()
    class_var <- "X16"
  }
  
  if (x == "adult") {
    dat <- dat %>%
      mutate_if(is.double, function(z) as.character(cut(z, 5, labels = FALSE)))
    dat <- dat[!qs(dat), ]
    class_var <- colnames(dat)[ncol(dat)]
  }

  if (x == "chess") {
    # No missing
    dat <- dat %>%
      mutate_if(is.logical, as.character)
    class_var <- colnames(dat)[ncol(dat)]
  }
  
  if (x == "mushroom") {
    dat <- dat %>%
      filter(X12 != "?") %>%
      mutate_if(function(y) !is.character(y), as.character) %>%
      select(-X17) # only one level

    dat <- dat[!qs(dat), ]
    class_var <- colnames(dat)[1]
  }
  
  saveRDS(list(dat = dat, class_var = class_var), glue::glue("../prc/{x}.rds"))
}

# DERMA
library(ess)
saveRDS(list(dat = derma, class_var = "ES"), glue::glue("../prc/derma.rds"))
