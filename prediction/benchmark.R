library("glue")
library("stringr")
source("utils.R")

RNGkind("L'Ecuyer-CMRG")
set.seed(300718)

graphs <- setdiff(list.files("../graphs/results"), c("placeholder.txt"))

for (graph in graphs) {
  x      <- stringr::str_split(graph, "_")[[1]][1]
  dat_   <- readRDS(glue::glue("../data/prc/{x}.rds"))
  dat    <- dat_[[1]]
  cls    <- dat_[[2]]
  
  ne_max <- ncol(dat)-1L
  nobs   <- nrow(dat)
  perm_obs <- sample(1:nobs, nobs)
  
  cat("\n")

  g <- readRDS(glue::glue("../graphs/results/{graph}"))

  for (ne in 3:ne_max) {
    cv <- cv_jt(
      data       = dat,
      graph      = g,
      class_var  = cls,
      dat_name   = x,
      perm       = perm_obs,
      nevidence  = ne,
      kfold      = 10,
      ncores     = 40
    )
    
    saveRDS(cv, glue::glue("results/{x}_{ne}.rds"))
  }
}
