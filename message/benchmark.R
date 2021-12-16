library("stringr")
library("tictoc")
library("glue")
library("fs")

.map_dbl <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = numeric(1), ...)
.map_lgl <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = logical(1), ...)

RNGkind("L'Ecuyer-CMRG")
set.seed(777)

graphs <- setdiff(list.files("../graphs/results"), c("placeholder.txt"))

nevals <- c(
  derma     = 50,
  credit    = 50,
  parkinson = 50,
  chess     = 50,
  mushroom  = 50,
  adult     = 50
)

neach <- 200L

for (graph in graphs) {

  x      <- stringr::str_split(graph, "_")[[1]][1]
  dat_   <- readRDS(glue::glue("../data/prc/{x}.rds"))
  dat    <- dat_[[1]]
  
  nev         <- 2L:(ncol(dat) - 1L)
  nobs        <- nrow(dat)
  perm_obs    <- sample(1:nobs, nobs)
  joint_space <- apply(dat, 2L, unique)
  
  cat("\n")
  g <- readRDS(glue::glue("../graphs/results/{graph}"))

  set_of_evidence <- structure(lapply(nev, function(x) {
    which_vars <- sample(1:ncol(dat), x)
    replicate(neach, sapply(joint_space[which_vars], function(var) sample(var, 1)), FALSE)
  }), names = nev)

  cl <- jti::cpt_list(dat, g)
  cp <- jti::compile(cl, initialize_cpts = FALSE)

  out <- lapply(seq_along(set_of_evidence), function(k) {

    evidence <- set_of_evidence[[k]]
    name <- names(set_of_evidence)[k]
    cat(glue::glue("{x}: - evidence: {name}/{ncol(dat)}"),"\n")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # ---------
    # NON-UNITY
    # ---------
    inc1 <- vector(length = neach)
    l1   <- 1L
    tictoc::tic(quiet = TRUE)
    for (e in evidence) {
      cpe1 <- jti::set_evidence(cp, e, initialize = TRUE, unity_msg = FALSE)
      inc1[l1] <- jti::has_inconsistencies(cpe1)
      l1 <- l1 + 1L
      for (z in 1:nevals[x]) jti::jt(cpe1, propagate = "full", unity_msg = FALSE)
    }
    t1 <- tictoc::toc(quiet = TRUE)

    # ---------
    # UNITY
    # ---------
    inc2 <- vector(length = neach)
    l2   <- 1L
    tictoc::tic(quiet = TRUE)
    for (e in evidence) {
      cpe2 <- jti::set_evidence(cp, e, initialize = TRUE, unity_msg = TRUE)
      inc2[l2] <- jti::has_inconsistencies(cpe2)
      l2 <- l2 + 1L
      for (z in 1:nevals[x]) jti::jt(cpe2, propagate = "full", unity_msg = TRUE)
    }
    t2 <- tictoc::toc(quiet = TRUE)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tr <- (t2$toc - t2$tic) / (t1$toc - t1$tic)
    data.frame(name = x, ne = as.integer(name), time_ratio = tr, inc = mean(inc2))
    
  })

  out <- Reduce(rbind, out)
  saveRDS(out, glue::glue("results/{x}.rds"))
}
