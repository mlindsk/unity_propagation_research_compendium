dt_discretizer <- function(d, method = 1L, verbose = TRUE, ...) {
  # method: 1 (midtpoint), 2 (location_scale)
  # ...: rpart.control parameters. Eg. cp: complexity parameter

  cont_idx <- which(sapply(d, is.numeric))
  counter <- 1L

  # TODO: Pre-screen the continous ones with iForest and use the scores as weights in rpart
  
  # Parallelize!
  obj <- lapply(cont_idx, function(k) {

    if (verbose) cat(paste0(counter, " / ", length(cont_idx)), "\n")
    counter <<- counter + 1L

    cont_var_k <- unlist(d[, k, drop = TRUE])
    dk         <- d[, -k, drop = FALSE]
    
    m          <- rpart::rpart(cont_var_k ~ ., data = dk, control = rpart::rpart.control(...))
    tree       <- m$frame
    leaves_idx <- tree$var == "<leaf>"
    leaves     <- tree[leaves_idx, c("n", "dev", "yval")]
    leaves     <- leaves[order(leaves[ , "yval"]),]
    nbins      <- nrow(leaves)
    y          <- leaves[, "yval", drop = TRUE]
    std_dev    <- sqrt(leaves[, "dev"] / (leaves[ , "n"] - 1))

    cuts    <- vector("numeric", length = nbins+1)
    cuts[1] <- -Inf # min(cont_var_k) - .1

    if (nbins > 1) {
      for (i in 2:nbins) {
        cuts[i] <- if (method == 2) {
          (y[i-1] * std_dev[i] + y[i] * std_dev[i-1]) / (std_dev[i-1] + std_dev[i])
        } else {
          (y[i] - y[i-1]) / 2 + y[i-1]
        }
      }
    }
    
    cuts[length(cuts)] <- Inf # max(cont_var_k) + .1
    list(cuts  = cuts, min_max = range(cont_var_k), tree = m)
  })
  structure(obj, class = c("decision_tree_discretizer", "list"))
}

subset_eq <- function(x, y) (x[1] >= y[1]) && (x[2] <= y[2])

discretize <- function(d, discretizer = dt_discretizer(d, method = 1L, verbose = TRUE, cp = 0.01)) {
  # TODO: check for format of d?
  # TODO: What if new observations has range above/below the current discretizer?
  for (cont_var in names(discretizer)) {
    min_max_cont_vars <- range(d[, cont_var])
    if (!subset_eq(min_max_cont_vars, discretizer[[cont_var]]$min_max)) stop("What to do with this one?")
    d[, cont_var] <- as.character(
      cut(
        d[, cont_var, drop = TRUE],
        discretizer[[cont_var]]$cuts,
        labels = FALSE
      )
    )
  }
  d
}
