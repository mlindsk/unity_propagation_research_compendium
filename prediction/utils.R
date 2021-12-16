library(jti)
library(ess)
library(magrittr)

neq_null <- function(x) !is.null(x)

cv_jt <- function(
                  data,
                  graph,
                  class_var,
                  dat_name,
                  perm,
                  nevidence = ncol(data) - 1L,
                  kfold     = 10L,
                  ncores    = 1L
                  ) {

  folds     <- cut(seq(1, nrow(data)), breaks = kfold, labels = FALSE)
  test_set  <- split(perm_obs, folds)
  class_idx <- match(class_var, colnames(data))

  ne_max <- ncol(data) - 1L
  l      <- 1L
  
  lapply(test_set, function(test_idx) {

    cat(
      glue::glue(
        "{dat_name}: - evidence: {nevidence}/{ne_max} - fold: {l}/{kfold}"
      ),"\n"
    )

    l <<- l + 1L

    train <- data[-test_idx, ]
    test  <- data[test_idx, ]
    N     <- nrow(train)

    cl_unity <- jti::cpt_list(train, graph)
    cl_dense <- jti::cpt_list(train, graph, smooth = TRUE)

    cp_unity_cpt   <- jti::compile(cl_unity, root_node =  class_var, initialize_cpts = FALSE)
    cp_dense       <- jti::compile(cl_dense, root_node =  class_var)
    
    preds <- parallel::mclapply(mc.cores = ncores, X = 1:nrow(test), FUN = function(l) {

      true_class <- test[l, class_var] %>% unlist()

      # Evidence
      ev <- sample(unlist(test[l, -class_idx]), nevidence)
      
      # DENSE
      # ---------
      cp_dense_ev <- try(jti::set_evidence(cp_dense, ev), silent = TRUE)
      if (inherits(cp_dense_ev, "try-error")) {
        # If the training model dosent have all levels from the test data we cant do anything!
        return(NULL)
      }
      
      tictoc::tic(quiet = TRUE)
      p_dense       <- jti::jt(cp_dense_ev, propagate = "collect")
      td            <- tictoc::toc(quiet = TRUE)
      qb_dense      <- jti::query_belief(p_dense, nodes = class_var)[[1]]
      pred_dense    <- qb_dense %>% which.max() %>% names() == true_class
      
      # UNITY_CPT
      # ---------
      cp_unity_cpt_ev <- jti::set_evidence(cp_unity_cpt, ev, initialize_cpts = TRUE)
      tictoc::tic(quiet = TRUE)      
      p_unity_cpt     <- jti::jt(cp_unity_cpt_ev, propagate = "collect")
      tu_cpt          <- tictoc::toc(quiet = TRUE)
      qb_unity_cpt    <- jti::query_belief(p_unity_cpt, nodes = class_var)[[1]]
      pred_unity_cpt  <- qb_unity_cpt %>% which.max() %>% names() == true_class
      
      list(
        cv   = c(1 - c(pred_dense), 1 - c(pred_unity_cpt)), #, 1 - c(pred_unity_pot)),
        inc  = c(jti::has_inconsistencies(p_unity_cpt)) # , jti::has_inconsistencies(p_unity_pot))
      )
    })
    
    preds           <- Filter(neq_null, preds)
    preds_dense     <- sapply(preds, function(a) a$cv[1])
    preds_unity_cpt <- sapply(preds, function(a) a$cv[2])
    
    list(
      cv   = c(mean(preds_dense), mean(preds_unity_cpt)), # , mean(preds_unity_pot)),
      inc_cpt  = sapply(preds, function(a) a$inc[1])
    )
  })
}
