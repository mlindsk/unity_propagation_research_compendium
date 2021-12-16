nets <- list.files("nets")

neval <- c(
  asia = 1000,
  insurance = 1000,
  hailfinder = 1000,
  win95pts = 1000,
  mildew = 100,
  andes = 100,
  pigs = 100,
  barley = 15,
  diabetes = 15,
  link = 2,
  munin = 2
)

# dfs <- parallel::mclapply(mc.cores = length(nets), nets, function(net) {
dfs <- lapply(nets, function(net) {

  net_name <- stringr::str_replace(net, "\\.rds", "")
  cat(net_name, "\n")

  ne       <- neval[net_name]  
  l        <- readRDS(glue::glue("nets/{net}"))
  cpts     <- jti::bnfit_to_cpts(l)
  cl       <- jti::cpt_list(cpts)
  cp       <- jti::compile(cl, unity_msg = TRUE)
  ncliques <- length(cp$charge$C)
  nunity   <- sum(sapply(cp$charge$C, function(x) inherits(x, "sparta_unity")))

  tictoc::tic(quiet = TRUE)
  cp1 <- jti::compile(cl, unity_msg = FALSE)
  for (k in 1:ne) jti::jt(cp1, propagate = "collect", unity_msg = FALSE)
  tf <- tictoc::toc(quiet = TRUE)
  
  tictoc::tic(quiet = TRUE)
  cp2 <- jti::compile(cl, unity_msg = TRUE)
  for (k in 1:ne) jti::jt(cp2, propagate = "collect")
  tt <- tictoc::toc(quiet = TRUE)
  
  data.frame(
    name     = net_name,
    rel_time = (tt$toc - tt$tic) / (tf$toc - tf$tic),
    nc       = ncliques,
    nu       = nunity
  )

})

res_df <- do.call(rbind, dfs)
res_df

saveRDS(res_df, "results/results.rds")
