nets <- list.files("nets")
net_names <- stringr::str_replace(nets, "\\.rds", "")

net_info <- structure(lapply(nets, function(net) {

  net_name <- stringr::str_replace(net, "\\.rds", "")
  if (net_name == "link") browser()
  cat(net_name, "\n")
  l           <- readRDS(glue::glue("nets/{net}"))
  cpts        <- jti::bnfit_to_cpts(l)
  cl          <- jti::cpt_list(cpts)
  cp          <- jti::compile(cl, initialize_cpts = TRUE, unity_msg = TRUE)
  ncliques    <- length(cp$charge$C)
  max_clique  <- max(sapply(cp$cliques, length))
  unities_idx <- sapply(cp$charge$C, function(x) inherits(x, "sparta_unity"))
  nunity      <- sum(unities_idx)
  max_unity   <- max(sapply(cp$cliques[unities_idx], length))
  
  list(nv = length(cpts), nc = ncliques, nu = nunity, mc = max_clique, mu = max_unity)
}), names = net_names)


do.call(rbind, net_info)
