easypackages::libraries(
  "dplyr",
  "glue"  
) 

datasets <- setdiff(list.files("../data/prc"), c("discsat.rds", "connect.rds", "covertype.rds"))

for (x in datasets) {

  print(x)
  
  dat_ <- readRDS(glue::glue("../data/prc/{x}"))
  dat  <- dat_[[1]]
  cls  <- dat_[[2]]
  y    <- strsplit(x, "\\.")[[1]][1]

  extra_edges <- c(
    parkinson = 50,
    adult     = 15,
    chess     = 0,
    connect   = 0,
    covertype = 100,
    credit    = 15,
    derma     = 80,
    mushroom  = 30
  )

  # ----
  # ESS:
  # ----
  g_ess <- ess::fit_graph(dat, q = 0, sparse_qic = FALSE, thres = 3, trace = FALSE)

  if (extra_edges[y] > 0) {
    for (k in 1:extra_edges[y]) g_ess <- ess::walk(g_ess, dat)
  } 

  saveRDS(g_ess %>% ess::as_igraph(), glue::glue("results/{y}_graph.rds"))

  # plot(g_ess)
  # cl <- jti::cpt_list(dat, g_ess)
  # cp <- jti::compile(cl)
  # plot(cp)

  # -----------
  # BNCLASSIFY:
  # -----------
  # tan_ <- bnclassify::tan_cl(cls, dat %>% mutate_all(as.factor), score = 'aic')
  # g_bnclassify  <- igraph::graph_from_edgelist(tan_$.dag$edges)
  # saveRDS(g_bnclassify, glue::glue("results/{x}_bnclassify.rds"))

  # --------
  # BNLEARN:
  # --------
  # bn_ <- bnlearn::hc(as.data.frame(dat %>% mutate_all(as.factor)))
  # g_bnlearn <- bnlearn::as.igraph(bn_)
  # saveRDS(g_bnlearn, glue::glue("results/{y}_graph.rds"))
}

# g <- readRDS("results/parkinson_bnlearn.rds")
# plot(g, vertex.size = 1, vertex.label = NA)
