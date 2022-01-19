library("jti")
library("sparta")
library("glue")

datasets <- setdiff(list.files("../data/prc"), c("discsat.rds", "connect.rds", "covertype.rds"))

for (d in datasets) {
  print(d)
  dat_   <- readRDS(glue::glue("../data/prc/{d}"))
  dat    <- dat_[[1]]
  name_d <- stringr::str_split(d, "\\.")[[1]][1]
  g      <- readRDS(glue::glue("results/{name_d}_graph.rds"))
  cl     <- jti::cpt_list(dat, g)
  cp     <- jti::compile(cl)

  sp <- sapply(cl, function(x) round(sparta::sparsity(x), 2))
  df <- data.frame(x = density(sp)$x, y = density(sp)$y)

  png(glue::glue("../img/{name_d}_sparsity.png"), width = 600, height = 600, pointsize = 15)
  plot(df$x, df$y, type = "l", bty="n", yaxt="n", xaxt="n", ylab = "", xlab = "", lwd = 12)
  dev.off() 
}

plot(df$x, df$y, type = "l", bty="n", yaxt="n", xaxt="n", ylab = "", xlab = "", lwd = 12)
plot(df$x, df$y, type = "l", bty="n", ylab = "", xlab = "", lwd = 12)

# jt_info <- lapply(datasets, function(d) {

#   print(d)
#   dat_   <- readRDS(glue::glue("../data/prc/{d}"))
#   dat    <- dat_[[1]]
#   name_d <- stringr::str_split(d, "\\.")[[1]][1]
#   g      <- readRDS(glue::glue("results/{name_d}_graph.rds"))
#   cl     <- cpt_list(dat, g)
#   cp     <- compile(cl)
  
#   clique_size      <- sapply(cp$charge$C, function(x) length(names(x)))
#   mean_clique_size <- mean(clique_size)
#   max_clique_size  <- max(clique_size)
#   min_clique_size  <- min(clique_size)
  
#   c(
#     ncliques          = length(clique_size),
#     mean_clique_size  = mean_clique_size,
#     max_clique_size   = max_clique_size,
#     min_clique_size   = min_clique_size
#   )
# })

# dat_names <- unname(sapply(datasets, function(d) stringr::str_split(d, "\\.")[[1]][1]))
# names(jt_info) <- dat_names
