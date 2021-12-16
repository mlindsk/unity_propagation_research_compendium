files <- setdiff(list.files("results"), c("results_prediction.rds", "placeholder.txt"))

res_df <- data.frame(
  name   = character(0),
  dm     = numeric(0),
  dl     = numeric(0),
  du     = numeric(0),
  ucm     = numeric(0),
  ucl     = numeric(0),
  ucu     = numeric(0),
  inc_cpt = numeric(0),
  ne      = numeric(0)
)

for (file in files) {

  file_split <- unlist(stringr::str_split(file, "_"))
  name_ <- file_split[1]
  ne    <- as.integer(stringr::str_replace(file_split[2], "\\.rds", ""))
  cv    <- readRDS(glue::glue("results/{file}"))

  cv_dense     <- sapply(cv, function(x) x$cv[1])
  cv_unity_cpt <- sapply(cv, function(x) x$cv[2])
  inc_cpt      <- sapply(cv, function(x) mean(x$inc_cpt))
  
  res_df <- rbind(res_df,
    data.frame(
      name = name_,

      dm   = mean(cv_dense),
      dl   = range(cv_dense)[1],
      du   = range(cv_dense)[2],

      ucm  = mean(cv_unity_cpt),
      ucl  = range(cv_unity_cpt)[1],
      ucu  = range(cv_unity_cpt)[2],

      inc_cpt = mean(inc_cpt),
      ne     = ne
    )
  )
}

saveRDS(res_df, "results/results_prediction.rds")
