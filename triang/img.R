library(ggplot2)
library(dplyr)
library(ggrepel)

library(showtext)
font_add("Courier", "COURIER.TTF")
showtext_auto()

res <- readRDS(glue::glue("results/results.rds"))

dat <- res %>%
  mutate(rel_unity = nu / nc) %>%
  slice(which(res$nu != 0)) %>%
  dplyr::arrange(rel_time)
  
p <- ggplot(dat, aes(x = rel_unity, y = rel_time, label = name)) +
  geom_point() + 
  geom_text_repel(family = "Courier", box.padding = 0.5) +
  geom_abline(slope = 0, intercept = 1) +
  # geom_smooth(method = "lm", se = FALSE) +
  xlab("Ratio of unity cliques to non-unity cliques") +
  ylab("Ratio of computation time") +
  theme_bw()
# p

ggplot2::ggsave("../img/bench_triang.pdf", p, , width = 5, height = 3)
