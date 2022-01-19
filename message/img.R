library("ggplot2")
library("dplyr")

library("showtext")
font_add("Courier", "COURIER.TTF")
showtext_auto()

fs  <- paste0("results/", setdiff(list.files("results"), "placeholder.txt"))
res <- Reduce(rbind, lapply(fs, readRDS) )

p <- res %>%
  ggplot(aes(x = ne, y = time_ratio), color = "black") +
  facet_wrap(~ name, ncol = 3) +
  xlab("Number of variables with evidence," ~italic("q")) +
  ylab("Ratio of computational time") +
  geom_line(linetype = "dotted") +
  geom_smooth(method = "loess", colour = "black", size = 0.8, se = FALSE) +
  theme_bw() +
  theme(strip.text.x = element_text(family = "Courier"))

# p

ggplot2::ggsave("../img/bench_message.pdf", p, , width = 6, height = 3.5)
