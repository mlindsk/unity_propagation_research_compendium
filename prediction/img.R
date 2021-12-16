library("dplyr")
library("tidyr")
library("ggplot2")

res <- readRDS("results/results_prediction.rds") %>%
  as_tibble()

res_tidy_1 <- tidyr::pivot_longer(
  res,
  c(dm, ucm), #, upm),
  names_to = "type",
  values_to = "cv"
)

res_tidy_1 <- res_tidy_1 %>%
  filter(type %in% c("dm", "ucm")) %>%
  mutate(type = case_when(
    type == "dm" ~ "Laplace Smoothing",
    type == "ucm" ~ "Unity Smoothing"
  ))

p1 <- res_tidy_1 %>%
  ggplot(aes(x = ne, y = cv, group = type)) +
  # geom_point(size = 0.3) +
  # geom_pointrange(aes(ymin = ucl, ymax = ucu, colour = type)) +
  # facet_grid(~ name) +
  facet_wrap(~ name, ncol = 3) + 
  geom_line(aes(linetype = type), size = .5) +
  theme(legend.position = "top") +
  xlab("Number of variables with evidence," ~italic("q")) +
  ylab("10-fold crossvalidation score") +
  theme_bw() +
  theme(legend.position = "top") +
  theme(legend.title=element_blank())

# p1

ggplot2::ggsave("../img/bench_prediction.pdf", p1, width = 6, height = 4)
