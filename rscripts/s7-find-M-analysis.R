library(tidyverse)
library(mmcfa)
source("rscripts/utils.R")
thesis_fig_path = "~/Documents/GitHub/Thesis---Pedro-Menezes-de-Araujo/figures/Chapter5/"

plot_seq_simple = function(id, time, z, w) {

  cut = w %>% sort() %>% diff() %>% `==`(1) %>% which()

  data.frame(
    id = id,
    time = time,
    z = z
  ) %>%
    mutate(id = factor(id, levels = names(sort(w)))) %>%
    ggplot(aes(x=time, y = id, fill = factor(z))) +
    geom_tile(color = "black") +
    geom_hline(yintercept = cut + 0.5)

}

#### data ####
wpp_data = readRDS("data/wpp_data_list.rds")
y = wpp_data$y
id = wpp_data$id
time = wpp_data$time

#### analysis ####
w_find_fixed = readRDS("models/find-G=6-M=15-z-fixed-fit-2026-04-30 23:25:59.885237.rds")
w_find_random = readRDS("models/find-G=6-M=15-z-random-fit-2026-04-30 23:01:46.788613.rds")

w_find_fixed$clust_size_p %>%
  ggplot(aes(x=n_clust, y = prop)) +
  geom_bar(stat = "identity") +
  labs(x = expression(M["+"]), y="Proportion")

ggsave(paste0(thesis_fig_path, "M-find.pdf"), width = 5, height = 3)

w_find_random$clust_size_p %>%
  ggplot(aes(x=n_clust, y = prop)) +
  geom_bar(stat = "identity")

w_find_random$entropy %>%
  ggplot(aes(x=iter, y=entropy, group=runs)) +
  geom_line()

w_find_fixed$entropy %>%
  ggplot(aes(x=iter, y=entropy, group=runs)) +
  geom_line() +
  labs(x = "Iterations", y= latex2exp::TeX("$\\sum_{i}p(W_i | Z_i)$ entropy"))

ggsave(paste0(thesis_fig_path, "M-find-entropy.pdf"), width = 5, height = 3)

w_find_fixed$entropy %>%
  filter(iter == 50) %>%
  .$entropy %>% max()

w_random = w_find_random$post_modes$`13`$w
w_fixed = w_find_fixed$post_modes$`13`$w
names(w_random) = names(w_random) %>% rename_countries()
names(w_fixed) = names(w_fixed) %>% rename_countries()
id = id %>% rename_countries()

z_fixed = w_find_fixed$model_data$data$z
z_random = w_find_random$post_modes$`13`$z

plot_seq_simple(id = id, time = time, z = z_random, w = w_random)
plot_seq_simple(id = id, time = time, z = z_fixed, w = w_fixed)






