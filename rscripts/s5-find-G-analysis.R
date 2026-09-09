library(tidyverse)
library(mmcfa)
library(patchwork)
source("rscripts/utils.R")

wpp_data = readRDS("data/wpp_data_list.rds")
y = wpp_data$y
id = wpp_data$id
time = wpp_data$time

fit = readRDS("models/find-G-fit-2026-04-30 20:51:20.879365-.rds")

clust_size = lapply(fit$chains, function(chain){
  chain$sample_list$z %>% compute_clust_size()
}) %>% do.call(cbind, .)

clust_size_mode = apply(
  clust_size,
  MARGIN = 2,
  FUN = function(x) x %>% table() %>% which.max() %>% names() %>% as.integer()
)

clust_size_mode
table(as.numeric(clust_size)) %>% prop.table()

ari_matrix = matrix(NA, nrow = 10, ncol = 10)

for(i in 1:10) {
  for(j in i:10) {
    zi = fit$chains[[i]]$sample_list$z %>% comp_class()
    zj = fit$chains[[j]]$sample_list$z %>% comp_class()
    ari_matrix[i, j] = mclust::adjustedRandIndex(zi, zj)
  }
}

ari_matrix[upper.tri(ari_matrix)] %>% mean()

relabel = function(fit, G) {

  z = lapply(1:length(fit$chains), function(i){
    if(clust_size_mode[i] == G) {
      fit$chains[[i]]$sample_list$z[clust_size[, i] == G, ]
    }
  }) %>%
    do.call(rbind, .)

  z = z %>% apply(
    MARGIN = 1,
    FUN = function(x) factor(x, labels = 1:G) %>% as.character() %>% as.integer()
  ) %>% t()

  ls = label.switching::label.switching(
    method = "ECR", zpivot = z[nrow(z), ], z = z, K = G
  )


  for(i in 1:nrow(z)) {
    perm = ls$permutations$`ECR`[i, ]
    z[i, ] = order(perm)[z[i, ]]
  }

  return(z)
}

z_6 = relabel(fit, 6) %>% comp_class()
data.frame(z_6 = z_6) %>% saveRDS("models/z-est.rds")

data.frame(
  id = id,
  time = time,
  z = z_6
) %>%
  ggplot(aes(x=time, y=id, fill=factor(z))) +
  geom_tile(color="black")

bind_rows(
  # data.frame(scale(y, center = TRUE, scale = FALSE), id = id, time = time) %>%
  #   mutate(z = z_5) %>%
  #   gather(age, y, -id, -time, -z) %>%
  #   mutate(age = str_remove(age, "X") %>% as.integer()) %>%
  #   group_by(age, z) %>%
  #   summarise(avg = mean(y)) %>%
  #   mutate(G = 5) %>%
  #   mutate(z = factor(z, labels = c(3, 6, 4, 5, 1)) %>% as.character() %>% as.integer()),

  data.frame(scale(y, center = TRUE, scale = FALSE), id = id, time = time) %>%
    mutate(z = z_6) %>%
    gather(age, y, -id, -time, -z) %>%
    mutate(age = str_remove(age, "X") %>% as.integer()) %>%
    group_by(age, z) %>%
    summarise(avg = mean(y)) %>%
    mutate(G = 6) %>%
    mutate(z = factor(z, labels = c(4, 5, 6, 3, 1, 2)) %>% as.character() %>% as.integer()),

) %>%
  mutate(regime = paste0("G['+'] == ", G)) %>%
  ggplot(aes(x = age, y = avg, color = factor(z))) +
  geom_point() +
  geom_line() +
  labs(x = "Age group", y = "Avg centred logit mortality", color = "G") +
  facet_grid(. ~ regime, labeller = label_parsed)

ggsave("plots/G-analysis.pdf", width = 7, height = 3)

p1 + p2

data.frame(scale(y, center = TRUE, scale = FALSE), id = id, time = time) %>%
  mutate(z = z_6) %>%
  gather(age, y, -id, -time, -z) %>%
  mutate(age = str_remove(age, "X") %>% as.integer()) %>%
  group_by(age, z) %>%
  summarise(avg = mean(y)) %>%
  mutate(G = 6) %>%
  mutate(regime = paste0("G['+'] == ", G)) %>%
  ggplot(aes(x = age, y = avg, color = factor(z))) +
  geom_point() +
  geom_line() +
  labs(x = "Age group", y = "Avg centred logit mortality", color = "G") +
  facet_grid(. ~ regime, labeller = label_parsed)


