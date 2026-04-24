library(tidyverse)
library(mixff)
source("rscripts/utils.R")


#### data ####
wpp_data = readRDS("data/wpp_data_list.rds")
y = wpp_data$y
id = wpp_data$id
time = wpp_data$time

#### analysis ####
w_find$run_time
w_find_fixed$clust_size_p
w_find$clust_size_p

w_random = w_find$post_modes$`11`$w
w_fixed = w_find_fixed$post_modes$`11`$w


w_find_fixed$entropy %>% filter(iter == init_iters) %>% round(5)

w = w_fixed

cut = w %>% sort() %>% diff() %>% `==`(1) %>% which()
cut

w_prior = w_find_fixed$post_modes$`11`$w_post_prob

data.frame(
  id = id,
  time = time,
  z = init_list$z
) %>%
  mutate(id = factor(id, levels = names(sort(w)))) %>%
  ggplot(aes(x=time, y = id, fill = factor(z))) +
  geom_tile(color = "black") +
  geom_hline(yintercept = cut + 0.5)







