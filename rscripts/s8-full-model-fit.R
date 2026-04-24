library(tidyverse)
library(mixff)

wpp_data = readRDS("data/wpp_data_list.rds")
wpp_data_tidy = readRDS("data/qx-wpp-male-quality-1960-2019-80.rds")

y = wpp_data$y
id = wpp_data$id
time = wpp_data$time

K = 3
G = 5
M = 11

w_init_runs = readRDS("models/find-M-fit-z-fixed.rds")
w_prior = w_init_runs$post_modes$`11`$w_post_prob
w_init = w_init_runs$post_modes$`11`$w
z_init = w_find_fixed$model_data$data$z

iters = 400
burn_in = 200
thin = 10
chains = 2
n_cores = 1
seed = 1

init_run = fit_model(
  y = y,
  id = id,
  time = time,
  K = K,
  G = G,
  M = M,
  n_basis = 10,
  iters = iters,
  burn_in = burn_in,
  thin = thin,
  chains = chains,
  n_cores = n_cores,
  center_data = TRUE,
  scale_data = FALSE,
  relabel = TRUE,
  seed = seed,
  w_prior = w_prior,
  w_dirichlet = 1,
  z_dirichlet = 1,
  z = z_init,
  w = w_init,
  spline_intercept_penalty = 1,
  spline_penalty = 1,
  verbose = TRUE,
  init_list = NULL,
  mixscat_prior = TRUE,
  add_cluster = TRUE
)

iters = 100
burn_in = 50
thin = 10
chains = 2
n_cores = 1
seed = 1

init_list = list(
  w = w_init,
  z = z_init,
  alpha = init_run$post_sample$alpha %>% compute_post_stat(),
  mu = init_run$post_sample$mu %>% compute_post_stat(),
  sigma = init_run$post_sample$sigma %>% compute_post_stat(),
  beta = init_run$post_sample$beta %>% compute_post_stat()
)

fit = fit_model(
  y = y,
  id = id,
  time = time,
  K = K,
  G = G,
  M = M,
  n_basis = 10,
  iters = iters,
  burn_in = burn_in,
  thin = thin,
  chains = chains,
  n_cores = n_cores,
  center_data = TRUE,
  scale_data = FALSE,
  relabel = TRUE,
  seed = seed,
  w_prior = w_prior,
  w_dirichlet = 1,
  z_dirichlet = 1,
  init_list = init_list,
  z = NULL,
  w = NULL,
  spline_intercept_penalty = 1,
  spline_penalty = 1,
  verbose = TRUE,
  mixscat_prior = TRUE,
  add_cluster = TRUE
)

saveRDS(fit, "models/full-model-fit.rds")


