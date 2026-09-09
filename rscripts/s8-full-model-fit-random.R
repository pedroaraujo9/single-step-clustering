library(tidyverse)
library(mmcfa)
source("rscripts/utils.R")


wpp_data = readRDS("data/wpp_data_list.rds")

y = wpp_data$y
id = wpp_data$id
time = wpp_data$time

K = 3
G = 6
M = 13

w_find = readRDS("models/find-M-z-random-fit-2026-04-25 04:00:23.342081-.rds")
w_prior = w_find$post_modes[[as.character(M)]]$w_post_prob
w_init = w_find$post_modes[[as.character(M)]]$w

#### init run ####
seed = 1
iters = 10
burn_in = 5
thin = 1
chains = 1
n_cores = 1

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
  z = NULL,
  w = w_init,
  spline_intercept_penalty = 1,
  spline_penalty = 1,
  verbose = TRUE,
  init_list = NULL,
  mixscat_prior = TRUE,
  add_cluster = TRUE
)

last_iter = init_run$post_sample$alpha %>% nrow()

init_list = list(
  w = w_init,
  z = init_run$post_sample$z[last_iter,],
  alpha = init_run$post_sample$alpha[last_iter,,],
  mu = init_run$post_sample$mu[last_iter,,],
  sigma = init_run$post_sample$sigma[last_iter,,],
  beta = init_run$post_sample$beta[last_iter,,]
)

#### final fit ####
iters = 10
burn_in = 5
thin = 1
chains = 1
n_cores = 1

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
  drop_params = c("pw", "pz", "alpha_precision"),
  add_cluster = TRUE
)

saveRDS(fit, paste0("models/full-model-fit-random-", Sys.time(), "-.rds"))


