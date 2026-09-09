library(tidyverse)
library(mixff)
source("rscripts/utils.R")


wpp_data = readRDS("data/wpp_data_list.rds")

y = wpp_data$y
id = wpp_data$id
time = wpp_data$time

K = 3
G = 6
M = 13

w_find = readRDS("models/find-G=6-M=15-z-fixed-fit-2026-04-26 16:20:44.22298.rds")
w_prior = w_find$post_modes[[as.character(M)]]$w_post_prob
w_init = w_find$post_modes[[as.character(M)]]$w
z_init = w_find$model_data$data$z

#### init run ####
seed = 1
iters = 1000
burn_in = 900
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
  z = z_init,
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
  z = z_init,
  alpha = init_run$post_sample$alpha[last_iter,,],
  mu = init_run$post_sample$mu[last_iter,,],
  sigma = init_run$post_sample$sigma[last_iter,,],
  beta = init_run$post_sample$beta[last_iter,,],
  psi = init_run$post_sample$psi[last_iter,]
)


#### final fit ####
iters = 400
burn_in = 200
thin = 2
chains = 3
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

saveRDS(fit, paste0("models/full-model-fit-fixed-", Sys.time(), "-.rds"))


