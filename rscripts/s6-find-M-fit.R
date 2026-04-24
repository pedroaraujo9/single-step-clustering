library(tidyverse)
library(mixff)
source("rscripts/utils.R")


#### data ####
wpp_data = readRDS("data/wpp_data_list.rds")
y = wpp_data$y
id = wpp_data$id
time = wpp_data$time

#### hyperparameters ####
K = 3
G = 6
M = 20
n_basis = 10
center_data = TRUE
scale_data = FALSE
w_dirichlet = 0.01
lambda = 1
init_list = NULL
seed = 1

#### init run ####
init_run = fit_model(
  y = y,
  id = id,
  time = time,
  K = K,
  G = G,
  M = M,
  n_basis = n_basis,
  iters = 2000,
  burn_in = 1000,
  thin = 10,
  chains = 1,
  n_cores = 1,
  center_data = center_data,
  scale_data = scale_data,
  seed = seed,
  z_dirichlet = 1,
  init_list = NULL,
  mixscat_prior = FALSE,
  add_cluster = TRUE
)

z_init = init_run$post_sample$z %>% comp_class()

init_list = list(
  alpha = init_run$post_sample$alpha %>% compute_post_stat(),
  sigma = init_run$post_sample$sigma %>% compute_post_stat(),
  mu = init_run$post_sample$mu %>% compute_post_stat(),
  z = z_init,
  beta = matrix(0, nrow = M * n_basis, ncol = G)
)

#### find M with z being random ####
n_init = 100
init_iters = 100
n_basis = 10

w_find_random = mixff::find_number_clust(
  y = y,
  id = id,
  time = time,
  K = K,
  G = G,
  M = M,
  z = NULL,
  n_init = n_init,
  init_iters = init_iters,
  n_basis = n_basis,
  spline_intercept_penalty = lambda,
  spline_penalty = lambda,
  center_data = center_data,
  scale_data = scale_data,
  init_list = NULL,
  seed = seed,
  w_dirichlet = w_dirichlet,
  verbose = TRUE
)

saveRDS(w_find_random, paste0("models/find-M-z-random-fit-", Sys.time(), "-.rds"))

#### find w with z being fixed ####
n_init = 100
init_iters = 50
n_basis = 10

w_find_fixed = mixscat::find_number_clust(
  M_max = M,
  z = init_list$z,
  id = id,
  time = time,
  n_basis = n_basis,
  n_init = n_init,
  init_iters = init_iters,
  dirichlet_param = w_dirichlet,
  lambda = lambda,
  seed = seed
)

saveRDS(w_find_fixed, paste0("models/find-M-z-fixed-fit-", Sys.time(), "-.rds"))





