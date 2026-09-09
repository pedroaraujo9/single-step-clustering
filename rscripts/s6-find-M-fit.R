library(tidyverse)
library(mmcfa)
source("rscripts/utils.R")


#### data ####
wpp_data = readRDS("data/wpp_data_list.rds")
y = wpp_data$y
id = wpp_data$id
time = wpp_data$time

#### hyperparameters ####
K = 3
G = 6
M = 15
n_basis = 10
center_data = TRUE
scale_data = FALSE
w_dirichlet = 0.01
lambda = 1
init_list = NULL
seed = 1

#### init run ####
z_init = readRDS("models/z-est.rds")
z_init = z_init$z_6

#### find M with z being random ####
n_init = 10
init_iters = 10
n_basis = 10

w_find_random = mmcfa::find_number_clust(
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

saveRDS(w_find_random, paste0("models/find-G=", G, "-M=", M, "-z-random-fit-", Sys.time(), ".rds"))

#### find w with z being fixed ####
n_init = 10
init_iters = 10
n_basis = 10

w_find_fixed = mixscat::find_number_clust(
  M_max = M,
  z = z_init,
  id = id,
  time = time,
  n_basis = n_basis,
  n_init = n_init,
  init_iters = init_iters,
  dirichlet_param = w_dirichlet,
  lambda = lambda,
  seed = seed
)

w_find_fixed$z = init_list$z

saveRDS(w_find_random, paste0("models/find-G=", G, "-M=", M, "-z-fixed-fit-", Sys.time(), ".rds"))





