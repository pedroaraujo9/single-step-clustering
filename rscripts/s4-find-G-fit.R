library(tidyverse)
library(mixff)
source("rscripts/utils.R")

wpp_data = readRDS("data/wpp_data_list.rds")
y = wpp_data$y
id = wpp_data$id
time = wpp_data$time

iters = 1000
burn_in = 500
thin = 10
chains = 3
n_cores = 1
seed = 1
n_basis = 2

fit = fit_model(
  y = y,
  id = id,
  time = time,
  K = 3,
  G = 10,
  M = 2,
  n_basis = n_basis,
  iters = iters,
  burn_in = burn_in,
  thin = thin,
  chains = chains,
  n_cores = n_cores,
  mixscat_prior = FALSE,
  center_data = TRUE,
  scale_data = FALSE,
  init_list = NULL,
  z = NULL,
  w = NULL,
  w_prior = NULL,
  seed = seed,
  w_dirichlet = 0.01,
  z_dirichlet = 0.01,
  spline_intercept_penalty = 1,
  spline_penalty = 1,
  add_cluster = TRUE,
  relabel = FALSE
)

saveRDS(fit, paste0("models/find-G-fit-", Sys.time(), "-.rds"))
