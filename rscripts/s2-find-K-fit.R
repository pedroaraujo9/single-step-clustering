library(tidyverse)
library(mmcfa)
source("rscripts/utils.R")

wpp_data = readRDS("data/wpp_data_list.rds")
y = wpp_data$y
id = wpp_data$id
time = wpp_data$time


iters = 10000
burn_in = 5000
thin = 10
chains = 3
n_cores = 1
seed = 1

K = 10

cusp_fit = fit_cusp(
  y = y,
  id = id,
  time = time,
  K = K,
  iters = iters,
  burn_in = burn_in,
  thin = thin,
  chains = chains,
  n_cores = n_cores,
  center_data = TRUE,
  scale_data = TRUE,
  init_list = NULL,
  seed = seed,
  cusp_nu = 4,
  cusp_a = 2,
  cusp_b = 2,
  cusp_min_var = 0.01,
  est_sigma = FALSE
)

saveRDS(cusp_fit, paste0("models/cusp-fit-", Sys.time(), "-.rds"))
