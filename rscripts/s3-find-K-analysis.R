library(tidyverse)
library(mmcfa)
source("rscripts/utils.R")

wpp_data = readRDS("data/wpp_data_list.rds")
y = wpp_data$y
id = wpp_data$id
time = wpp_data$time

cusp_fit = readRDS("models/cusp-fit-2026-04-30 18:46:15.213569-.rds")

lapply(1:length(cusp_fit$chains), function(chain) {
  cusp_fit$chains[[chain]]$sample_list$H_active[, 1]
})
