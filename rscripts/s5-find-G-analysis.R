library(tidyverse)
library(mixff)
source("rscripts/utils.R")

fit = readRDS("models/find-G-fit.rds")

lapply(fit$chains, function(chain){
  chain$sample_list$z %>% compute_clust_size()
})
