library(tidyverse)
source("rscripts/utils.R")

wpp_data = readRDS(
  "data/qx-wpp-male-quality-1960-2019-80.rds"
) %>%
  dplyr::select(country, year, age, qx) %>%
  tidyr::pivot_wider(names_from = age, values_from = qx)

wpp_data_list = list(
  id = wpp_data$country,
  time = wpp_data$year,
  y = wpp_data %>%
    dplyr::select(-country, -year) %>%
    as.matrix() %>%
    logit()
)

saveRDS(wpp_data_list, "data/wpp_data_list.rds")
