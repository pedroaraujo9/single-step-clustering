library(tidyverse)
source("rscripts/utils.R")


wpp_data_tidy = readRDS(
  "data/qx-wpp-male-quality-1960-2019-80.rds"
)


wpp_data = wpp_data_tidy %>%
  dplyr::select(country, year, age, qx) %>%
  tidyr::pivot_wider(names_from = age, values_from = qx)

continent = c(
  c(
    "Europe",      # Albania
    "Africa",      # Algeria
    "South America", # Argentina
    "Oceania",     # Australia
    "Europe",      # Austria
    "Europe",      # Belarus
    "Europe",      # Belgium
    "South America", # Brazil
    "Europe",      # Bulgaria
    "North America", # Canada
    "South America", # Chile
    "Asia",        # China
    "Asia",        # Hong Kong
    "Asia",        # Taiwan
    "South America", # Colombia
    "North America", # Costa Rica
    "Europe",      # Croatia
    "North America", # Cuba
    "Europe",      # Czechia
    "Europe",      # Denmark
    "North America", # Dominican Republic
    "South America", # Ecuador
    "Africa",      # Egypt
    "North America", # El Salvador
    "Europe",      # Finland
    "Europe",      # France
    "Asia",        # Georgia
    "Europe",      # Germany
    "Europe",      # Greece
    "Europe",      # Hungary
    "Asia",        # India
    "Asia",        # Iran
    "Europe",      # Ireland
    "Asia",        # Israel
    "Europe",      # Italy
    "Asia",        # Japan
    "Asia",        # Kazakhstan
    "Europe",      # Lithuania
    "Asia",        # Malaysia
    "North America", # Mexico
    "Asia",        # Mongolia
    "Europe",      # Netherlands
    "Oceania",     # New Zealand
    "Europe",      # Norway
    "North America", # Panama
    "South America", # Paraguay
    "South America", # Peru
    "Europe",      # Poland
    "Europe",      # Portugal
    "North America", # Puerto Rico
    "Asia",        # South Korea
    "Europe",      # Moldova
    "Europe",      # Russia
    "Asia",        # Saudi Arabia
    "Europe",      # Serbia
    "Asia",        # Singapore
    "Europe",      # Slovakia
    "Europe",      # Slovenia
    "Europe",      # Spain
    "Europe",      # Sweden
    "Europe",      # Switzerland
    "Asia",        # Thailand
    "Africa",      # Tunisia
    "Asia",        # Turkmenistan
    "Asia",        # Türkiye
    "Europe",      # Ukraine
    "Europe",      # U.K.
    "North America", # U.S.A.
    "South America", # Uruguay
    "Asia",        # Uzbekistan
    "South America"  # Venezuela
  )
)

latin_american = c(
  FALSE, # Albania
  FALSE, # Algeria
  TRUE,  # Argentina
  FALSE, # Australia
  FALSE, # Austria
  FALSE, # Belarus
  FALSE, # Belgium
  TRUE,  # Brazil
  FALSE, # Bulgaria
  FALSE, # Canada
  TRUE,  # Chile
  FALSE, # China
  FALSE, # Hong Kong
  FALSE, # Taiwan
  TRUE,  # Colombia
  TRUE,  # Costa Rica
  FALSE, # Croatia
  TRUE,  # Cuba
  FALSE, # Czechia
  FALSE, # Denmark
  TRUE,  # Dominican Republic
  TRUE,  # Ecuador
  FALSE, # Egypt
  TRUE,  # El Salvador
  FALSE, # Finland
  FALSE, # France
  FALSE, # Georgia
  FALSE, # Germany
  FALSE, # Greece
  FALSE, # Hungary
  FALSE, # India
  FALSE, # Iran
  FALSE, # Ireland
  FALSE, # Israel
  FALSE, # Italy
  FALSE, # Japan
  FALSE, # Kazakhstan
  FALSE, # Lithuania
  FALSE, # Malaysia
  TRUE,  # Mexico
  FALSE, # Mongolia
  FALSE, # Netherlands
  FALSE, # New Zealand
  FALSE, # Norway
  TRUE,  # Panama
  TRUE,  # Paraguay
  TRUE,  # Peru
  FALSE, # Poland
  FALSE, # Portugal
  TRUE,  # Puerto Rico
  FALSE, # South Korea
  FALSE, # Moldova
  FALSE, # Russia
  FALSE, # Saudi Arabia
  FALSE, # Serbia
  FALSE, # Singapore
  FALSE, # Slovakia
  FALSE, # Slovenia
  FALSE, # Spain
  FALSE, # Sweden
  FALSE, # Switzerland
  FALSE, # Thailand
  FALSE, # Tunisia
  FALSE, # Turkmenistan
  FALSE, # Türkiye
  FALSE, # Ukraine
  FALSE, # U.K.
  FALSE, # U.S.A.
  TRUE,  # Uruguay
  FALSE, # Uzbekistan
  TRUE   # Venezuela
)


wpp_data_list = list(
  id = wpp_data$country,
  time = wpp_data$year,
  continent = continent,
  is_latin_american = latin_american,
  y = wpp_data %>%
    dplyr::select(-country, -year) %>%
    as.matrix() %>%
    logit()
)

saveRDS(wpp_data_list, "data/wpp_data_list.rds")

#### centred curves ####
thesis_fig_path = "~/Documents/GitHub/Thesis---Pedro-Menezes-de-Araujo/figures/Chapter5/"

y = wpp_data_tidy %>%
  group_by(age) %>%
  summarise(center = mean(logit(qx))) %>%
  left_join(wpp_data_tidy, by = "age") %>%
  mutate(y = logit(qx) - center, y2 = logit(qx)) %>%
  select(country, year, age, y, y2) %>%
  left_join(
    data.frame(country = unique(wpp_data_tidy$country), continent = continent),
    by = "country"
  )

y %>%
  filter(year %in% c(1960, 1990, 2019)) %>%
  ggplot(aes(x=age, y=y, group = country)) +
  geom_line(alpha = 0.3) +
  facet_grid(. ~ year)

y %>%
  mutate(country = country %>% rename_countries()) %>%
  filter(country %in% c("Brazil", "Ireland", "Ukraine", "India")) %>%
  mutate(id = paste0(country, "-", year)) %>%
  ggplot(aes(x=age, y=y, group = id, color=year)) +
  geom_line(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(. ~ country) +
  viridis::scale_color_viridis() +
  labs(x = "Age group", y="Logit of centered mortality curve", color = "Period")

ggsave(paste0(thesis_fig_path, "centred-curves.pdf"), width = 8, height = 4)

y %>%
  mutate(country = country %>% rename_countries()) %>%
  filter(country %in% c("Brazil", "Ireland", "Ukraine", "India")) %>%
  mutate(id = paste0(country, "-", year)) %>%
  ggplot(aes(x=age, y=y2, group = id, color=year)) +
  geom_line(alpha = 0.6) +
  facet_grid(. ~ country) +
  viridis::scale_color_viridis() +
  labs(x = "Age group", y="Logit of mortality curve", color = "Period")

ggsave(paste0(thesis_fig_path, "curves.pdf"), width = 8, height = 4)


#### other data ####
male_empirical = readRDS("data/blv-qx-wpp-male-all-empirical-1960-2019-80.rds")
male_empirical_min = readRDS("data/blv-qx-wpp-male-empirical-pop-1960-2019-80.rds")
lf = readRDS("data/input-data-cluster-empirical-pop.rds")

male_lf = lf %>% filter(sex == "Male")

male_empirical = male_empirical$df %>% spread(age, qx)
male_empirical_min = male_empirical_min$df %>% spread(age, qx)

male_empirical_min$country %>% unique()
male_empirical$country %>% unique()

logit = function(x) log(x / (1 - x))

library(mmcfa)

fit = fit_model(
  y = male_empirical %>% select(-(country:year)) %>% as.matrix() %>% logit(),
  id = male_empirical$country,
  time = male_empirical$year,
  K = 4,
  G = 10,
  M = 2,
  n_basis = 2,
  iters = 1000,
  burn_in = 500,
  thin = 2,
  chains = 1,
  n_cores = 1,
  mixscat_prior = FALSE,
  center_data = TRUE,
  scale_data = FALSE,
  init_list = NULL,
  z = NULL,
  w = NULL,
  w_prior = NULL,
  seed = 1,
  w_dirichlet = 0.01,
  z_dirichlet = 0.01,
  spline_intercept_penalty = 1,
  spline_penalty = 1,
  add_cluster = TRUE,
  relabel = FALSE
)

fit = fit_model(
  y = male_lf %>% select(`1`:`4`) %>% as.matrix(),
  id = male_lf$country,
  time = male_lf$year,
  K = 4,
  G = 10,
  M = 2,
  n_basis = 2,
  iters = 1000,
  burn_in = 500,
  thin = 2,
  chains = 1,
  n_cores = 1,
  mixscat_prior = FALSE,
  center_data = TRUE,
  scale_data = FALSE,
  init_list = NULL,
  z = NULL,
  w = NULL,
  w_prior = NULL,
  seed = 1,
  w_dirichlet = 0.01,
  z_dirichlet = 0.01,
  spline_intercept_penalty = 1,
  spline_penalty = 1,
  add_cluster = TRUE,
  relabel = FALSE
)

clust_size = lapply(fit$chains, function(chain){
  chain$sample_list$z %>% compute_clust_size()
}) %>% do.call(cbind, .)

clust_size_mode = apply(
  clust_size,
  MARGIN = 2,
  FUN = function(x) x %>% table() %>% which.max() %>% names() %>% as.integer()
)

clust_size_mode
table(as.numeric(clust_size)) %>% prop.table()


fit2 = fit_model(
  y = male_empirical_min %>% select(-(country:year)) %>% as.matrix() %>% logit(),
  id = male_empirical_min$country,
  time = male_empirical_min$year,
  K = 4,
  G = 10,
  M = 2,
  n_basis = 2,
  iters = 2000,
  burn_in = 1000,
  thin = 2,
  chains = 1,
  n_cores = 1,
  mixscat_prior = FALSE,
  center_data = TRUE,
  scale_data = FALSE,
  init_list = NULL,
  z = NULL,
  w = NULL,
  w_prior = NULL,
  seed = 1,
  w_dirichlet = 0.01,
  z_dirichlet = 0.01,
  spline_intercept_penalty = 1,
  spline_penalty = 1,
  add_cluster = TRUE,
  relabel = FALSE
)

clust_size = lapply(fit2$chains, function(chain){
  chain$sample_list$z %>% compute_clust_size()
}) %>% do.call(cbind, .)

clust_size_mode = apply(
  clust_size,
  MARGIN = 2,
  FUN = function(x) x %>% table() %>% which.max() %>% names() %>% as.integer()
)

clust_size_mode
table(as.numeric(clust_size)) %>% prop.table()

z_df = data.frame(
  country = male_empirical_min$country,
  year = male_empirical_min$year,
  z = fit2$post_sample$z %>% comp_class()
)

z_df %>%
  ggplot(aes(x=year, y=country, fill = as.factor(z))) +
  geom_tile(color="black")


z_df %>%
  right_join(
    male_empirical_min %>% gather(age, qx, -country, -year, -population),
    by = c("country", "year")
  ) %>%
  group_by(z, age) %>%
  summarise(mean_qx = mean(logit(qx))) %>%
  ggplot(aes(x=as.integer(age), y=mean_qx, group = z, color = as.factor(z))) +
  geom_line()















