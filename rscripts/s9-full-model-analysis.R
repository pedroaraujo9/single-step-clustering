library(tidyverse)
library(mmcfa)
source("rscripts/utils.R")
thesis_fig_path = "~/Documents/GitHub/Thesis---Pedro-Menezes-de-Araujo/figures/Chapter5/"

wpp_data = readRDS("data/wpp_data_list.rds")
y = wpp_data$y
id = wpp_data$id
time = wpp_data$time
continent = wpp_data$continent
is_latin_america = wpp_data$is_latin_america

fit = readRDS("models/full-model-fit-fixed-2026-05-01 05:23:26.077289-.rds")
G = 6

#### convergence ####
compute_convergence = function(param_name, fit, tr = function(x) x) {

  param_dim = dim(fit$chains$`chain=1`$sample_list[[param_name]])

  metrics = lapply(1:param_dim[2], function(i){
    lapply(1:param_dim[3], function(j){

      rhat = lapply(fit$chains, function(chain){
        chain$sample_list[[param_name]][, i, j] %>% tr()
      }) %>% do.call(cbind, .) %>%
        posterior::rhat()

      ess = lapply(fit$chains, function(chain){
        chain$sample_list[[param_name]][, i, j] %>% tr()
      }) %>% do.call(cbind, .) %>%
        posterior::ess_basic()

      data.frame(rhat = rhat, ess = ess, param = paste0(param_name, "[", i, ",", j, "]"))

    }) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)

}

fit$post_sample$alpha %>% dim()

beta_conv = compute_convergence("beta", fit)
alpha_conv = compute_convergence("alpha", fit)
theta_conv = compute_convergence("theta", fit)
mu_conv = compute_convergence("mu", fit)
sigma_conv = compute_convergence("sigma", fit, tr = log)

sigma_conv %>% arrange(desc(rhat)) %>% head(10)
fit$chains$`chain=1`$sample_list$sigma[, 20, 2] %>% mean()
fit$chains$`chain=2`$sample_list$sigma[, 20, 2] %>% mean()
fit$chains$`chain=3`$sample_list$sigma[, 20, 2] %>% mean()
fit$post_sample$sigma[, 20, 2] %>% log() %>% plot()

bind_rows(
  beta_conv %>% mutate(param = "beta"),
  alpha_conv %>% mutate(param = "alpha"),
  theta_conv %>% mutate(param = "theta"),
  mu_conv %>% mutate(param = "mu"),
  sigma_conv %>% mutate(param = "log(sigma)")
) %>%
  drop_na() %>%
  ggplot(aes(x = param, y = rhat)) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_x_discrete(labels = function(x) parse(text = x)) +
  labs(
    x = "Parameter",
    y = latex2exp::TeX("$\\hat{R}$")
  )

ggsave(paste0(thesis_fig_path, "rhat-params.pdf"), width = 5, height = 3)

bind_rows(
  beta_conv %>% mutate(param = "beta"),
  alpha_conv %>% mutate(param = "alpha"),
  theta_conv %>% mutate(param = "theta"),
  mu_conv %>% mutate(param = "mu"),
  sigma_conv %>% mutate(param = "log(sigma)")
) %>%
  drop_na() %>%
  ggplot(aes(x = param, y = ess)) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_x_discrete(labels = function(x) parse(text = x)) +
  labs(
    x = "Parameter",
    y = "Effective sample size"
  )

ggsave(paste0(thesis_fig_path, "ess-params.pdf"), width = 5, height = 3)

lapply(fit$chains, function(chain){
  chain$sample_list$mu[, 5, 1]
}) %>% do.call(cbind, .) %>%
  posterior::rhat()

lapply(fit$chains, function(chain){
  chain$sample_list$mu[, 3, 1]
}) %>% do.call(cbind, .) %>%
  as.data.frame() %>%
  mutate(iter = 1:nrow(.)) %>%
  gather(chain, value, -iter) %>%
  ggplot(aes(x=iter, y=value, color=chain)) +
  geom_line()

rhat_params = lapply(list(beta_conv, alpha_conv, theta_conv, mu_conv, sigma_conv), function(df){
  df %>% drop_na() %>% .$rhat %>% summary()
}) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  mutate(param = c("beta", "alpha", "theta", "mu", "sigma"))

ess_params = lapply(list(beta_conv, alpha_conv, theta_conv, mu_conv, sigma_conv), function(df){
  df %>% drop_na() %>% .$ess %>% summary()
}) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  mutate(param = c("beta", "alpha", "theta", "mu", "sigma"))

rhat_params
ess_params


ws = lapply(1:length(fit$chains), function(i){
  fit$chains[[i]]$sample_list$w %>% comp_class()
})

mclust::adjustedRandIndex(ws[[1]], ws[[2]])
mclust::adjustedRandIndex(ws[[1]], ws[[3]])
mclust::adjustedRandIndex(ws[[2]], ws[[3]])

table(ws[[1]], ws[[3]])

ws[[1]] %>% unique() %>% sort()
ws[[2]] %>% unique() %>% sort()
ws[[3]] %>% unique() %>% sort()

#### coefs ####
alpha_avg = (-fit$post_sample$alpha) %>% compute_post_stat()
alpha_li = (-fit$post_sample$alpha) %>% compute_post_stat(function(x) HDInterval::hdi(x, credMass = 0.95)[1])
alpha_ui = (-fit$post_sample$alpha) %>% compute_post_stat(function(x) HDInterval::hdi(x, credMass = 0.95)[2])

alpha_avg %>%
  as.data.frame() %>%
  mutate(age = colnames(wpp_data$y)) %>%
  gather(dim, post_mean, -age) %>%
  left_join(
    alpha_li %>%
      as.data.frame() %>%
      mutate(age = colnames(wpp_data$y)) %>%
      gather(dim, li, -age),
    by = c("age", "dim")
  ) %>%
  left_join(
    alpha_ui %>%
      as.data.frame() %>%
      mutate(age = colnames(wpp_data$y)) %>%
      gather(dim, ui, -age),
    by = c("age", "dim")
  ) %>%
  mutate(age = as.integer(age)) %>%
  mutate(dim = str_replace(dim, "V", "")) %>%
  ggplot(aes(x=age, y=post_mean, color=dim)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=li, ymax=ui, fill=dim), alpha=0.2, color=NA) +
  labs(x="Age group", y="Loadings", color="Dimension", fill="Dimension")

ggsave(paste0(thesis_fig_path, "alpha-coefs.pdf"), width = 5, height = 3)

#### theta ####
theta_avg = (-fit$post_sample$theta) %>% compute_post_stat()

theta_avg %>%
  as.data.frame() %>%
  mutate(id = id %>% rename_countries(), time = time) %>%
  gather(dim, post_mean, -id, -time) %>%
  filter(id %in% c("Russia", "Ukraine", "Ireland", "Brazil")) %>%
  ggplot(aes(x=time, y=post_mean, color=id)) +
  geom_line() +
  facet_grid(. ~ dim)

#### mortality states ####
z_post_sample = fit$post_sample$z
z = z_post_sample %>% comp_class()
z_post_prob = fit$post_sample$z_post_prob %>% compute_post_stat()
z_sel_post_prob = lapply(1:length(z), function(i){
  mean(z_post_sample[, i] == z[i])
}) %>% do.call(c, .)

state_names = c("High", "High adult", "Mid + young adult", "Mid", "Mid - young", "Low")
new_label = c(6, 4, 2, 1, 5, 3)

z = factor(z, labels = new_label) %>%
  as.character() %>%
  as.integer() %>%
  factor(levels = 1:G, labels = state_names)

##### centroids ####
data.frame(scale(y, center = T, scale = F), id = id, time = time) %>%
  mutate(z = z) %>%
  gather(age, y, -id, -time, -z) %>%
  mutate(age = str_remove(age, "X") %>% as.integer()) %>%
  group_by(age, z) %>%
  summarise(avg = mean(y), .groups = "drop_last") %>%
  ggplot(aes(x=age, y=avg, color=factor(z))) +
  geom_point() +
  geom_line() +
  labs(x="Age group", y="Avg centred logit mortality", color="Mortality state")

ggsave(paste0(thesis_fig_path, "mortality-states.pdf"), width = 6, height = 3)

##### prop states over time ####
state_prop_sample = lapply(1:nrow(z_post_sample), function(i){

  cat(i, "\r")

  z = z_post_sample[i, ] %>%
    factor(labels = new_label) %>%
    as.character() %>%
    as.integer() %>%
    factor(levels = 1:G) %>%
    factor(labels = state_names)

  lapply(unique(time), function(t){
    z[time == t] %>% table() %>% prop.table()
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    mutate(time = unique(time)) %>%
    gather(state, prop, -time) %>%
    mutate(iter = i)

}) %>% do.call(rbind, .)

state_prop_sample %>%
  group_by(time, state) %>%
  summarise(prop_avg = mean(prop), li = HDInterval::hdi(prop)[1], ui = HDInterval::hdi(prop)[2],
            .groups = "drop_last") %>%
  mutate(state = factor(state, levels = state_names)) %>%
  ggplot(aes(x=time, y=prop_avg, color=state)) +
  geom_line() +
  geom_ribbon(aes(x=time, ymin=li, ymax=ui, fill=state), alpha=0.2, inherit.aes = FALSE) +
  labs(x="Period", y="Proportion", color="Mortality state", fill="Mortality state") +
  scale_y_continuous(limits = c(0, 1))

ggsave(paste0(thesis_fig_path, "time-mortality-states.pdf"), width = 6, height = 3)

##### entropy ####
compute_post_entropy = function(z_post_sample, time, G, state_names) {

  entropy_post_sample = lapply(1:nrow(z_post_sample), function(i){

    cat(i, "\r")

    z = z_post_sample[i, ] %>%
      factor(levels = 1:G, labels = new_label) %>%
      as.character() %>%
      as.integer() %>%
      factor(levels = 1:G, labels = state_names)

    lapply(unique(time), function(t){

      p = z[time == t] %>% table() %>% prop.table() %>% as.numeric()
      -sum(ifelse(p > 0, p * log(p), 0)) / log(G)
    }) %>%
      do.call(c, .) %>%
      data.frame(entropy = .) %>%
      mutate(time = unique(time)) %>%
      as.data.frame() %>%
      mutate(iter = i)

  }) %>% do.call(rbind, .)

  entropy_post_sample

}

european_countries = unique(id)[continent == "Europe"]
latam_countries = unique(id)[is_latin_america]

entropy_post_sample = compute_post_entropy(z_post_sample, time, G, state_names)

entropy_europe = compute_post_entropy(
  z_post_sample[, id %in% european_countries], time, G, state_names
)

entropy_latam = compute_post_entropy(
  z_post_sample[, id %in% latam_countries], time, G, state_names
)

entropy_summ = entropy_post_sample %>%
  group_by(time) %>%
  summarise(entropy_avg = mean(entropy),
            li = HDInterval::hdi(entropy)[1], ui = HDInterval::hdi(entropy)[2])

entropy_europe_summ = entropy_europe %>%
  group_by(time) %>%
  summarise(entropy_avg = mean(entropy),
            li = HDInterval::hdi(entropy)[1], ui = HDInterval::hdi(entropy)[2])

entropy_latam_summ = entropy_latam %>%
  group_by(time) %>%
  summarise(entropy_avg = mean(entropy),
            li = HDInterval::hdi(entropy)[1], ui = HDInterval::hdi(entropy)[2])

entropy_summ %>%
  ggplot(aes(x=time, y=entropy_avg)) +
  geom_line() +
  geom_ribbon(aes(x=time, ymin=li, ymax=ui), alpha=0.2) +
  labs(x="Period", y="Normalised entropy") +
  scale_y_continuous(limits = c(0, 1))

ggsave(paste0(thesis_fig_path, "entropy-mortality-states.pdf"), width = 5, height = 3)

bind_rows(
  entropy_summ %>% mutate(group = "All countries"),
  entropy_europe_summ %>% mutate(group = "European countries"),
  entropy_latam_summ %>% mutate(group = "Latin American countries")
) %>%
  ggplot(aes(x=time, y=entropy_avg, color=group)) +
  geom_line() +
  geom_ribbon(aes(x=time, ymin=li, ymax=ui, fill=group), alpha=0.2, inherit.aes = F) +
  labs(x="Period", y="Normalised entropy", color = "", fill="") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "top")

##### sigma ####
sigma_avg = fit$post_sample$sigma %>% compute_post_stat()

sigma_id = data.frame(sigma_avg) %>%
  mutate(id = unique(id)) %>%
  gather(state, sigma, -id) %>%
  as_tibble() %>%
  mutate(state = state %>%
           str_remove("X") %>%
           as.integer() %>%
           factor(labels = new_label) %>%
           as.character() %>%
           as.integer() %>%
           factor(levels = 1:G, labels = state_names))

data.frame(id, z) %>%
  distinct() %>%
  left_join(sigma_id, by = c("z" = "state", "id")) %>%
  filter(z == "Mid + young adult") %>%
  ggplot(aes(x=id, y=sigma)) +
  geom_bar(stat = "identity") +
  coord_flip()


#### country clusters ####
data.frame(
  id = id,
  time = time,
  z = fit$args$init_list$z
) %>%
  mutate(id = factor(id, levels = names(sort(fit$args$init_list$w)))) %>%
  ggplot(aes(x=time, y=id, fill=factor(z))) +
  geom_tile(color = "black")

any(is.na(id))

w_post_sample = fit$post_sample$w
w = w_post_sample %>% comp_class()
spline_prob_sample = fit$post_sample$spline_probs
names(w) = names(w) %>% rename_countries()
id = id %>% rename_countries()

plot_seq(
  M = 13,
  w = w,
  z = z,
  id = id,
  time = time,
  spline_probs_sample = spline_prob_sample,
  z_sel_post_prob = z_sel_post_prob,
  map_new_class = NULL,
  cluster_label = NULL,
  label_hjust = -0.1
) +
  labs(x = "Period", y="Country", fill="Mortality state")

ggsave("plots/seqplot-male.pdf", width = 10, height = 10)

new_class = c(4, 8, 9, 5, 12, 7, 2, 6, 13, 11, 1, 3, 10)

plot_seq(
  M = 13,
  w = w,
  z = z,
  id = id,
  time = time,
  z_sel_post_prob = z_sel_post_prob,
  spline_probs_sample = spline_prob_sample,
  map_new_class = new_class,
  cluster_label = NULL,
  label_hjust = -0.2
) +
  labs(x="Period", y="Country", fill = "Mortality state ") +
  theme(legend.position = "top")

ggsave(paste0(thesis_fig_path, "seqplot-male.pdf"), width = 10, height = 10)

plot_seq_include_mix(
  M = 13,
  w = w,
  z = z,
  id = id,
  time = time,
  z_sel_post_prob = z_sel_post_prob,
  spline_probs_sample = spline_prob_sample,
  map_new_class = new_class,
  cluster_label = NULL,
  included = 5:10,
  label_hjust = -0.2
) +
  labs(x="Period", y="Country", fill = "Mortality state ") +
  theme(legend.position = "top")

ggsave(paste0("plots/seqplot-male-western.pdf"), width = 10, height = 7)

#### validation ####
data = data.frame(scale(y, center = T, scale = F), id = id, time = time) %>%
  mutate(z = z, w = w[id]) %>%
  gather(age, y, -id, -time, -z, -w) %>%
  mutate(age = str_remove(age, "X") %>% as.integer()) %>%
  as_tibble()

data

data %>%
  filter(id %in% c("South Korea", "Cuba", "Portugal")) %>%
  filter(age %in% c(0, 10, 30, 60, 80)) %>%
  ggplot(aes(x=time, y = y, color=id)) +
  geom_line() +
  facet_grid(. ~ age)

data %>%
  filter(w %in% c(2, 8)) %>%
  filter(age %in% c(0, 10, 30, 60, 80)) %>%
  ggplot(aes(x=time, y = y, color=id)) +
  geom_line() +
  facet_grid(w ~ age)









