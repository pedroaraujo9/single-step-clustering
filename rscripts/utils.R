compute_post_stat = function(sample, stat_function = mean, ...) {

  dims = length(dim(sample))

  if(!is.null(sample)) {

    if(dims == 3) {
      est = lapply(1:dim(sample)[3], function(g){
        cbind(sample[, , g]) |> apply(MARGIN = 2, FUN = stat_function, ...)
      }) %>% do.call(cbind, .)

    }else if(dims == 2) {

      est = apply(sample, FUN = stat_function, MARGIN = 2,  ...)

    }else if(dims == 1) {

      est = stat_function(sample, ...)

    }

  }else{
    est = NULL
  }

  return(est)

}

comp_class = function(sample) {

  class = apply(
    sample,
    MARGIN = 2,
    FUN = function(x) x |> table() |> which.max() |> names() |> as.integer()
  )

  return(class)

}

logit = function(x) {
  log(x/(1-x))
}

mse_score = function(y_true, y_pred) {
  mean((y_true - y_pred)^2)
}

rmse_score = function(y_true, y_pred) {
  sqrt(mse_score(y_true, y_pred))
}

corr_score = function(y_true, y_pred) {
  diag(cor(y_true, y_pred))
}


compute_clust_size = function(z_sample) {
  apply(z_sample, MARGIN = 1, function(x) length(unique(x)))
}

rename_countries = function(x) {
  x %>%
    str_replace("Venezuela \\(Bolivarian Republic of\\)", "Venezuela") %>%
    str_replace("United States of America", "U.S.A.") %>%
    str_replace("United Kingdom", "U.K.") %>%
    str_replace("Russian Federation", "Russia") %>%
    str_replace("Republic of Korea", "South Korea") %>%
    str_replace("Republic of Moldova", "Moldova") %>%
    str_replace("Iran \\(Islamic Republic of\\)", "Iran") %>%
    str_replace("China, Taiwan Province of China", "Taiwan") %>%
    str_replace("China\\, Hong Kong SAR", "Hong Kong")
}

plot_seq = function(M,
                    w,
                    z,
                    id,
                    time,
                    z_sel_post_prob,
                    spline_probs_sample = NULL,
                    map_new_class = NULL,
                    cluster_label = NULL,
                    label_hjust = -0.1) {

  n_time = length(unique(time))

  prob_stage_summ = spline_probs_sample %>%
    compute_post_stat() %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(time = rep(unique(time), times = M), w = rep(1:M, each = unique(n_time))) %>%
    gather(state, prob, -time, -w) %>%
    mutate(state = factor(state, labels = levels(z)))

  w_base = w

  if(!is.null(map_new_class)) {
    for(i in 1:length(map_new_class)) {
      w[w_base == i] = map_new_class[i]
    }

    w_base_summ = prob_stage_summ$w

    for(i in 1:length(map_new_class)) {
      prob_stage_summ$w[w_base_summ == i] = map_new_class[i]
    }
  }

  z_mode = prob_stage_summ %>%
    dplyr::group_by(time, w) %>%
    dplyr::summarise(z_mode = state[which.max(prob)], .groups = "drop") %>%
    dplyr::arrange(w, time)

  cluster_init_end = data.frame(w = w, id = names(w)) %>%
    dplyr::arrange(w) %>%
    dplyr::mutate(id = factor(id, levels = id)) %>%
    dplyr::arrange(w, id) %>%
    dplyr::group_by(w) %>%
    dplyr::summarise(min_id = min(as.integer(id)), max_id = max(as.integer(id)))

  # Create a variable for mode difference
  mode_change = z_mode %>%
    dplyr::group_by(w) %>%
    dplyr::mutate(mode_diff = c(0, z_mode %>% as.integer() %>% diff())) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::filter(mode_diff != 0) %>%
    dplyr::select(time, w) %>%
    dplyr::left_join(cluster_init_end, by = c("w"))

  model_change_gg = ggplot2::geom_segment(
    data = mode_change,
    ggplot2::aes(x = time - 0.5, xend = time - 0.5, y = min_id - 0.5, yend = max_id + 0.5),
    inherit.aes = F,
    color = "white", linetype = 1
  )

  id_label = id %>% unique()

  cut_point = data.frame(w = w, id = names(w)) %>%
    dplyr::mutate(id = id %>% factor(levels = id_label)) %>%
    dplyr::arrange(w, id) %>%
    dplyr::group_by(w) %>%
    dplyr::slice_tail(n = 1) %>%
    .$id

  mid_points = data.frame(w = w, id = names(w)) %>%
    dplyr::arrange(w) %>%
    dplyr::mutate(id = factor(id, levels = id)) %>%
    dplyr::group_by(w) %>%
    dplyr::summarise(mid_point = round(stats::median(as.integer(id)), 0)) %>%
    dplyr::mutate(w = paste0("Cluster ", w))

  if(!is.null(cluster_label)) {
    mid_points$w = factor(mid_points$w, labels = cluster_label)
  }

  data.frame(id = id, time = time, z = z) %>%
    dplyr::mutate(id = factor(id, levels = names(sort(w)))) %>%
    dplyr::mutate(state_prob = z_sel_post_prob) %>%
    ggplot2::ggplot(ggplot2::aes(x=time, y=id, fill=factor(z))) +
    ggplot2::geom_tile(color="gray20", aes(alpha = state_prob)) +
    ggplot2::labs(x="Time", y="Id", fill=expression(Z[it])) +
    ggplot2::geom_hline(yintercept = which(unique(names(sort(w))) %in% cut_point) + 0.5,
                        color = "white", linewidth = 1) +
    ggplot2::scale_alpha_continuous(
      range = c(0.3, 1),
      name = "State probability"
    ) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0)) +
    model_change_gg +
    ggplot2::theme_minimal() +
    ggplot2::geom_text(
      data = mid_points,
      ggplot2::aes(x = max(time), y = mid_point, label = w),
      inherit.aes = FALSE,
      hjust = ifelse(is.null(label_hjust), -0.3, label_hjust),
      angle = 0
    ) +
    ggplot2::coord_cartesian(clip = 'off') +
    ggplot2::theme(
      legend.position = "top",
      plot.margin = ggplot2::margin(5, 60, 5, 5)
    ) +
  ggplot2::guides(alpha = "none")
}

plot_seq_include = function(w,
                            z,
                            id,
                            time,
                            included = NULL,
                            spline_probs_sample = NULL,
                            map_new_class = NULL,
                            cluster_label = NULL,
                            label_hjust = -0.1) {

  n_time = length(unique(time))
  M = length(unique(w))

  prob_stage_summ = spline_probs_sample %>%
    comp_post_stat() %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(time = rep(unique(time), times = M), w = rep(1:M, each = unique(n_time))) %>%
    gather(state, prob, -time, -w) %>%
    mutate(state = factor(state, labels = levels(z)))

  w_base = w

  if(!is.null(map_new_class)) {
    for(i in 1:length(map_new_class)) {
      w[w_base == i] = map_new_class[i]
    }

    w_base_summ = prob_stage_summ$w

    for(i in 1:length(map_new_class)) {
      prob_stage_summ$w[w_base_summ == i] = map_new_class[i]
    }
  }

  # Filter to included clusters after relabelling
  if(!is.null(included)) {
    keep_ids = names(w)[w %in% included]
    keep_rows = id %in% keep_ids
    id   = id[keep_rows]
    time = time[keep_rows]
    z    = z[keep_rows]
    w    = w[w %in% included]
    prob_stage_summ = prob_stage_summ %>% dplyr::filter(w %in% included)
  }

  z_mode = prob_stage_summ %>%
    dplyr::group_by(time, w) %>%
    dplyr::summarise(z_mode = state[which.max(prob)], .groups = "drop") %>%
    dplyr::arrange(w, time)

  cluster_init_end = data.frame(w = w, id = names(w)) %>%
    dplyr::arrange(w) %>%
    dplyr::mutate(id = factor(id, levels = id)) %>%
    dplyr::arrange(w, id) %>%
    dplyr::group_by(w) %>%
    dplyr::summarise(min_id = min(as.integer(id)), max_id = max(as.integer(id)))

  # Create a variable for mode difference
  mode_change = z_mode %>%
    dplyr::group_by(w) %>%
    dplyr::mutate(mode_diff = c(0, z_mode %>% as.integer() %>% diff())) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::filter(mode_diff != 0) %>%
    dplyr::select(time, w) %>%
    dplyr::left_join(cluster_init_end, by = c("w"))

  model_change_gg = ggplot2::geom_segment(
    data = mode_change,
    ggplot2::aes(x = time - 0.5, xend = time - 0.5, y = min_id - 0.5, yend = max_id + 0.5),
    inherit.aes = F,
    color = "white", linetype = 1
  )

  id_label = id %>% unique()

  cut_point = data.frame(w = w, id = names(w)) %>%
    dplyr::mutate(id = id %>% factor(levels = id_label)) %>%
    dplyr::arrange(w, id) %>%
    dplyr::group_by(w) %>%
    dplyr::slice_tail(n = 1) %>%
    .$id

  mid_points = data.frame(w = w, id = names(w)) %>%
    dplyr::arrange(w) %>%
    dplyr::mutate(id = factor(id, levels = id)) %>%
    dplyr::group_by(w) %>%
    dplyr::summarise(mid_point = round(stats::median(as.integer(id)), 0)) %>%
    dplyr::mutate(w = paste0("Cluster ", w))

  if(!is.null(cluster_label)) {
    mid_points$w = factor(mid_points$w, labels = cluster_label)
  }

  data.frame(id = id, time = time, z = z) %>%
    dplyr::mutate(id = factor(id, levels = names(sort(w)))) %>%
    ggplot2::ggplot(ggplot2::aes(x=time, y=id, fill=factor(z))) +
    ggplot2::geom_tile(color="gray20") +
    viridis::scale_fill_viridis(discrete = T) +
    ggplot2::labs(x="Time", y="Id", fill=expression(Z[it])) +
    ggplot2::geom_hline(yintercept = which(unique(names(sort(w))) %in% cut_point) + 0.5,
                        color = "white", linewidth = 1) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0)) +
    model_change_gg +
    ggplot2::theme_minimal() +
    ggplot2::geom_text(
      data = mid_points,
      ggplot2::aes(x = max(time), y = mid_point, label = w),
      inherit.aes = FALSE,
      hjust = ifelse(is.null(label_hjust), -0.3, label_hjust),
      angle = 0
    ) +
    ggplot2::coord_cartesian(clip = 'off') +
    ggplot2::theme(
      legend.position = "top",
      plot.margin = ggplot2::margin(5, 60, 5, 5)
    )
}

plot_seq_include_mix = function(M,
                                w,
                                z,
                                id,
                                time,
                                z_sel_post_prob,
                                included = NULL,
                                spline_probs_sample = NULL,
                                map_new_class = NULL,
                                cluster_label = NULL,
                                label_hjust = -0.1) {

  n_time = length(unique(time))

  prob_stage_summ = spline_probs_sample %>%
    compute_post_stat() %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(time = rep(unique(time), times = M), w = rep(1:M, each = unique(n_time))) %>%
    gather(state, prob, -time, -w) %>%
    mutate(state = factor(state, labels = levels(z)))

  w_base = w

  if(!is.null(map_new_class)) {
    for(i in 1:length(map_new_class)) {
      w[w_base == i] = map_new_class[i]
    }

    w_base_summ = prob_stage_summ$w

    for(i in 1:length(map_new_class)) {
      prob_stage_summ$w[w_base_summ == i] = map_new_class[i]
    }
  }

  # Filter to included clusters after relabelling
  if(!is.null(included)) {
    keep_ids = names(w)[w %in% included]
    keep_rows = id %in% keep_ids
    id              = id[keep_rows]
    time            = time[keep_rows]
    z               = z[keep_rows]
    z_sel_post_prob = z_sel_post_prob[keep_rows]
    w               = w[w %in% included]
    prob_stage_summ = prob_stage_summ %>% dplyr::filter(w %in% included)
  }

  z_mode = prob_stage_summ %>%
    dplyr::group_by(time, w) %>%
    dplyr::summarise(z_mode = state[which.max(prob)], .groups = "drop") %>%
    dplyr::arrange(w, time)

  cluster_init_end = data.frame(w = w, id = names(w)) %>%
    dplyr::arrange(w) %>%
    dplyr::mutate(id = factor(id, levels = id)) %>%
    dplyr::arrange(w, id) %>%
    dplyr::group_by(w) %>%
    dplyr::summarise(min_id = min(as.integer(id)), max_id = max(as.integer(id)))

  # Create a variable for mode difference
  mode_change = z_mode %>%
    dplyr::group_by(w) %>%
    dplyr::mutate(mode_diff = c(0, z_mode %>% as.integer() %>% diff())) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::filter(mode_diff != 0) %>%
    dplyr::select(time, w) %>%
    dplyr::left_join(cluster_init_end, by = c("w"))

  model_change_gg = ggplot2::geom_segment(
    data = mode_change,
    ggplot2::aes(x = time - 0.5, xend = time - 0.5, y = min_id - 0.5, yend = max_id + 0.5),
    inherit.aes = F,
    color = "white", linetype = 1
  )

  id_label = id %>% unique()

  cut_point = data.frame(w = w, id = names(w)) %>%
    dplyr::mutate(id = id %>% factor(levels = id_label)) %>%
    dplyr::arrange(w, id) %>%
    dplyr::group_by(w) %>%
    dplyr::slice_tail(n = 1) %>%
    .$id

  mid_points = data.frame(w = w, id = names(w)) %>%
    dplyr::arrange(w) %>%
    dplyr::mutate(id = factor(id, levels = id)) %>%
    dplyr::group_by(w) %>%
    dplyr::summarise(mid_point = round(stats::median(as.integer(id)), 0)) %>%
    dplyr::mutate(w = paste0("Cluster ", w))

  if(!is.null(cluster_label)) {
    mid_points$w = factor(mid_points$w, labels = cluster_label)
  }

  data.frame(id = id, time = time, z = z) %>%
    dplyr::mutate(id = factor(id, levels = names(sort(w)))) %>%
    dplyr::mutate(state_prob = z_sel_post_prob) %>%
    ggplot2::ggplot(ggplot2::aes(x=time, y=id, fill=factor(z))) +
    ggplot2::geom_tile(color="gray20", aes(alpha = state_prob)) +
    ggplot2::labs(x="Time", y="Id", fill=expression(Z[it])) +
    ggplot2::geom_hline(yintercept = which(unique(names(sort(w))) %in% cut_point) + 0.5,
                        color = "white", linewidth = 1) +
    ggplot2::scale_alpha_continuous(
      range = c(0.3, 1),
      name = "State probability"
    ) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0)) +
    model_change_gg +
    ggplot2::theme_minimal() +
    ggplot2::geom_text(
      data = mid_points,
      ggplot2::aes(x = max(time), y = mid_point, label = w),
      inherit.aes = FALSE,
      hjust = ifelse(is.null(label_hjust), -0.3, label_hjust),
      angle = 0
    ) +
    ggplot2::coord_cartesian(clip = 'off') +
    ggplot2::theme(
      legend.position = "top",
      plot.margin = ggplot2::margin(5, 60, 5, 5)
    ) +
  ggplot2::guides(alpha = "none")
}
