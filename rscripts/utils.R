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
