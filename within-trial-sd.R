library(tidyverse)

# function ----------------------------------------------------------------
xnorms <- function(n_ps, n_obs) {
    #make matrix with each column being a participant 
    # and each row being an observation
    mat <- matrix(NA, nrow = n_ps, ncol = n_obs)
    
    #populate matrix with indep norm 
    for(i in 1:n_ps) {
      mat[i, ] <- rnorm(n_obs)
    }
    return(mat)
}

# simulations -------------------------------------------------------------
sims <-  tibble(
  n_ps    = c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 50000),
  n_obs   = rep(35, length(n_ps)),
  data    = map2(n_ps, n_obs, xnorms),
  sd      = map(data, ~ apply(.x, 1, sd)),
  sd_mean = map_dbl(sd, mean),
  sd_sd   = map_dbl(sd, stats::sd),
  sd_se   = sd_sd/sqrt(n_obs),
  sd_min  = map_dbl(sd, min),
  sd_p05  = map_dbl(sd, ~ quantile(.x, probs = .05)),
  sd_Q1   = map_dbl(sd, quantile, probs = .25),
  sd_mdn  = map_dbl(sd, quantile, .50),
  sd_Q3   = map_dbl(sd, quantile, .75),
  sd_p95  = map_dbl(sd, quantile, .95),
  sd_max  = map_dbl(sd, max)
) 

# Histograms --------------------------------------------------------------
sims %>%
  unnest(sd) %>%  
  ggplot(aes(x = sd)) +
  geom_histogram(fill = "skyblue", col = "black") + 
  geom_vline(aes(xintercept = sd_mean)) +
  facet_wrap(~n_ps, scales = "free_y")

