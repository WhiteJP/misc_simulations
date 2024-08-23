library(faux) # for `rnorm_multi()` to sim correlated multivar norms
library(tidyverse)

# get table with different rs          
 tab <- tibble(
  r = c(-0.99, seq(-0.8, 0.8, 0.2), .99)
)
 
# function to sim correlated normal dists
 sim_correlated_norms <- function(r, n = 1e6, n_z = 2) {
   
  tab <- rnorm_multi(
    n = n, 
    mu = rep(0, n_z),
    sd = rep(1, n_z),
    r = r, 
    varnames = LETTERS[1:n_z],
    empirical = FALSE)
  
  # tab %>% 
  #   mutate(
  #     comp = (A + B)/2
  #   )
  
  tab$comp <- rowMeans(tab)

  tab
               
 }

# runs sims. 
z_sims <- tab %>% 
  mutate(
    data = map(.x = r, sim_correlated_norms, n = 1e5, n_z = 3),
    #emp_r = map_dbl(.x = data, ~ cor(.x$A, .x$B)),
    mean = map_dbl(.x = data, .f = ~ mean(.x$comp)),   
    sd = map_dbl(.x = data, .f = ~ sd(.x$comp))                  
  )

z_sims
