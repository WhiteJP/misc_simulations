library(tidyverse)
library(pROC)

# simulate data for normative data and case, assuming cases are 
norm <- rnorm(10000)
case <- rnorm(10000, mean = -1.6, sd = 1.4)

dat <- data.frame(norm, case) %>% 
  pivot_longer(cols = everything(), names_to = "group", values_to = "score")


rocobj <- roc(group ~ score, dat, 
              levels = c("norm", "case"))

# function for finding optimal threshold based on cost-benefit and prevalence
# using prOC::coords
optimal_threshold <- function(rocobj, prevalence, relcostFN){
  coords(rocobj, x = "best", best.method = "youden",  
         best.weights = c(relcostFN, prevalence),
         ret = c("threshold", "sensitivity", "specificity"))[1, ]
}

# get grid of possibilities
tab <- expand.grid(prevalence = c(0.1, 0.25, 0.5),
                   relcostFN = c(1/4, 1/2, 2/3, 1, 3/2, 2, 4))

# calculate optimal thresholds
tab <- tab %>% 
  mutate(
   optimal = map2_dfr(.x = prevalence, .y = relcostFN, 
           .f = ~ optimal_threshold(rocobj, prevalence = .x, relcostFN = .y))
  ) %>% 
  unnest(optimal, names_sep = ".")

# make (flex)table to view results 
flextable::set_flextable_defaults(digits = 3)

tab %>% 
  rename(
    `Prevalence in Population` = prevalence, 
    `Cost of FN / Cost of FP`= relcostFN,
    `Optimal_Threshold` = optimal.threshold,
    `Optimal_Sensitivity` = optimal.sensitivity,    
    `Optimal_Specificity` = optimal.specificity
  ) %>% 
  flextable::flextable() %>% 
  ftExtra::span_header() %>% 
  flextable::align(align = "center", part = "all")
  
