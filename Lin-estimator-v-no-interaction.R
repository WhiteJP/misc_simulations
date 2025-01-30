## Lin estimator, with v without interaction

pre <- rnorm(1000, 25, 7.5)
treat <- sample(c("control", "treatment"), 1000, replace = TRUE)

# add a treatment effect that depends on pre score for treatment group
post <- ifelse(treat == "control", pre, pre + pre*0.8) + rnorm(1000, 0, 5)
change <- post - pre

#zscore pre
zpre <- (pre - mean(pre))/sd(pre)

#plot
library(ggplot2)
d <- data.frame(zpre = zpre, post = post, change = change, treat = treat)
d$treat <- factor(d$treat, levels = c("control", "treatment"))

ggplot(d, aes(x = zpre, y = change, color = treat)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) 

ggplot(d, aes(x = zpre, y = change, color = treat)) + 
  geom_point() + 
  moderndive::geom_parallel_slopes()

#models
lm(change ~ zpre + treat, data = d) |> summary()
lm(change ~ zpre*treat, data = d) |> summary()

#Ran a quick simulation or two after our discussion about interaction v no interaction. 

#Tldr; ATE estimate is remarkably close between the two methods (although not identical) 
#but decent variance gain for including interaction (in cases where there really is an interaction #between pre score and treatment, which is normally the case for our stuff). 