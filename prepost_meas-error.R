n <- 2000
mean <- 80
pre_sd <- 30
post_sd <- 15

nsims <- 10000
mean_diff <- numeric(nsims)

for (i in 1:nsims) {
  pre <- jpw::censor_both(
    rnorm(n, mean, pre_sd),
    50, 
    100
  )
  post <-jpw::censor_both(
    rnorm(n, mean, post_sd),
    0, 
    100
  )
  mean_diff[i] <- mean(post - pre)

}

hist(mean_diff)
