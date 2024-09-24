## simulations to see whether sampling every second state from a markov chain is also markovian
s0 <- 0
s <- s0
for (i in 1:10000) {
  s <- c(s, s[length(s)] + 1 + rnorm(1, 0, .25))
}


# lets get partial linear correlations and see if they are zero for lag2

## for original chain
library(ppcor)
lag1<- dplyr::lag(s, 1)
lag2 <- dplyr::lag(s, 2)
d <- cbind(s, lag1, lag2)
pcor(d[3:nrow(d),])


## for sampling every second chain
s_half <- s[c(TRUE, FALSE)]
lag1<- dplyr::lag(s_half, 1)
lag2 <- dplyr::lag(s_half, 2)
d <- cbind(s_half, lag1, lag2)
pcor(d[3:nrow(d),])


