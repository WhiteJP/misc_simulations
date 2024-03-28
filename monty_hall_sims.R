## Monty Hall simluation

nsims <- 100000
outcomes <- c("goat", "goat", "car")
stay_result <- switch_result <- logical(nsims)
for (i in seq(nsims)) {
  doors <- sample(outcomes, 3)
  choice <- sample.int(3, 1)
  
  goats_left <- setdiff(which(doors == "goat"), choice)
  if(length(goats_left) == 1) {
    doors[goats_left] <- NA
  } else {
    doors[sample(goats_left, 1)] <- NA
  }
  
  stay_result[i] <- doors[choice] == "car"
  switch_result[i] <- doors[!is.na(doors) & seq_along(doors) != choice] == "car"
}

mean(stay_result)
mean(switch_result)
