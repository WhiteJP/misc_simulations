## Monty Hall simulation

# setup
nsims <- 1e5
outcomes <- c("goat", "goat", "car")
stay_result <- switch_result <- logical(nsims) 

for (i in seq(nsims)) {
  doors <- sample(outcomes, 3) # shuffle outcomes behind doors
  choice <- sample.int(3, 1) # Player chooses a door
  
  # Monty opens a door with a goat
  goats_left <- setdiff(which(doors == "goat"), choice) #index of goats left
  if(length(goats_left) == 1) {
    doors[goats_left] <- NA # if one goat left open that door
  } else {
    doors[sample(goats_left, 1)] <- NA # if two goats randomly open one
  }
  
  # result
  stay_result[i] <- doors[choice] == "car" 
  switch_result[i] <- doors[!is.na(doors) & seq_along(doors) != choice] == "car"
}

# results
mean(stay_result)
mean(switch_result)
