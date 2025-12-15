# Input
input = readLines("Inputs/day1.txt")
input_ss = strsplit(input, "", fixed = T)

# Function to move the dial
move_dial = function(start_pos, i){
  # Parse direction and step
  direction = ifelse(i[1] == "R", 1, -1)
  step = as.numeric(paste0(i[-1], collapse = ""))
  # Move
  new_pos = start_pos + direction*step
  # Position Adjusted
  new_pos_adj = new_pos
  if(new_pos < 0){
    new_pos_adj = 100 + sign(new_pos) * abs(new_pos) %% 100
  }
  if(new_pos_adj >= 100){
    new_pos_adj = new_pos %% 100
  }
  # Times through zero
  sequence = seq(min(c(start_pos, new_pos)), max(c(start_pos, new_pos)))
  zeros = sum(sequence%%100==0) - sum(start_pos == 0)
  # Return results
  return(list(
    pos = new_pos_adj, 
    instruction = paste(i, collapse = ""),
    zeros = zeros
  ))
}

# Starting point
latest_pos = 50
history = list()

# Run all movements
for(j in 1:length(input_ss)){
  m = move_dial(latest_pos, input_ss[[j]])
  latest_pos = m[["pos"]]
  history[[j]] = m
}

# Part 1
sum(sapply(history, function(x){ x$pos == 0} ))
# Part 2
sum(sapply(history, function(x){ x$zeros} ))

