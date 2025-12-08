# Input
input = readLines("Inputs/day7_1.txt")
input_s = strsplit(input, "", fixed = T)
input_c = matrix(unlist(input_s), byrow = T, nrow = length(input_s))

# Do 1 step
single_step = function(m, pos){
  pos_x = pos[1]
  pos_y = pos[2]
  # Check if we are at the bottom of the matrix
  if(pos_x == nrow(m)){
    return(list(
      completed = T,
      m = m,
      pos = list(pos),
      split = 0
    ))
  }
  # Check if one down is a split
  split = 0
  if(m[pos_x+1, pos_y] == "^"){
    if(pos_y-1 > 0 && m[pos_x+1, pos_y-1] == "."){
      m[pos_x+1, pos_y-1] = "|"
      split = split + 1
    }
    if(pos_y-1 > 0 && m[pos_x+1, pos_y+1] == "."){
      m[pos_x+1, pos_y+1] = "|"
      split = split + 1
    }
    return(list(
      completed = F,
      m = m,
      pos = list(c(pos_x+1, pos_y-1), c(pos_x+1, pos_y+1)),
      splits = split
    ))
  }
  # Otherwise just go down by one
  if(m[pos_x+1, pos_y] == ".")
    m[pos_x+1, pos_y] = "|"
  return(list(
    completed = F,
    m = m,
    pos = list(c(pos_x+1, pos_y)),
    splits = 0
  ))
}

# Run down the matrix and calculate splits
pos = list(c(1, which(input_s[[1]] == "S")))
m = input_c
completed = F
splits = NULL
while(!all(completed)){
  completed = NULL
  next_pos = NULL
  for(i in 1:length(pos)){
    ns = single_step(m, pos[[i]])
    completed = c(completed, ns$completed)
    m = ns$m
    next_pos = c(next_pos, ns$pos)
    splits = c(splits, ns$splits)
  }
  pos = unique(next_pos)
}

# Part 1
print(sum(splits != 0))

# Recursive search with memoization
memo_env = new.env(hash = T, parent = emptyenv())

recursive_search_fast = function(m, pos){
  # Input
  pos_x = pos[1]
  pos_y = pos[2]
  # Check memoization
  key = paste(pos_x, pos_y, sep = ",")
  if(exists(key, envir = memo_env)){
    return(get(key, envir = memo_env))
  }
  
  # Check if we are at the bottom of the matrix
  if(pos_x == nrow(m)){
    return(1)
  }
  
  # Calculate paths
  count = 0
  target = m[pos_x+1, pos_y]
  
  if(target == "^"){
    # Splitter: try left and right
    if(pos_y-1 > 0 && m[pos_x+1, pos_y-1] == "."){
      count = count + recursive_search_fast(m, c(pos_x+1, pos_y-1))
    }
    if(pos_y+1 <= ncol(m) && m[pos_x+1, pos_y+1] == "."){
      count = count + recursive_search_fast(m, c(pos_x+1, pos_y+1))
    }
  } else if(target == "."){
    # Open space: go down
    count = count + recursive_search_fast(m, c(pos_x+1, pos_y))
  }
  
  # Store result
  assign(key, count, envir = memo_env)
  return(count)
}

# Part 2
# Clear memoization environment
rm(list = ls(memo_env), envir = memo_env)
start_pos = c(1, which(input_s[[1]] == "S"))
print(format(recursive_search_fast(input_c, start_pos), scientific = F))


