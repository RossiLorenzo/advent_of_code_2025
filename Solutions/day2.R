# Input
input = readLines("Inputs/day2.txt")
input_s = strsplit(input, ",", fixed = T)[[1]]
input_ss = strsplit(input_s, "-", fixed = T)

# Function to evaluate if a number is valid
is_valid = function(x){
  x_s = strsplit(as.character(x), "")[[1]]
  if(x_s[1] == "0")
    return(x)
  if(length(x_s) %% 2 == 1)
    return(0)
  if(
    paste0(x_s[1:(length(x_s)/2)], collapse = "") != 
    paste0(x_s[((length(x_s)/2)+1):(length(x_s))], collapse = "")
  )
    return(0)
  return(x)
}

# Part 1
ll = lapply(input_ss, function(x){ seq(as.numeric(x[1]), as.numeric(x[2]))})
ll_is_valid = lapply(ll, function(x){ sapply(x, is_valid) })
sum(unlist(ll_is_valid))

# Function to evaluate if a number is valid - Part 2
is_valid_p2 = function(x){
  x_s = strsplit(as.character(x), "")[[1]]
  for(i in 1:ceiling(length(x_s)/2)){
    if(i == length(x_s))
      next()
    if(as.character(x) == paste0(rep(x_s[1:i], length(x_s)/i), collapse = ""))
      return(x)
  }
  return(0)
}

# Part 2
ll_is_valid_p2 = lapply(ll, function(x){ sapply(x, is_valid_p2) })
format(sum(unlist(ll_is_valid_p2)), scientific = F)
