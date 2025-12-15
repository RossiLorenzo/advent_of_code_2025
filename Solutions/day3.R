# Input
input = readLines("Inputs/day3.txt")

# Function to find the top numbers in order and the max Joltage
max_joltage = function(x, steps){
  x_s = as.numeric(strsplit(x, "", fixed = T)[[1]])
  digits = NULL
  for(i in 1:steps){
    max_n = (length(x_s)-(steps-i))
    max_pos = min(which(x_s[1:max_n] == max(x_s[1:max_n])))
    digits = c(digits, x_s[max_pos])
    x_s = x_s[(max_pos+1):length(x_s)]
  }
  return(sum(digits * sapply((steps-1):0, function(y){ 10^y })))
}

# Part 1
format(sum(sapply(input, max_joltage, 2)), scientific = F)
# Part 2
format(sum(sapply(input, max_joltage, 12)), scientific = F)
