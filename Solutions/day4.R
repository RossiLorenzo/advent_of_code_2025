# Input
input = readLines("Inputs/day4.txt")
input_s = strsplit(input, "", fixed = T)
input_m = matrix(unlist(input_s), byrow = T, nrow = length(input_s))

# Function to run 1 loop of removal
remove_paper = function(m){
  for(i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      min_i = max(c(1, i-1))
      max_i = min(c(nrow(m), i+1))
      min_j = max(c(1, j-1))
      max_j = min(c(ncol(m), j+1))
      m_subset = m[(min_i:max_i), (min_j:max_j)]
      if(sum(m_subset %in% c("@", "R")) <= 4 & m[i, j] == "@"){
        m[i, j] = "R"
      }
    }
  }
  to_rep = which(m == "R")
  if(length(to_rep) == 0){
    return(m)
  }
  m[to_rep] = rep(".", length(to_rep))
  return(m)
}

# Part 1
m2 = remove_paper(input_m)
sum(input_m == "@") - sum(m2 == "@")

# Part 2
keep_checking = TRUE
m = input_m
while(keep_checking){
  m2 = remove_paper(m)
  keep_checking = !all(m2 == m)
  m = m2
}
sum(input_m == "@") - sum(m2 == "@")
