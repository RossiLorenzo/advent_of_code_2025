# Input
input = readLines("Inputs/day6.txt")
input_s = strsplit(input, " ", fixed = T)
input_c = as.matrix(sapply(input_s, function(x){ x[which(x != "")]}), ncol = length(input_s))

# Part1
fs = apply(input_c, 1, function(x){ paste(x[-length(x)], collapse = x[length(x)])})
print(format(sum(sapply(fs, function(x){ eval(parse(text = x)) })), scientific = F))

# Re-split input since spaces are important now.
input_s = strsplit(input, "", fixed = T)
operation = input_s[[length(input_s)]]
operation_c = operation[operation != " "]
input_c = matrix(unlist(input_s[-length(input_s)]), byrow = T, nrow = length(input_s)-1)

# Find empty separators
sep = c(0, which(apply(input_c, 2, function(x){ all(x == " ") })), ncol(input_c)+1)

# Calculate Formulas
fs = NULL
for(i in 1:(length(sep)-1)){
  tmp = input_c[,(sep[i]+1):(sep[i+1]-1)]
  fs = c(fs, paste0(apply(tmp, 2, function(x){ paste0(x[x != " "], collapse = "")}), collapse = operation_c[i]))
}

# Part 2
print(format(sum(sapply(fs, function(x){ eval(parse(text = x)) })), scientific = F))
