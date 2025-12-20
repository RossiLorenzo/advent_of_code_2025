# Input
input = readLines("Inputs/day11.txt")

# Clean nodes
input_c = sapply(
  strsplit(input, ": ", fixed = T),
  function(x) {
    tmp = list(strsplit(x[2], " ", fixed = T)[[1]])
    names(tmp) = x[1]
    return(tmp)
  }
)

# Path Follower
follow_path = function(path, l, through = NULL) {
  current = path[length(path)]
  remaining = if(is.null(through)) "" else paste(sort(setdiff(through, path)), collapse = ",")
  key = paste(current, remaining, sep = "|")
  # Check if I'm looping
  if (length(path) >= 2) {
    if (path[length(path)] %in% path[1:(length(path) - 1)]) {
      return(NULL)
    }
  }
  # Have I been here already?
  if (exists(key, envir = memo_env)) {
    return(get(key, envir = memo_env))
  }
  # If we reached "out" return the path
  if (path[length(path)] == "out") {
    if (is.null(through)) {
      assign(key, 1, envir = memo_env)
      return(1)
    }
    assign(key, ifelse(all(through %in% path), 1, 0), envir = memo_env)
    return(ifelse(all(through %in% path), 1, 0))
  }
  # Otherwise go on all nodes
  count = 0
  possible_directions = l[[path[length(path)]]]
  for (n in possible_directions) {
    count = count + follow_path(c(path, n), l, through)
  }
  # Store result
  assign(key, count, envir = memo_env)
  return(count)
}

# Part1
memo_env = new.env(hash = T, parent = emptyenv())
print(follow_path("you", input_c))

# Part 2
memo_env = new.env(hash = T, parent = emptyenv())
print(format(follow_path("svr", input_c, c("dac", "fft")), scientific = F))
