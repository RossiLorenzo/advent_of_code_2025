# Input
input = readLines("Inputs/day8.txt")

# Explode & Clean
cross_joined = expand.grid(input, input, stringsAsFactors = F)
cross_joined = cross_joined[cross_joined$Var1 > cross_joined$Var2, ]
var1_s = strsplit(cross_joined$Var1, ",", fixed = T)
var2_s = strsplit(cross_joined$Var2, ",", fixed = T)
cross_joined$X1 = sapply(var1_s, function(x){ as.numeric(x[1]) })
cross_joined$Y1 = sapply(var1_s, function(x){ as.numeric(x[2]) })
cross_joined$Z1 = sapply(var1_s, function(x){ as.numeric(x[3]) })
cross_joined$X2 = sapply(var2_s, function(x){ as.numeric(x[1]) })
cross_joined$Y2 = sapply(var2_s, function(x){ as.numeric(x[2]) })
cross_joined$Z2 = sapply(var2_s, function(x){ as.numeric(x[3]) })

# Calculate Distances and Order
cross_joined$Distance = sqrt(
  (cross_joined$X1 - cross_joined$X2) ^ 2 +
  (cross_joined$Y1 - cross_joined$Y2) ^ 2 +
  (cross_joined$Z1 - cross_joined$Z2) ^ 2 
)
cross_joined = cross_joined[sort(cross_joined$Distance, index.return = T)$ix, ]

# Function 
do_one_step = function(circ, var1, var2){
  # Check if either Var1 or Var2 is already in a circuit
  var1_in_circuit = sapply(circ, function(x) { var1 %in% x })
  var2_in_circuit = sapply(circ, function(x) { var2 %in% x })
  # If they are on different circuits merge them
  if(any(var1_in_circuit) & any(var2_in_circuit)){
    if(which(var1_in_circuit) == which(var2_in_circuit))
      return(circ)
    circ[[which(var1_in_circuit)]] = c(
      circ[[which(var1_in_circuit)]],
      circ[[which(var2_in_circuit)]]
    )
    circ = circ[-which(var2_in_circuit)]
    return(circ)
  }
  # If one of the two nodes is in a circuit add the other one in too
  if(any(var1_in_circuit)){
    circ[[which(var1_in_circuit)]] = unique(c(
      circ[[which(var1_in_circuit)]], 
      cross_joined$Var2[i]
    ))
    return(circ)
  }
  if(any(var2_in_circuit)){
    circ[[which(var2_in_circuit)]] = unique(c(
      circ[[which(var2_in_circuit)]], 
      cross_joined$Var1[i]
    ))
    return(circ)
  }
  # Otherwise create a new circuit altogether
  circ = c(circ, list(c(cross_joined$Var1[i], cross_joined$Var2[i])))
  return(circ)
}

# Part 1
circuits = list(
  c(cross_joined[1,"Var1"], cross_joined[1,"Var2"])
)
for(i in 2:1000)
  circuits = do_one_step(circuits, cross_joined$Var1[i], cross_joined$Var2[i])
p1_l = sapply(circuits, length)
print(prod(sort(p1_l, decreasing = T)[1:3]))

# Part 2
i <<- 1000
while(length(unlist(circuits)) != length(input)){
  i <<- i+1
  circuits = do_one_step(circuits, cross_joined$Var1[i], cross_joined$Var2[i])
}
cross_joined[i,"X1"] * cross_joined[i,"X2"]
