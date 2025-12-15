# Input
input = readLines("Inputs/day5.txt")
ranges = input[1:(which(input == "")-1)]
ingredients = as.numeric(input[(which(input == "")+1):length(input)])

# Split Ranges
ranges_s = strsplit(ranges, "-", fixed = T)
ranges_l = sapply(ranges_s, function(x){ as.numeric(x[1]) })
ranges_u = sapply(ranges_s, function(x){ as.numeric(x[2]) })

# Part1: Count fresh ingredients
count_fresh_ingredients = 0
for(i in ingredients){
  for(r in 1:length(ranges)){
    if(i >= ranges_l[r] & i <= ranges_u[r]){
      count_fresh_ingredients = count_fresh_ingredients + 1  
      break()
    }
  }
}
print(count_fresh_ingredients)

# Sort ranges
ranges_l_sorted = ranges_l[sort(ranges_l, index.return = T)$ix]
ranges_u_sorted = ranges_u[sort(ranges_l, index.return = T)$ix]
comined_ranges_df = data.frame(
  lower = ranges_l_sorted[1],
  upper = ranges_u_sorted[1]
)
# Combine ranges
for(i in 2:length(ranges)){
  if(ranges_l_sorted[i] > comined_ranges_df[nrow(comined_ranges_df), "upper"]){
    comined_ranges_df = rbind(comined_ranges_df, data.frame(
      lower = ranges_l_sorted[i],
      upper = ranges_u_sorted[i]
    ))
  } else{
    comined_ranges_df[nrow(comined_ranges_df), "upper"] = max(c(
      ranges_u_sorted[i],
      comined_ranges_df[nrow(comined_ranges_df), "upper"]
    ))
  }
}
# Part2
print(format(sum(comined_ranges_df$upper - comined_ranges_df$lower + 1), scientific = F))
