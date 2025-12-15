# Input
input = readLines("Inputs/day9.txt")
input_y = as.numeric(sapply(strsplit(input, ",", fixed = T), function(x){ x[1] }))
input_x = as.numeric(sapply(strsplit(input, ",", fixed = T), function(x){ x[2] }))

# Explode & Clean
cross_joined = expand.grid(input, input, stringsAsFactors = F)
cross_joined = cross_joined[cross_joined$Var1 > cross_joined$Var2, ]
var1_s = strsplit(cross_joined$Var1, ",", fixed = T)
var2_s = strsplit(cross_joined$Var2, ",", fixed = T)
cross_joined$Y1 = sapply(var1_s, function(x){ as.numeric(x[1]) })
cross_joined$X1 = sapply(var1_s, function(x){ as.numeric(x[2]) })
cross_joined$Y2 = sapply(var2_s, function(x){ as.numeric(x[1]) })
cross_joined$X2 = sapply(var2_s, function(x){ as.numeric(x[2]) })

# Part1
cross_joined$max_rect_area = (
  (abs(cross_joined$X2 - cross_joined$X1) + 1) *
    (abs(cross_joined$Y2 - cross_joined$Y1) + 1)
)
max(cross_joined$max_rect_area)

# Rescale to 1-> Max
input_x_r = input_x - min(input_x) + 1
input_y_r = input_y - min(input_y) + 1
cross_joined$X1 = cross_joined$X1 - min(cross_joined$X1) + 1
cross_joined$X2 = cross_joined$X2 - min(cross_joined$X2) + 1
cross_joined$Y1 = cross_joined$Y1 - min(cross_joined$Y1) + 1
cross_joined$Y2 = cross_joined$Y2 - min(cross_joined$Y2) + 1

# Set all red&green tiles
input_x_r = c(input_x_r, input_x_r[1])
input_y_r = c(input_y_r, input_y_r[1])
points_by_row = vector("list", max(input_x_r))
for(i in 2:length(input_x_r)){
  # Horizontal - Save first and last 
  if(input_x_r[i] == input_x_r[i-1]){
    w_new_pts = c(points_by_row[[input_x_r[i]]], input_y_r[i-1], input_y_r[i])
    points_by_row[[input_x_r[i]]] = c(min(w_new_pts), max(w_new_pts))
  }
  # Vertical - Save the point
  else{
    for(j in seq(input_x_r[i], input_x_r[i-1])){
      w_new_pts = c(points_by_row[[j]], input_y_r[i])
      points_by_row[[j]] = c(min(w_new_pts), max(w_new_pts))
    }
  }
}
points_by_row_x = sapply(points_by_row, function(x){ x[1]} )
points_by_row_y = sapply(points_by_row, function(x){ x[2]} )

# Part2
max_area = 0
for(i in 1:nrow(cross_joined)){
  min_t = max(points_by_row_x[seq(cross_joined$X1[i], cross_joined$X2[i])])
  max_t = min(points_by_row_y[seq(cross_joined$X1[i], cross_joined$X2[i])])
  if(
    cross_joined$Y1[i] >= min_t & cross_joined$Y1[i] <= max_t &
    cross_joined$Y2[i] >= min_t & cross_joined$Y2[i] <= max_t
  )
    max_area = max(c(max_area, cross_joined$max_rect_area[i]))
}
print(max_area)
