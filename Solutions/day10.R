# Input
input = readLines("Inputs/day10.txt")
input_ss = strsplit(input, " ", fixed = T)

# Clean configurations
lights_needed_on = sapply(input_ss, function(x) {
  x = gsub("\\[|\\]", "", x[1])
  res = rep(0, nchar(x))
  res[which(strsplit(x, "", fixed = T)[[1]] == "#")] = 1
  return(res)
})
buttons_available = sapply(1:length(input_ss), function(i) {
  x = input_ss[[i]]
  x_ss = strsplit(gsub("\\(|\\)", "", x[-c(1, length(x))]), ",", fixed = T)
  res = rep(0, length(lights_needed_on[[i]]))
  lapply(x_ss, function(y) {
    res[as.numeric(y) + 1] = 1
    return(res)
  })
})
targets_joltage = lapply(input_ss, function(x) {
  x = gsub("\\{|\\}", "", x[length(x)])
  as.numeric(strsplit(x, ",")[[1]])
})

# Part 1 Function
push_one_button = function(config, button) {
  (config + button) %% 2
}

# Part 1 Execution
steps_needed = NULL
for (i in 1:length(input)) {
  found = F
  step = 1
  available_buttons = buttons_available[[i]]
  available_configs = available_buttons
  final_config = lights_needed_on[[i]]

  if (any(sapply(available_configs, function(x) {
    all(x == final_config)
  }))) {
    found = T
  }

  while (!found) {
    step = step + 1
    available_configs = unique(c(
      available_configs,
      sapply(available_configs, function(config) {
        lapply(available_buttons, function(button) {
          push_one_button(config, button)
        })
      })
    ))
    found = any(sapply(available_configs, function(x) {
      all(x == final_config)
    }))
  }
  steps_needed = c(steps_needed, step)
}
print(sum(steps_needed))

# Part 2 Function
solve_machine = function(A, b) {
  num_vars = ncol(A)

  # Bounds
  upper_bounds = numeric(num_vars)
  for (j in 1:num_vars) {
    idx = which(A[, j] > 0)
    if (length(idx) == 0) {
      upper_bounds[j] = 0
    } else {
      upper_bounds[j] = min(floor(b[idx] / A[idx, j]))
    }
  }

  if (any(upper_bounds < 0)) {
    return(Inf)
  }

  # QR Decomposition
  q_dec = qr(A)
  r = q_dec$rank

  if (r == 0) {
    if (all(b == 0)) {
      return(0)
    } else {
      return(Inf)
    }
  }

  pivots = q_dec$pivot[1:r]
  free_vars = integer(0)
  if (r < num_vars) free_vars = q_dec$pivot[(r + 1):num_vars]

  B = A[, pivots, drop = F]
  F_mat = A[, free_vars, drop = F]

  q_rows = qr(t(B))
  row_pivots = q_rows$pivot[1:r]

  B_sub = B[row_pivots, , drop = F]
  F_sub = F_mat[row_pivots, , drop = F]
  b_sub = b[row_pivots]

  # Check solution
  check_solution = function(x_free_batch) {
    sol_base = solve(B_sub, b_sub)
    if (length(free_vars) > 0) {
      sol_coeffs = solve(B_sub, F_sub)
      x_basic_mat = sol_base - sol_coeffs %*% x_free_batch
    } else {
      x_basic_mat = matrix(sol_base, ncol = 1)
    }

    tol = 1e-9
    x_basic_rounded = round(x_basic_mat)

    is_integer = colSums(abs(x_basic_mat - x_basic_rounded)) < tol
    if (!any(is_integer)) {
      return(Inf)
    }

    is_non_neg = colSums(x_basic_rounded < 0) == 0
    valid_indices = which(is_integer & is_non_neg)
    if (length(valid_indices) == 0) {
      return(Inf)
    }

    candidates_basic = x_basic_rounded[, valid_indices, drop = F]

    # Reconstruct
    full_x = matrix(0, nrow = num_vars, ncol = length(valid_indices))
    full_x[pivots, ] = candidates_basic
    if (length(free_vars) > 0) {
      candidates_free = x_free_batch[, valid_indices, drop = F]
      full_x[free_vars, ] = candidates_free
    }

    residuals = colSums(abs(A %*% full_x - b))
    is_exact_sol = residuals < tol

    if (!any(is_exact_sol)) {
      return(Inf)
    }

    valid_full_x = full_x[, is_exact_sol, drop = F]
    return(min(colSums(valid_full_x)))
  }

  if (length(free_vars) == 0) {
    return(check_solution(matrix(0, nrow = 0, ncol = 1)))
  } else {
    bounds_free = upper_bounds[free_vars]
    ranges = lapply(bounds_free, function(u) 0:u)
    grid_df = expand.grid(ranges)
    x_free_mat = t(as.matrix(grid_df))
    return(check_solution(x_free_mat))
  }
}

# Part 2 Execution
part2_sum = 0
for (i in 1:length(input)) {
  A = do.call(cbind, buttons_available[[i]])
  b = targets_joltage[[i]]
  res = solve_machine(A, b)
  if (!is.infinite(res)) part2_sum = part2_sum + res
}
print(part2_sum)
