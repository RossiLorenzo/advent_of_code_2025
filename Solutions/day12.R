# Input
input <- readLines("Inputs/day12.txt")

# Parsing Logic
blocks <- list()
current_block <- c()
for (line in input) {
  if (line == "") {
    if (length(current_block) > 0) {
      blocks[[length(blocks) + 1]] <- current_block
      current_block <- c()
    }
  } else {
    current_block <- c(current_block, line)
  }
}
if (length(current_block) > 0) blocks[[length(blocks) + 1]] <- current_block

shape_blocks <- list()
query_lines <- c()

for (b in blocks) {
  if (grepl("^[0-9]+:", b[1])) {
    shape_blocks[[length(shape_blocks) + 1]] <- b
  } else {
    query_lines <- c(query_lines, b)
  }
}

# Parse Shapes
shapes <- lapply(shape_blocks, function(blk) {
  grid_lines <- blk[-1]
  mat <- do.call(rbind, strsplit(grid_lines, ""))
  coords <- which(mat == "#", arr.ind = TRUE)
  coords[, 1] <- coords[, 1] - min(coords[, 1])
  coords[, 2] <- coords[, 2] - min(coords[, 2])
  return(coords)
})
shape_sizes <- sapply(shapes, nrow)

# Transformation
rotate_coords <- function(coords) {
  new_coords <- coords
  new_coords[, 1] <- coords[, 2]
  new_coords[, 2] <- -coords[, 1]
  new_coords[, 1] <- new_coords[, 1] - min(new_coords[, 1])
  new_coords[, 2] <- new_coords[, 2] - min(new_coords[, 2])
  new_coords <- new_coords[order(new_coords[, 1], new_coords[, 2]), , drop = FALSE]
  return(new_coords)
}

flip_coords <- function(coords) {
  new_coords <- coords
  new_coords[, 2] <- -coords[, 2]
  new_coords[, 1] <- new_coords[, 1] - min(new_coords[, 1])
  new_coords[, 2] <- new_coords[, 2] - min(new_coords[, 2])
  new_coords <- new_coords[order(new_coords[, 1], new_coords[, 2]), , drop = FALSE]
  return(new_coords)
}

generate_variants <- function(base_shape) {
  vars <- list()
  curr <- base_shape
  for (i in 1:4) {
    vars[[length(vars) + 1]] <- curr
    curr <- rotate_coords(curr)
  }
  curr <- flip_coords(base_shape)
  for (i in 1:4) {
    vars[[length(vars) + 1]] <- curr
    curr <- rotate_coords(curr)
  }

  unique_vars <- list()
  hashes <- c()

  for (v in vars) {
    v <- v[order(v[, 1], v[, 2]), , drop = FALSE]
    h <- paste(as.vector(t(v)), collapse = ",")
    if (!h %in% hashes) {
      hashes <- c(hashes, h)
      unique_vars[[length(unique_vars) + 1]] <- v
    }
  }
  return(unique_vars)
}

all_shape_variants <- lapply(shapes, generate_variants)

queries <- lapply(query_lines, function(line) {
  parts <- strsplit(line, ": ")[[1]]
  dims <- as.numeric(strsplit(parts[1], "x")[[1]])
  counts <- as.numeric(strsplit(parts[2], " ")[[1]])
  to_fit <- c()
  for (i in seq_along(counts)) {
    if (counts[i] > 0) {
      to_fit <- c(to_fit, rep(i - 1, counts[i]))
    }
  }
  return(list(w = dims[1], h = dims[2], shapes = to_fit))
})

solve_region <- function(w, h, shape_indices) {
  print(paste("Checking region", w, "x", h, "Loading..."))

  total_area <- w * h
  # Initial Check
  needed_area <- sum(shape_sizes[shape_indices + 1])
  if (needed_area > total_area) {
    return(FALSE)
  }

  # Sort optimization
  shapes_data <- lapply(shape_indices, function(i) {
    list(id = i, size = nrow(shapes[[i + 1]]))
  })
  sizes <- sapply(shapes_data, function(x) x$size)
  shapes_data <- shapes_data[order(sizes, decreasing = TRUE)]
  sorted_indices <- sapply(shapes_data, function(x) x$id)

  grid <- matrix(0, nrow = h, ncol = w)

  solve_fill <- function(mtx, remaining) {
    # If no shapes left, we succeeded!
    if (length(remaining) == 0) {
      return(TRUE)
    }

    # Pruning: Check sufficient space
    current_rem_area <- sum(shape_sizes[remaining + 1])
    current_empty_slots <- sum(mtx == 0)
    if (current_empty_slots < current_rem_area) {
      return(FALSE)
    }

    # Find fill target
    empty_idx <- which(mtx == 0)[1]
    if (is.na(empty_idx)) {
      return(TRUE)
    } # Should not be reached due to area check, but safe

    dst_r <- (empty_idx - 1) %% nrow(mtx) + 1
    dst_c <- (empty_idx - 1) %/% nrow(mtx) + 1

    # Strategy 1: Cover this slot with a shape
    unique_rem_indices <- unique(remaining)
    for (type_idx in unique_rem_indices) {
      variants <- all_shape_variants[[type_idx + 1]]

      for (v_idx in seq_along(variants)) {
        v <- variants[[v_idx]]

        # Try to align every cell of variant `v` to `(dst_r, dst_c)`
        for (k in 1:nrow(v)) {
          dr <- dst_r - v[k, 1]
          dc <- dst_c - v[k, 2]

          prop_r <- v[, 1] + dr
          prop_c <- v[, 2] + dc

          # Bounds
          if (any(prop_r < 1 | prop_r > nrow(mtx) | prop_c < 1 | prop_c > ncol(mtx))) next

          # Overlap
          prop_lin <- prop_r + (prop_c - 1) * nrow(mtx)
          if (any(mtx[prop_lin] == 1)) next

          # Place
          new_mtx <- mtx
          new_mtx[prop_lin] <- 1

          rem_pos <- match(type_idx, remaining)
          new_remaining <- remaining[-rem_pos]

          if (solve_fill(new_mtx, new_remaining)) {
            return(TRUE)
          }
        }
      }
    }

    # Strategy 2: Skip this slot (leave empty)
    # Only if we have slack
    if (current_empty_slots > current_rem_area) {
      new_mtx <- mtx
      new_mtx[empty_idx] <- 1 # Mark as filled (actually skipped)
      if (solve_fill(new_mtx, remaining)) {
        return(TRUE)
      }
    }

    return(FALSE)
  }

  return(solve_fill(grid, sorted_indices))
}

results <- 0
for (i in seq_along(queries)) {
  q <- queries[[i]]
  if (solve_region(q$w, q$h, q$shapes)) {
    results <- results + 1
  }
}

print(results)
