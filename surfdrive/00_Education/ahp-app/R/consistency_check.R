get_random_index <- function(n) {
  # Saaty's RI table
  RI <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45)
  RI[n]
}

compute_consistency_ratio <- function(mat) {
  n <- nrow(mat)
  ev <- eigen(mat)
  lambda_max <- Re(ev$values[1])
  ci <- (lambda_max - n) / (n - 1)
  ri <- get_random_index(n)
  if (ri == 0) return(0)
  ci / ri
}
