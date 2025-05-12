compute_ahp_weights <- function(mat) {
  ev <- eigen(mat)
  weights <- Re(ev$vectors[, 1])
  weights / sum(weights)
}
