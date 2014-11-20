EBICgraph <- function(
  S, # Sample covariance matrix
  adj, # adjacency matrix of undirected graph
  n, # Sample size
  gamma = 0.5)
{
  stopifnot(isSymmetric(adj))
  
  # Check for positive definite:
  if(any(eigen(S)$values < 0))  {
    if (n > nrow(S))
    {
      warning("Correlation/covariance matrix is not positive definite, yet sample size is higher than number of variables. Finding nearest positive definite matrix")
      S <- as.matrix(Matrix::nearPD(S, keepDiag = TRUE, ensureSymmetry = TRUE)$mat)         
    } else stop("Correlation/covariance matrix is not positive definite, and sample size is lower than or equal to the number of variables.")
  }
  
  diag(adj) <- 1
  
  # Compute zeroes:
  zeroes <- which(adj==0,arr.ind=TRUE)
  
  # Fit network:
  if (nrow(zeroes)>0)  res <- glasso::glasso(S, 0, zero=zeroes) else res <- glasso(S, 0)

  # Compute EBIC:
  C <- res$wi
#   L <- n/2 * (log(det(C)) - sum(diag(S %*% C)))
#   L <- logGaus (S, C, n)
#   E <- sum(C[lower.tri(C,diag=TRUE)] != 0)
#   p <- nrow(C)
#   
#   # return EBIC:
#   -2 * L + E * log(n) + 4 * E * gamma * log(p)  
  EBIC(S, C, n, gamma)
}
