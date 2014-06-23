# Simply computes the Gaussian log likelihood given sample covariance and estimate of precision:
logGaus <- function(S,K,n)
{
  SK = S %*% K
  tr = function(A) sum(diag(A))
  n/2 * (log(det(K)) - tr(SK))
}

# Computes the EBIC:
EBIC <- function(S,K,n,gamma = 0.5)
{
  L <- logGaus(S, K, n)
  E <- sum(K[lower.tri(K,diag=TRUE)] != 0)
  p <- nrow(K)
  
  # return EBIC:
  -2 * L + E * log(n) + 4 * E * gamma * log(p)  
}