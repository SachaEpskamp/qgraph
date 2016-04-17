# Simply computes the Gaussian log likelihood given sample covariance and estimate of precision:

# Original:
# logGaus <- function(S,K,n)
# {
#   SK = S %*% K
#   tr = function(A) sum(diag(A))
#   n/2 * (log(det(K)) - tr(SK))
# }

## According to huge???
logGaus <- function(S,K,n)
{
    KS = K %*% S
    tr = function(A) sum(diag(A))
    return(n/2 * (log(det(K)) - tr(KS))  )
}

# Computes the EBIC:
EBIC <- function(S,K,n,gamma = 0.5,E,countDiagonal=FALSE)
{
#   browser()
  L <- logGaus(S, K, n)
  if (missing(E)){
    E <- sum(K[lower.tri(K,diag=countDiagonal)] != 0)
  }
  p <- nrow(K)
  
  # return EBIC:
  -2 * L + E * log(n) + 4 * E * gamma * log(p)  
}