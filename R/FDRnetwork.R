FDRnetwork <- function(
  net, # Correlation or partial correlation matrix
  cutoff = 0.9 # Cutoff value for lfdr
  )
{
  # Check if net is (partial) correlation network:
  if (!isSymmetric(net)) stop("Matrix is not symmetric, cannot be a (partial) correlation matrix")
  if (!all(eigen(net)$values > 0)) stop("Matrix is not positive definite, cannot be a (partial) correlation matrix")
  
  vec <- net[upper.tri(net)]
  Res <- fdrtool(vec, "correlation", plot=FALSE, verbose = FALSE, cutoff.method = "locfdr")
  newnet <- net
  newnet[upper.tri(newnet)][(1-Res$lfdr) > cutoff] <- 0
  newnet[lower.tri(newnet)] <- t(newnet)[lower.tri(newnet)]
  newnet <- as.matrix(newnet)
  diag(newnet) <- 1
  return(newnet)
}

