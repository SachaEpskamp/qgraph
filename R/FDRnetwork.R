FDRnetwork <- function(
  net, # Correlation or partial correlation matrix
  cutoff = 0.1, # Cutoff value for lfdr
  method = c('lfdr', 'pval', 'qval') # Element of result to use in thresholding. pval: remove edges HIGHER than cutoff score. qval: remove edges HIGHER than cutoff score
  )
{
  # Check if net is (partial) correlation network:
  if (!isSymmetric(net)) stop("Matrix is not symmetric, cannot be a (partial) correlation matrix")
  if (!all(eigen(net)$values > 0)) stop("Matrix is not positive definite, cannot be a (partial) correlation matrix")
  
  vec <- net[upper.tri(net)]
  Res <- fdrtool(vec, "correlation", plot=FALSE, verbose = FALSE, cutoff.method = "locfdr")
  newnet <- net
  newnet[upper.tri(newnet)][Res[[method[[1]]]] > cutoff] <- 0
  newnet[lower.tri(newnet)] <- t(newnet)[lower.tri(newnet)]
  newnet <- as.matrix(newnet)
  diag(newnet) <- 1
  return(newnet)
}

