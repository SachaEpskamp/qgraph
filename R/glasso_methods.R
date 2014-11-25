# Selects optimal lamba based on EBIC for given covariance matrix. 
# EBIC is computed as in Foygel, R., & Drton, M. (2010, November). Extended Bayesian Information Criteria for Gaussian Graphical Models. In NIPS (pp. 604-612). Chicago  

# Computes partial correlation matrix given precision matrix:
wi2net <- function(x)
{
  x <- -cov2cor(x)
  diag(x) <- 0
  x <- forceSymmetric(x)
  return(x)
}

# Computes optimal glasso network based on EBIC:
EBICglasso <- function(
  S, # Sample covariance matrix
  n, # Sample size
  gamma = 0.5,
  penalize.diagonal = FALSE, # Penalize diagonal?
  nlambda = 100,
  lambda.min.ratio = 0.1,
  returnAllResults = FALSE, # If true, returns a list
  checkPD = TRUE, # Checks if matrix is positive definite and stops if not
  ... # glasso arguments
) {
  
  if (checkPD){
    if (any(eigen(S)$values < 0)) stop("'S' is not positive definite")
  }
  
  # Standardize cov matrix:
  S <- cov2cor(S)

  
  # Compute lambda sequence (code taken from huge package):
    lambda.max = max(max(S - diag(nrow(S))), -min(S - diag(nrow(S))))
lambda.min = lambda.min.ratio*lambda.max
    lambda = exp(seq(log(lambda.min), log(lambda.max), length = nlambda))
  
  # Run glasso path:
  glas_path <- glassopath(S, lambda, trace = 0, penalize.diagonal=penalize.diagonal)

    # Compute EBICs:
#     EBICs <- apply(glas_path$wi,3,function(C){
#       EBIC(S, C, n, gamma)
#     })

    lik <- sapply(seq_along(lambda),function(i){
    logGaus(S, glas_path$wi[,,i], n)
    })

    EBICs <- sapply(seq_along(lambda),function(i){
      EBIC(S, glas_path$wi[,,i], n, gamma)
    })

  # Smallest EBIC:
  opt <- which.min(EBICs)
  
  # Return network:
  net <- as.matrix(forceSymmetric(wi2net(glas_path$wi[,,opt])))
  colnames(net) <- rownames(net) <- colnames(S)

  # Return 
  if (returnAllResults){
    return(list(
      results = glas_path,
      ebic = EBICs,
      loglik = lik,
      optnet = net,
      lambda = lambda
      ))
  } else return(net)
}


