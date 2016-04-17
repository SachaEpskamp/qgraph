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
  lambda.min.ratio = 0.01,
  returnAllResults = FALSE, # If true, returns a list
  checkPD = TRUE, # Checks if matrix is positive definite and stops if not
  penalizeMatrix, # Optional logical matrix to indicate which elements are penalized
  countDiagonal = FALSE, # Set to TRUE to get old qgraph behavior: conting diagonal elements as parameters in EBIC computation. This is not correct, but is included to replicate older analyses
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
  if (missing(penalizeMatrix)){
    glas_path <- glassopath(S, lambda, trace = 0, penalize.diagonal=penalize.diagonal, ...)
  }else{
    glas_path <- list(
        w = array(0, c(ncol(S), ncol(S), length(lambda))),
        wi = array(0, c(ncol(S), ncol(S), length(lambda))),
        rholist = lambda
      )
    
    for (i in 1:100){
      res <- glasso(S, penalizeMatrix * lambda[i], trace = 0, penalize.diagonal=penalize.diagonal, ...)
      glas_path$w[,,i] <- res$w
      glas_path$wi[,,i] <- res$wi
    }
  }
  

    # Compute EBICs:
#     EBICs <- apply(glas_path$wi,3,function(C){
#       EBIC(S, C, n, gamma)
#     })

    lik <- sapply(seq_along(lambda),function(i){
    logGaus(S, glas_path$wi[,,i], n)
    })

    EBICs <- sapply(seq_along(lambda),function(i){
      EBIC(S, glas_path$wi[,,i], n, gamma, countDiagonal=countDiagonal)
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


