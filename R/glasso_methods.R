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
  gamma = 0,
  ... # glasso arguments
) {
#   stopifnot(require("glasso"))
  
  # Default rho:
#   if (missing(rho))
#   {
    # Compute rho max (code taken from huge):
    rho.max = max(max(S - diag(nrow(S))), -min(S - diag(nrow(S))))
    rho.min = rho.max/100
    rho = exp(seq(log(rho.min), log(rho.max), length = 100))
#   }
  

#   sink("/dev/null")
  glas_path <- glassopath(S, rho, trace = 0)
#   sink()

    # Likelihoods:
    EBICs <- apply(glas_path$wi,3,function(C){
      L <- n/2 * (log(det(C)) - sum(diag(S %*% C)))
      E <- sum(C[lower.tri(C,diag=TRUE)] != 0)
      p <- nrow(C)
      
      # EBIC:
      -2 * L + E * log(n) + 4 * E * gamma * log(p)
    })

# 
# # For each rho, run glasso and store matrix indices of zeroes:
#   Zeroes <- lapply(rho, function(r) {
#     Res <- glasso(S, r, ...)
#     which(Res$wi==0,arr.ind=TRUE)    
#     })
# 
#   # Rerun glasso to estimate unpenalized models with forced zeroes:
#   sink("/dev/null")
#   glassoRes <- lapply(Zeroes, function(z) glasso(S, 0, z, ...))
#   sink()
# #   
# #   i <- 10
# #   unPenalized <- glasso(S, rho[i], zero = which(glassoRes[[i]]$wi !=0,arr.ind=TRUE))
# #   unPenalized$loglik
# #   glassoRes[[i]]$loglik
#   
#   # Compute EBIC:
#   EBICs <- sapply(glassoRes, function(res,gamma, n, S){
# 
#     C <- res$wi
#     L <- n/2 * (log(det(C)) - sum(diag(S %*% C)))
# #     L <- res$loglik
#     E <- sum(C[lower.tri(C,diag=TRUE)] != 0)
#     p <- nrow(C)
#     
#     # EBIC:
#     -2 * L + E * log(n) + 4 * E * gamma * log(p)
#   }, gamma = gamma, n = n, S = S)

  # Smalles EBIC:
  opt <- which.min(EBICs)

#   net <- as.matrix(forceSymmetric(wi2net(glassoRes[[opt]]$wi)))
  net <- as.matrix(forceSymmetric(wi2net(glas_path$wi[,,opt])))
  colnames(net) <- rownames(net) <- colnames(S)

  # Return 
  return(net)
}


