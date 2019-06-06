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
EBICglassoCore <- function(
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
  refit = TRUE, # If TRUE, network structure is taken and non-penalized version is computed.
  ebicMethod = c("new","old"),
  regularized = TRUE,
  threshold = FALSE,
  verbose = TRUE,
  criterion = "ebic",
  ... # glasso arguments
) {
  ebicMethod <- match.arg(ebicMethod)
  
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
    
    for (i in 1:nlambda){
      res <- glasso(S, penalizeMatrix * lambda[i], trace = 0, penalize.diagonal=penalize.diagonal, ...)
      glas_path$w[,,i] <- res$w
      glas_path$wi[,,i] <- res$wi
    }
  }
    
    # Threshold:
    if (threshold){
      for (i in 1:nlambda){
        # Degree:
        p <- ncol(glas_path$wi[,,i])
        # D <- max(centrality(ifelse( glas_path$wi[,,i] != 0,1, 0))$OutDegree)
        thresh <- (log(p*(p-1)/2)) / sqrt(n)
        glas_path$wi[,,i] <- ifelse(abs(glas_path$wi[,,i]) < thresh,0,glas_path$wi[,,i])
        
      }
    }
    
  

    # Compute EBICs:
    if (ebicMethod == "old"){
      if (criterion != "ebic") stop("criterion must be 'ebic' when ebicMethod = 'old'")
      EBICs <- sapply(seq_along(lambda),function(i){
        if (!regularized){
          invSigma <- ggmFit(wi2net(glas_path$wi[,,i]), S, sampleSize = n, ebicTuning = gamma, refit = TRUE,verbose = FALSE)$invSigma
        } else {
          invSigma <- glas_path$wi[,,i]
        }
        EBIC(S, invSigma, n, gamma, countDiagonal=countDiagonal)
      })     
    } else {
      EBICs <- sapply(seq_along(lambda),function(i){
          fit <- ggmFit(wi2net(glas_path$wi[,,i]), S, n, ebicTuning = gamma,refit = !regularized, verbose = FALSE)
          # print(fit$fitMeasures$ebic)
          # browser()
          fit$fitMeasures[[criterion]]
        })
    }


    # lik <- sapply(seq_along(lambda),function(i){
    # logGaus(S, glas_path$wi[,,i], n)
    # })
    # 
    # EBICs <- sapply(seq_along(lambda),function(i){
    #   EBIC(S, glas_path$wi[,,i], n, gamma, countDiagonal=countDiagonal)
    # })

    # EBIC via lavaan codes:
    # EBICs <- sapply(seq_along(lambda),function(i){
    #     fit <- ggmFit(wi2net(glas_path$wi[,,i]), S, n, ebicTuning = gamma)
    #     print(fit$fitMeasures$ebic)
    #     # browser()
    #     fit$fitMeasures$ebic
    #   })

  # Smallest EBIC:
  opt <- which.min(EBICs)
  
  # Check if rho is smallest:
  if (opt == 1){
    message("Note: Network with lowest lambda selected as best network: assumption of sparsity might be violated.")
  }
  
  # Return network:
  net <- as.matrix(forceSymmetric(wi2net(glas_path$wi[,,opt])))
  colnames(net) <- rownames(net) <- colnames(S)
  
  # Check empty network:
  if (all(net == 0)){
    message("An empty network was selected to be the best fitting network. Possibly set 'lambda.min.ratio' higher to search more sparse networks. You can also change the 'gamma' parameter to improve sensitivity (at the cost of specificity).")
  }
  
  # Refit network:
  # Refit:
  if (refit){
    if (verbose) message("Refitting network without LASSO regularization")
    if (!all(net[upper.tri(net)]!=0)){
      glassoRes <- suppressWarnings(glasso::glasso(S, 0, zero = which(net == 0 & upper.tri(net), arr.ind=TRUE), trace = 0, penalize.diagonal=penalize.diagonal, ...))      
    } else {
      glassoRes <- suppressWarnings(glasso::glasso(S, 0, trace = 0, penalize.diagonal=penalize.diagonal, ...))
    }
    net <- as.matrix(forceSymmetric(wi2net(glassoRes$wi)))
    colnames(net) <- rownames(net) <- colnames(S)
    optwi <- glassoRes$wi
  } else {
    optwi <- glas_path$wi[,,opt]
  }

  # If regularized and low lambda was selected, give warning:
  if (regularized && lambda[opt] < 0.1 * lambda.max && !isTRUE(threshold)){
    warning("A dense regularized network was selected (lambda < 0.1 * lambda.max). Recent work indicates a possible drop in specificity. Interpret the presence of the smallest edges with care. Setting threshold = TRUE will enforce higher specificity, at the cost of sensitivity.")
  }
  
  # Return 
  if (returnAllResults){
    return(list(
      results = glas_path,
      ebic = EBICs,
      # loglik = lik,
      optnet = net,
      lambda = lambda,
      optwi = optwi
      ))
  } else return(net)
}

# Old function:

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
  refit = FALSE, # If TRUE, network structure is taken and non-penalized version is computed.
  threshold = FALSE,
  verbose = TRUE,
  # ebicMethod = c("new","old"),
  # ebicRefit = FALSE,
  ... # glasso arguments
) {
 EBICglassoCore(S=S, # Sample covariance matrix
                n=n, # Sample size
                gamma = gamma,
                penalize.diagonal = penalize.diagonal, # Penalize diagonal?
                nlambda = nlambda,
                lambda.min.ratio = lambda.min.ratio,
                returnAllResults = returnAllResults, # If true, returns a list
                checkPD = checkPD, # Checks if matrix is positive definite and stops if not
                penalizeMatrix = penalizeMatrix, # Optional logical matrix to indicate which elements are penalized
                countDiagonal = countDiagonal, # Set to TRUE to get old qgraph behavior: conting diagonal elements as parameters in EBIC computation. This is not correct, but is included to replicate older analyses
                refit = refit, # If TRUE, network structure is taken and non-penalized version is computed.
                ebicMethod = "old",
                regularized = TRUE,
                threshold=threshold,
                verbose=verbose,
                ...)
}
# 
# 
# # Computes optimal glasso network based on EBIC:
# EBICglasso2 <- function(
#   S, # Sample covariance matrix
#   n, # Sample size
#   gamma = 0.5,
#   penalize.diagonal = FALSE, # Penalize diagonal?
#   nlambda = 100,
#   lambda.min.ratio = 0.01,
#   returnAllResults = FALSE, # If true, returns a list
#   checkPD = TRUE, # Checks if matrix is positive definite and stops if not
#   penalizeMatrix, # Optional logical matrix to indicate which elements are penalized
#   countDiagonal = FALSE, # Set to TRUE to get old qgraph behavior: conting diagonal elements as parameters in EBIC computation. This is not correct, but is included to replicate older analyses
#   refit = TRUE, # If TRUE, network structure is taken and non-penalized version is computed.
#   # ebicMethod = c("new","old"),
#   # ebicRefit = FALSE,
#   threshold = FALSE,
#   ... # glasso arguments
# ) {
#   EBICglassoCore(S=S, # Sample covariance matrix
#                  n=n, # Sample size
#                  gamma = gamma,
#                  penalize.diagonal = penalize.diagonal, # Penalize diagonal?
#                  nlambda = nlambda,
#                  lambda.min.ratio = lambda.min.ratio,
#                  returnAllResults = returnAllResults, # If true, returns a list
#                  checkPD = checkPD, # Checks if matrix is positive definite and stops if not
#                  penalizeMatrix = penalizeMatrix, # Optional logical matrix to indicate which elements are penalized
#                  countDiagonal = countDiagonal, # Set to TRUE to get old qgraph behavior: conting diagonal elements as parameters in EBIC computation. This is not correct, but is included to replicate older analyses
#                  refit = refit, # If TRUE, network structure is taken and non-penalized version is computed.
#                  ebicMethod = "new",
#                  regularized = TRUE,
#                  threshold=threshold,
#                  ...)
# }


