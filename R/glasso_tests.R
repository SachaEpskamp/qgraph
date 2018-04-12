# Test version not to be included yet

# # Computes optimal glasso network based on EBIC:
# EBICglassoCluster <- function(
#   S, # Sample covariance matrix
#   n, # Sample size
#   cluster = matrix(1, ncol(S), ncol(S)),
#   gamma = 0.5,
#   penalize.diagonal = FALSE, # Penalize diagonal?
#   nlambda = 100,
#   lambda.min.ratio = 0.01,
#   returnAllResults = FALSE, # If true, returns a list
#   checkPD = TRUE, # Checks if matrix is positive definite and stops if not
#   penalizeMatrix, # Optional logical matrix to indicate which elements are penalized
#   countDiagonal = FALSE, # Set to TRUE to get old qgraph behavior: conting diagonal elements as parameters in EBIC computation. This is not correct, but is included to replicate older analyses
#   refit = FALSE, # If TRUE, network structure is taken and non-penalized version is computed.
#   ebicMethod = c("old","new"),
#   regularized = TRUE, # If FALSE: refit all networks
#   threshold = FALSE,
#   verbose = TRUE,
#   nCores = 1,
#   ... # glasso arguments
# ) {
#   ebicMethod <- match.arg(ebicMethod)
#   
#   if (checkPD){
#     if (any(eigen(S)$values < 0)) stop("'S' is not positive definite")
#   }
#   
#   # Standardize cov matrix:
#   S <- cov2cor(S)
#   
#   # Compute lambda sequence (code taken from huge package):
#   lambda.max = max(max(S - diag(nrow(S))), -min(S - diag(nrow(S))))
#   lambda.min = lambda.min.ratio*lambda.max
#   lambda = exp(seq(log(lambda.min), log(lambda.max), length = nlambda))
#   
# 
#   # Number of clusters:
#   diag(cluster) <- NA
#   nCluster <- length(unique(na.omit(c(cluster))))
#   clusters <- sort(unique(na.omit(c(cluster))))
#   allLambda <- do.call(expand.grid,lapply(clusters,function(x)lambda))
#   
#   # Run glasso path (old codes):
#   # if (nCluster == 1 & missing(penalizeMatrix)){
#   #   glas_path <- glassopath(S, lambda, trace = 0, penalize.diagonal=penalize.diagonal, ...)
#   #   nGraphs <- nlambda
#   #   
#   #   # Threshold:
#   #   if (threshold){
#   #     for (i in 1:nGraphs){
#   #       # Degree:
#   #       p <- ncol(glas_path$wi[,,i])
#   #       # D <- max(centrality(ifelse( glas_path$wi[,,i] != 0,1, 0))$OutDegree)
#   #       threshold <- (log(p*(p-1)/2)) / sqrt(n)
#   #       glas_path$wi[,,i] <- ifelse(abs(glas_path$wi[,,i]) < threshold,0,glas_path$wi[,,i])
#   #       
#   #     }
#   #   }
#   #   
#   # }else{ # New codes:
#     nGraphs <- nrow(allLambda)
#     
#     # glas_path <- list(
#     #   w = array(0, c(ncol(S), ncol(S), nGraphs)),
#     #   wi = array(0, c(ncol(S), ncol(S), nGraphs)),
#     #   rholist = allLambda
#     # )
#     
#     # PenalizeMatrix:
#     if (missing(penalizeMatrix)){
#       penalizeMatrix <- matrix(TRUE, ncol(S), ncol(S))
#     }
#     
#     # Parallel:
#     if (nCores > 1){
#       cl <- parallel::makePSOCKcluster(nCores - 1)
#       # Export to cluster:
#       parallel::clusterExport(cl, c("cluster","clusters","allLambda","S","penalizeMatrix","penalize.diagonal","regularized","threshold","countDiagonal","n"), envir = environment())
#     } else {
#       cl <- NULL
#     }
#     
#     # Run the loop"
#     Results <- pblapply(1:nGraphs,function(i){
#       # Construct penalty matrix:
#       lambdaMat <- cluster
#       for (c in seq_along(clusters)){
#         lambdaMat[lambdaMat==clusters[c]] <- allLambda[i,c]
#       }
#       diag(lambdaMat) <- 0
#    
#       res <- glasso(S, penalizeMatrix * lambdaMat, trace = 0, penalize.diagonal=penalize.diagonal, ...)
#       
#       
#       # Threshold?
#       if (threshold){
#           p <- ncol(res$wi)
#           # D <- max(centrality(ifelse( glas_path$wi[,,i] != 0,1, 0))$OutDegree)
#           threshold <- (log(p*(p-1)/2)) / sqrt(n)
#           res$wi <- ifelse(abs(res$wi) < threshold,0,res$wi)
#       }
#       
#       # Refit without regularization?
#       if (!regularized){
#         if (!all(res$wi[upper.tri(res$wi)] != 0)){
#           res <- suppressWarnings(glasso::glasso(S, 0, zero = which(res$wi == 0 & upper.tri(res$wi), arr.ind=TRUE), trace = 0, penalize.diagonal=penalize.diagonal, ...))  
#         } else {
#           res <- suppressWarnings(glasso::glasso(S, 0, trace = 0, penalize.diagonal=penalize.diagonal, ...))  
#         }
#       }
#       
#       # Compute EBIC:
#       if (ebicMethod == "old"){
#         res$EBIC <- EBIC(S, res$wi, n, gamma, countDiagonal=countDiagonal)
#       } else {
#           fit <- ggmFit(invSigma = res$wi, covMat = S, sampleSize = n, ebicTuning = gamma,refit = FALSE, verbose = FALSE)
#           res$EBIC <- fit$fitMeasures$ebic
#       }
#       
#       
#       return(res)
#       # glas_path$w[,,i] <- res$w
#       # glas_path$wi[,,i] <- res$wi
#     }, cl = cl)
#     # 
#     # # Combine to path:
#     # glas_path <- list(
#     #   w = do.call(abind::abind,c(lapply(Results,"[[","w"),list(along=3))),
#     #   wi = do.call(abind::abind,c(lapply(Results,"[[","w"),list(along=3)))
#     # )
#   # }
#   
# 
#   
#   
#   # Compute EBICs:
#   # if (ebicMethod == "old"){
#   #   EBICs <- sapply(1:nGraphs,function(i){
#   #     # if (ebicRefit){
#   #     #   invSigma <- ggmFit(wi2net(glas_path$wi[,,i]), S, sampleSize = n, ebicTuning = gamma, refit = TRUE,verbose = FALSE)$invSigma
#   #     # } else {
#   #     #   invSigma <- glas_path$wi[,,i]
#   #     # }
#   #     invSigma <- glas_path$wi[,,i]
#   #     EBIC(S, invSigma, n, gamma, countDiagonal=countDiagonal)
#   #   })     
#   # } else {
#   #   EBICs <- sapply(1:nGraphs,function(i){
#   #     fit <- ggmFit(wi2net(glas_path$wi[,,i]), S, n, ebicTuning = gamma,refit = FALSE, verbose = FALSE)
#   #     # print(fit$fitMeasures$ebic)
#   #     # browser()
#   #     fit$fitMeasures$ebic
#   #   })
#   # }
#   
#   
#   # lik <- sapply(seq_along(lambda),function(i){
#   # logGaus(S, glas_path$wi[,,i], n)
#   # })
#   # 
#   # EBICs <- sapply(seq_along(lambda),function(i){
#   #   EBIC(S, glas_path$wi[,,i], n, gamma, countDiagonal=countDiagonal)
#   # })
#   
#   # EBIC via lavaan codes:
#   # EBICs <- sapply(seq_along(lambda),function(i){
#   #     fit <- ggmFit(wi2net(glas_path$wi[,,i]), S, n, ebicTuning = gamma)
#   #     print(fit$fitMeasures$ebic)
#   #     # browser()
#   #     fit$fitMeasures$ebic
#   #   })
#     
#     
#   # Smallest EBIC:
#   EBICs <- sapply(Results,'[[','EBIC')
#   opt <- which.min(EBICs)
#   
#   # Check if rho is smallest:
#   # if (opt == 1 && verbose){
#   #   # warning("Network with lowest lambda selected as best network. Try setting 'lambda.min.ratio' lower.")
#   #   warning("Network with lowest lambda selected as best network.")
#   # }
#   
#   # Return network:
#   net <- as.matrix(forceSymmetric(wi2net(Results[[opt]]$wi)))
#   colnames(net) <- rownames(net) <- colnames(S)
#   
#   # Check empty network:
#   # if (all(net == 0) && verbose){
#   #   message("An empty network was selected to be the best fitting network. Possibly set 'lambda.min.ratio' higher to search more sparse networks. You can also change the 'gamma' parameter to improve sensitivity (at the cost of specificity).")
#   # }
# 
#   # Refit network:
#   # Refit:
#   if (refit && regularized){
#     if (verbose) message("Refitting network without LASSO regularization")
#     glassoRes <- suppressWarnings(glasso::glasso(S, 0, zero = which(net == 0 & upper.tri(net), arr.ind=TRUE), trace = 0, penalize.diagonal=penalize.diagonal, ...))
#     net <- as.matrix(forceSymmetric(wi2net(glassoRes$wi)))
#     colnames(net) <- rownames(net) <- colnames(S)
#     optwi <- glassoRes$wi
#   } else {
#     optwi <- Results[[opt]]$wi
#   }
#   
#   # Return 
#   if (returnAllResults){
#     return(list(
#       results = Results,
#       ebic = EBICs,
#       # loglik = lik,
#       optnet = net,
#       lambda = allLambda,
#       optwi = optwi
#     ))
#   } else return(net)
# }