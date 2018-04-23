# Model selection:
ggmModSelect <- function(
  S, # Sample covariance matrix
  n, # Sample size
  gamma = 0, # EBIC parameter, set to 0 for BIC selection
  start = c("glasso","empty","full"),
  stepwise = TRUE,
  considerPerStep = c("subset","all"), # Subset will only consider changing edges that previously would improve EBIC. When no edge improves. All edges are tested again.
  verbose = TRUE,
  nCores = 1,
  checkPD = TRUE,
  ... # EBICglasso arguments for starting point
) {
  if (is.character(start)){
    # Start:
    start <- match.arg(start)
  } else {
    startMat <- start
    start <- "manual"
  }

  
  # Number of variables:
  nVar <- ncol(S)
  
  # Warning if there are many variables:
  if (nVar > 30 && stepwise && verbose){
    message("'ggmModSelect' using stepwise = TRUE may be very slow in large graphs (> 30 nodes). Consider setting stepwise = FALSE")
  }
  
  if (checkPD){
    if (any(eigen(S)$values < 0)) stop("'S' is not positive definite")
  }
  
  # Standardize cov matrix:
  S <- cov2cor(S)
  
  ### Starting graph ###
  if (start == "glasso"){
    if (verbose) message("Running glasso to obtain starting model...")
    # Run the glassopath:
    glassores <- EBICglassoCore(
      S = S, # Sample covariance matrix
      n = n, # Sample size
      gamma = gamma,
      refit = TRUE, # If TRUE, network structure is taken and non-penalized version is computed.
      ebicMethod = "new",
      regularized = FALSE,
      threshold = FALSE,
      verbose = FALSE,
      returnAllResults = TRUE)
    
    curGraph <- glassores$optnet
    curEBIC <- min(glassores$ebic)
  } else if (start == "empty"){
    curGraph <- matrix(0, nVar, nVar)
    fit <- ggmFit(curGraph, S, n, verbose = FALSE, ebicTuning = gamma)
    curEBIC <- fit$fitMeasures$ebic
  } else if (start == "full"){
    curGraph <- corpcor::cor2pcor(cov2cor(S))
    fit <- ggmFit(curGraph, S, n, verbose = FALSE, ebicTuning = gamma)
    curEBIC <- fit$fitMeasures$ebic
  } else if (start == "manual"){
    curGraph <- startMat
    fit <- ggmFit(startMat, S, n, verbose = FALSE, ebicTuning = gamma, refit = TRUE)
    curEBIC <- fit$fitMeasures$ebic
  }
  
  # If not stepwise model search, stop here:
  if (!stepwise){
    Results <- list(
      graph = curGraph,
      EBIC = curEBIC
    )
    return(Results)
  }
  
  # Parallel:
  if (nCores > 1){
    cl <- parallel::makePSOCKcluster(nCores - 1)
    # Export to cluster:
    parallel::clusterExport(cl, c("S","n"), envir = environment())
  } else {
    cl <- NULL
  }
  
  # Edges to currently consider:
  curSkel <- curGraph!=0
  curEdges <- curSkel[upper.tri(curSkel)]
  curConsider <- rep(TRUE,length(curEdges))
  
  # Now perform stepwise model selection until optimum is reached:
  repeat{
    
    # Form all graphs to consider:
    allGraphs <- lapply(which(curConsider),function(i){
      testEdges <- curEdges
      testEdges[i] <- !curEdges[i]
      testSkel <- matrix(0,nVar,nVar)
      testSkel[upper.tri(testSkel)] <- 1*testEdges
      testSkel[lower.tri(testSkel)] <- t(testSkel)[lower.tri(testSkel)]
      testSkel
    })
    
    # Run the loop:
    if (all(curConsider)){
      if (verbose) message("Testing all edges...")      
    } else {
      if (verbose) message("Testing subset of edges...")
    }

    Results <- pblapply(allGraphs,function(G){
      # Fit graph:
      if (!all(G[upper.tri(G)] != 0)){
        suppressWarnings(glassores_test <- glasso::glasso(S, 0, zero = which(G == 0 & upper.tri(G), arr.ind=TRUE), trace = 0, penalize.diagonal=FALSE, ...))
      } else {
        suppressWarnings(glassores_test <- glasso::glasso(S, 0, trace = 0, penalize.diagonal=FALSE, ...))
      }
      
      # Compute EBIC:
      fit <- ggmFit(invSigma = glassores_test$wi,covMat = S, sampleSize = n, ebicTuning = gamma, refit = FALSE)
      
      return(list(
        graph = wi2net(glassores_test$wi),
        EBIC = fit$fitMeasures$ebic
      ))
    }, cl = cl)
    
    # All EBICs:
    EBICs <- sapply(Results,"[[","EBIC")
    
    # Test if any smaller:
    if (any(EBICs < curEBIC)){
      # Update which to consider:
      curConsider[curConsider] <- EBICs < curEBIC
      
      # Find optimal network:
      optnet <- which.min(EBICs)
      curGraph <- Results[[optnet]]$graph
      curEBIC <- Results[[optnet]]$EBIC
      curConsider[optnet] <- FALSE
      if (verbose) message("Changed one edge...")
      
      # Update current edges:
      if (!any(curConsider)){
        curConsider[] <- TRUE
      }
      curSkel <- curGraph!=0
      curEdges <- curSkel[upper.tri(curSkel)]
    } else {
      
      # If we were considering all edges, break:
      if (all(curConsider)){
        break        
      } else {
        # Else consider all edges again:
        curConsider[] <- TRUE
      }

    }
  }
  
  # stop cluster:
  if (nCores > 1){
    parallel::stopCluster(cl)
  }
  
  if (!is.null(colnames(S))){
    rownames(curGraph) <- colnames(curGraph) <- colnames(S)
  }
  
  return(list(
    graph = as.matrix(curGraph),
    EBIC = curEBIC
  ))
}