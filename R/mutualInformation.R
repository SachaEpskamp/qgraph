# entropy <- function(covMat, subset){
#   covMat <- covMat[subset,subset,drop=FALSE]
#   1/2 * log((2*pi*exp(1))^ncol(covMat) * det(covMat), 2 )
# }

mutualInformation <- function(
  ggm,
  from, # Defaults to all nodes
  to = "all", # Defaults to all nodes but node of interest
  covMat
){
  
  
  if (!missing(covMat)){
    if (!missing(ggm)) stop("If 'covMat' is not missing, 'ggm' must be missing.")
    corMat <- cov2cor(covMat)
  } else {
    net <- getWmat(ggm)
    diag(net) <- 0
    inv <- diag(nrow(net)) - net
    if (any (eigen(inv)$values < 0)) stop("Network is not a valid partial correlation network")
    corMat <- cov2cor(solve(inv))
    colnames(corMat) <- colnames(net)
  }

  if (missing(from)){
    from <- seq_len(ncol(corMat))
  }

  # If from and to are characters, match to labels:
  if (is.character(from)){
    if (!all(from %in% colnames(corMat))) stop("Node names not found in column names of network")
    from <- match(from,colnames(corMat))
  }

  if (is.character(to) && !identical(to,"all")){
    if (!all(to %in% colnames(corMat))) stop("Node names not found in column names of network")
    to <- match(to,colnames(corMat))
  }

  # Mutual information:
  res <- sapply(from, function(f){
    # entropy(corMat, f) + entropy(corMat, to) - entropy(corMat, unique(c(from,to)))
    if (identical(to,"all")){
      to2 <- seq_len(ncol(corMat))[-f]
    } else {
      to2 <- to
    }
    if (any(f == to2)) return(NA)
    all <- unique(c(f,to2))
    1/2 * log((det(corMat[f,f,drop=FALSE]) * det(corMat[to2,to2,drop=FALSE]) / det(corMat[all,all,drop=FALSE]))   ,2)
  })
  
  names(res) <- colnames(corMat)[from]
  
  return(res)
}