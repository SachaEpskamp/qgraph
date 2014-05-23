VARglm <-
  function(x,family,vars,adjacency,icfun = BIC,...)
  {
    # Returns estimated weights matrix of repeated measures data x
    ## x must be matrix, rows indicate measures and columns indicate variables
    # If adjacency is missing, full adjacency is tested
    # 'family' can be assigned family function (see ?family), list of such
    ## functions for each variable in x or character vector with names of the
    ## family functions.
    # 'vars' must be a vector indicating which variables are predicted, can be useful for parallel implementation.
    
    if (missing(x)) stop("'x' must be assigned")
    x <- as.matrix(x)
    
    Ni <- ncol(x)
    Nt <- nrow(x)
    
    # Check input:
    if (missing(vars)) vars <- 1:Ni
    No <- length(vars)
    
    if (missing(adjacency)) adjacency <- matrix(1,Ni,No)
    if (is.vector(adjacency)) adjacency <- as.matrix(adjacency)
    if (!is.matrix(adjacency) && ncol(adjacency)!=No && nrow(adjacency)!=Ni) stop("'adjacency' must be square matrix with a row for each predictor and column for each outcome variable.")
    
    if (any(apply(x,2,sd)==0))
    {
      adjacency[apply(x,2,sd)==0,] <- 0
      adjacency[,apply(x,2,sd)==0] <- 0
      warning("Adjacency matrix adjusted to not include nodes with 0 variance.")
    }
    
    if (missing(family)) 
    {
      if (identical(c(0,1),sort(unique(c(x))))) family <- rep("binomial",No) else family <- rep("gaussian",No)
    }
    if (length(family)==1)
    {
      family <- list(family)
      if (No > 1) for (i in 2:No) family[[i]] <- family[[1]]
    }
    if (length(family)!=No) stop("Length of family is not equal to number of outcome variables.")
    
    ## Output:
    Out <- list() 
    Out$graph <- matrix(0,Ni,No)
    Out$IC <- 0
    
    # Run glms:
    for (i in 1:No)
    {
      if (is.function(family[[i]])) fam <- family[[i]] else fam <- get(family[[i]])
      if (any(as.logical(adjacency[,i]))) 
      {
        tryres <- try(Res <- glm(x[-1,vars[i]] ~ x[-nrow(x),as.logical(adjacency[,i])],family=fam))
        if (is(tryres, 'try-error')) Res <- glm(x[-1,vars[i]] ~ NULL,family=fam)
      } else {
        Res <- glm(x[-1,vars[i]] ~ NULL,family=fam)
      }
      Out$graph[as.logical(adjacency[,i]),i] <- coef(Res)[-1]
      Out$IC <- Out$IC + icfun(Res,...)
    }
    Out$graph[is.na(Out$graph)] <- 0
    return(Out)
  }