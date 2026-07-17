## Automatically computes a correlation matrix:
# Wrapper around lavCor which detects ordinal variables!

cor_auto <- function(
  data, # A data frame
  select, # Columns to select
  detectOrdinal = TRUE, # Detect ordinal variables
  ordinalLevelMax = 7, # Maximum amount of levels to be classified as ordinal
  npn.SKEPTIC = FALSE, # If TRUE, will compute nonparanormal SKEPTIC on fully continous data
  forcePD = FALSE, # Forces the result to be positive definite using nearPD from Matrix
  missing = "pairwise",
  verbose=TRUE
  )
{

  # Check for data frame:
  # if (!is.data.frame(data))
  # {
    data <- as.data.frame(data)
  # }
  
  # Select columns:
  if (!missing(select))
  {
    data <- subset(data, select = select)
  }
  
  # Remove factors:
  Factors <- sapply(data,is,"factor") & !sapply(data,is,"ordered")
  if (any(Factors))
  {
    if (verbose){
      message(paste("Removing factor variables:",paste(names(data)[Factors], collapse = "; ")))      
    }

    data <- data[,!Factors,drop=FALSE]
  }

  # Remove columns with all NA:
  data <- data[,sapply(data,function(x)mean(is.na(x)))!=1,drop=FALSE]

  
  # Detect ordinal:
  Numerics <- which(sapply(data,is.numeric) | sapply(data,is.integer))
  
  if (detectOrdinal & length(Numerics) > 0)
  {
    
    isOrd <- sapply(Numerics, function(i) {
      isInt <- is.integer(data[,i]) | all(data[,i] %% 1 == 0, na.rm=TRUE)
      nLevel <- length(unique(data[,i]))
      return(isInt & nLevel <= ordinalLevelMax)
    } )
    
    if (any(isOrd))
    {
      if (verbose){
        message(paste("Variables detected as ordinal:",paste(names(data)[Numerics][isOrd], collapse = "; ")))        
      }
      
      for (i in Numerics[isOrd])
      {
        data[,i] <- ordered(data[,i])
      } 
    }
    
  }
  
  ### START COMPUTING CORRELATIONS ###
  # Warn if npn.SKEPTIC is requested but cannot be applied due to ordinal variables:
  if (npn.SKEPTIC && !all(sapply(data,is.numeric) | sapply(data,is.integer)))
  {
    warning("'npn.SKEPTIC' is ignored: the nonparanormal SKEPTIC is only applied when all variables are continuous, but the data contain ordinal variables. Set 'detectOrdinal = FALSE' to treat all variables as continuous.")
  }

  # IF ALL NUMERIC OR INTEGER, NONPARANORMAL SKEPTIC:
  if (all(sapply(data,is.numeric) | sapply(data,is.integer) ) & npn.SKEPTIC)
  {
#     message("All variables detected to be continuous, computing nonparanormal skeptic!")
    
    for (i in seq_len(ncol(data))) data[,i] <- as.numeric(data[,i])
    if(!requireNamespace("huge")) stop("'huge' package needs to be installed.")
    # Drop row names, as huge.npn would otherwise erroneously copy them to the
    # returned correlation matrix and fail:
    dataMat <- as.matrix(data)
    rownames(dataMat) <- NULL
    CorMat <- huge::huge.npn(dataMat, "skeptic")
  } else {

    # Re-throw estimation failures inside lavaan (e.g. "NA/NaN Hessian
    # evaluation" from nlminb) with context and possible workarounds
    # (github issue #78):
    lavCorWithContext <- function(expr){
      tryCatch(expr, error = function(e){
        stop("lavaan::lavCor() failed to estimate the correlation matrix: ",
             conditionMessage(e),
             "\nThis is an estimation failure in the lavaan package, not in qgraph itself. It typically occurs with many (ordinal) variables, few observations, or ordinal variables with rarely endorsed categories. Possible workarounds: remove variables with (nearly) constant values or very sparse categories, treat ordinal variables as continuous with 'detectOrdinal = FALSE', or compute the correlations without lavaan (e.g. cor(data, use = \"pairwise.complete.obs\")).",
             call. = FALSE)
      })
    }

    #provide needed arguments for lavcor
    if(missing == "fiml"){

      #fiml needs ml, TRUE and fit to have estimation in object
      lavobject <- lavCorWithContext(lavaan::lavCor(data, missing = missing, se = "none", meanstructure = TRUE, estimator = "ML", output = "fit"))
      if (!lavaan::lavInspect(lavobject, "converged"))
      {
        warning("The lavaan model used to estimate FIML correlations did not converge; the returned correlation matrix may not be reliable.")
      }
      #compute correlation matrix from covariance matrix
      CorMat <- cov2cor(lavaan::inspect(lavobject, "cov.ov"))
      class(CorMat) <- "matrix"
    }
    else{
      #use defaults for other options
      meanstructure <- FALSE
      estimator <- "two.step"
      CorMat <- lavCorWithContext(suppressWarnings(lavaan::lavCor(data, missing = missing, meanstructure = meanstructure, estimator = estimator)))
      class(CorMat) <- "matrix"
    }
    
  }

  # Check for positive definite:
  if(forcePD & !all(eigen(CorMat)$values > 0))  {
    warning("Correlation matrix is not positive definite. Finding nearest positive definite matrix")
  
    CorMat <- as.matrix(Matrix::nearPD(CorMat, corr = TRUE, ensureSymmetry = TRUE, keepDiag = TRUE)$mat)
  }

  return(CorMat)

#   ## If all ordinal, do tetrachoric or polychoric:
#   if (all(sapply(data,is,"ordered")))
#   {
#     nLevel <- sapply(data,nlevels)
#     
#     # Tetrachoric:
#     if (all(nLevel == 2))
#     {
#       message("Binary data detected, computing tetrachoric correlations!")
#       for (i in seq_len(ncol(data))) data[,i] <- as.numeric(data[,i])
#       res <- tetrachoric(as.matrix(data))
#       CorMat <- as.matrix(res$rho)
#       attr(CorMat, "thresholds") <- res$tau
#       return(CorMat)
#       
#     } else {
#       message("Polytomous data detected, computing polychoric correlations!")
#       for (i in seq_len(ncol(data))) data[,i] <- as.numeric(data[,i])
#       res <- polychoric(as.matrix(data))
#       CorMat <- as.matrix(res$rho)
#       attr(CorMat, "thresholds") <- res$tau
#       return(CorMat)
#     }
#     
#   } 
#   
#   # Else shared data detected, use muthen1984 from lavaan:
#   message("Both continuous and ordinal data detected, using muthen1984 from Lavaan package!")
#   ov.names <- names(data)
#   ov.types <- lavaan:::lav_dataframe_check_vartype(data, ov.names=ov.names)
#   ov.levels <- sapply(lapply(data, levels), length)
#   mutRes <- lavaan:::muthen1984(data, ov.names, ov.types, ov.levels)
#   
#   CorMat <- mutRes$COR
#   attr(CorMat,"thresholds") <- mutRes$TH
#   return(CorMat)
}
