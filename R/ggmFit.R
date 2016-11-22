goodNum <- function(x){
  sapply(x,function(xx){
    if (xx < 0.0001 & xx > 0){
      return("< 0.0001")
    }
    digits <- max(0,floor(log10(abs(xx))) + 1)
    isInt <- xx%%1 == 0
    gsub("\\.$","",formatC(signif(unlist(xx),digits=digits+(!isInt)*2), digits=digits+(!isInt)*2,format="fg", flag="#"))
  })  
}

# Computes fit measures of a GGM
ggmFit <- function(
  pcor, # pcor matrix or qgraph object
  covMat, # sample variance-covariance matrix
  sampleSize, # Sample sample-size
  invSigma, # inverse variance covariance matrix instead of pcor
  refit = TRUE, # Refit the model in glasso without LASSO?
  ebicTuning = 0.5
){
  mimic <- "lavaan"
  
  if (missing(covMat)){
    stop("Argument 'covMat' may not be missing.")
  }
  if (missing(sampleSize)){
    stop("Argument 'sampleSize' may not be missing.")
  }
  
  # mimic <- match.arg(mimic)
  # If mimic is lavaan, rescale covmat:
  # Scale to divide by N:
  if (mimic == "lavaan"){
    covMat <- covMat * (sampleSize - 1)/sampleSize
  }
  
  # Number of observations:
  if (mimic == "lavaan"){
    Ncons <- sampleSize
  } else {
    Ncons <- sampleSize - 1
  }
  
  # If both pcor and invSigma not missing, stop:
  if (!missing(pcor) & !missing(invSigma)){
    stop("'pcor' and 'invSigma' arguments can not both be missing.")
  }
  
  # If missing invSigma, compute from pcor:
  if (missing(invSigma)){
    # If qgraph object, extract:
    if (is(pcor,"qgraph")){
      pcor <- qgraph::getWmat(pcor)
      diag(pcor) <- 1
    }
    
    # Sample inverse variance-covariance matrix:
    try <- try(sampleInverse <- corpcor::pseudoinverse(covMat))
    if (is(try,"try-error")){
      stop("Cannot compute pseudoinverse from sample variance-covariance matrix.")
    }
    
    # Remove diagonal:
    diag(pcor) <- 0
    
    # Compute delta scaling matrices:
    Delta <- diag(sqrt(diag(sampleInverse)))
    
    # Compute inverse:
    invSigma <- Delta %*% (diag(ncol(pcor)) - pcor) %*% Delta
  }
  
  # Refit:
  if (refit){
    message("Refitting network without LASSO regularization")
    glassoRes <- suppressWarnings(glasso::glasso(covMat, 0, zero = which(invSigma == 0, arr.ind=TRUE)))
    invSigma <- glassoRes$wi
    Sigma <- glassoRes$w
  } else {
    # Implied variance-covariance matrix:
    Sigma <- corpcor::pseudoinverse(invSigma)
  }
  
  # Fitmeasures list:
  fitMeasures <- list()
  
  # Number of variables:
  fitMeasures$nVar <- nVar <- ncol(covMat)
  
  # Number of observations:
  fitMeasures$nObs <- nVar * (nVar+1) / 2
  
  # Number of parameters:
  fitMeasures$npar <- sum(invSigma[upper.tri(invSigma,diag=FALSE)] != 0)
  
  # Degrees of freedom:
  fitMeasures$df <- fitMeasures$nObs - fitMeasures$npar
  
  # Compute Fmin:
  fitMeasures$fmin <- (sum(diag(covMat %*% corpcor::pseudoinverse(Sigma)))- log(det(covMat %*% corpcor::pseudoinverse(Sigma))) - nVar)/2
  fitMeasures$chisq <- 2 * Ncons * fitMeasures$fmin
  fitMeasures$pvalue <- pchisq(fitMeasures$chisq, fitMeasures$df, lower.tail = FALSE)
  
  # Baseline model:
  # via glasso:
  glassoRes_baseline <- suppressWarnings(glasso::glasso(covMat, 0, zero = which(diag(nVar) == 0, arr.ind=TRUE)))
  invSigma_baseline <- glassoRes_baseline$wi
  Sigma_baseline <- glassoRes_baseline$w
  
  fitMeasures$baseline.chisq <- Ncons * (sum(diag(covMat %*% Sigma_baseline))- log(det(covMat %*% Sigma_baseline)) - nVar)
  fitMeasures$baseline.df <- nVar
  fitMeasures$baseline.pvalue <- pchisq(fitMeasures$baseline.chisq, fitMeasures$baseline.df, lower.tail = FALSE)
  
  # Incremental Fit Indices
  Tb <- fitMeasures$baseline.chisq
  Tm <- fitMeasures$chisq
  
  dfb <- fitMeasures$baseline.df
  dfm <- fitMeasures$df
  
  fitMeasures$nfi <- (Tb - Tm) / Tb
  fitMeasures$tli <-  (Tb/dfb - Tm/dfm) / (Tb/dfb - 1) 
  fitMeasures$rfi <-  (Tb/dfb - Tm/dfm) / (Tb/dfb ) 
  fitMeasures$ifi <-  (Tb - Tm) / (Tb - dfm)
  fitMeasures$rni <-  ((Tb- dfb) - (Tm - dfm)) / (Tb - dfb)
  fitMeasures$cfi <- ifelse(dfm > Tm, 1, 1 - (Tm - dfm)/(Tb - dfb))
  
  # RMSEA
  fitMeasures$rmsea <- sqrt( max(Tm - dfm,0) / (Ncons * dfm))
  
  # Codes for rmsea confidence interval taken from lavaan:
  lower.lambda <- function(lambda) {
    (pchisq(Tm, df=dfm, ncp=lambda) - 0.95)
  }
  if(is.na(Tm) || is.na(dfm)) {
    fitMeasures$rmsea.ci.lower <- NA
  } else if(dfm < 1 || lower.lambda(0) < 0.0) {
    fitMeasures$rmsea.ci.lower <- 0
  } else {
    if (lower.lambda(0) * lower.lambda(Tm) > 0){
      lambda.l <- NA
    } else {
      lambda.l <- try(uniroot(f=lower.lambda, lower=0, upper=Tm)$root,
                      silent=TRUE)      
    }
    fitMeasures$rmsea.ci.lower <- sqrt( lambda.l/(sampleSize*dfm) )
  }
  
  N.RMSEA <- max(sampleSize, Tm*4) 
  upper.lambda <- function(lambda) {
    (pchisq(Tm, df=dfm, ncp=lambda) - 0.05)
  }
  if(is.na(Tm) || is.na(dfm)) {
    fitMeasures$rmsea.ci.upper <- NA
  } else if(dfm < 1 || upper.lambda(N.RMSEA) > 0 || upper.lambda(0) < 0) {
    fitMeasures$rmsea.ci.upper <- 0
  } else {
    
    if (upper.lambda(0) * upper.lambda(N.RMSEA) > 0){
      lambda.u <- NA
    } else {
      
      lambda.u <- try(uniroot(f=upper.lambda, lower=0,upper=N.RMSEA)$root,
                      silent=TRUE)  
    }
    
    if(inherits(lambda.u, "try-error")) {lambda.u <- NA }
    
    fitMeasures$rmsea.ci.upper <- sqrt( lambda.u/(sampleSize*dfm) )
  }
  
  fitMeasures$rmsea.pvalue <- 
    1 - pchisq(Tm, df=dfm, ncp=(sampleSize*dfm*0.05^2))
  
  # RMR:
  sqrt.d <- 1/sqrt(diag(covMat))
  D <- diag(sqrt.d, ncol=length(sqrt.d))
  R <- D %*% (covMat - Sigma) %*% D
  RR <- (covMat - Sigma)
  e <- nVar*(nVar+1)/2 + nVar
  
  fitMeasures$rmr <- sqrt( sum(RR[lower.tri(RR, diag=TRUE)]^2) / e )
  fitMeasures$srmr <-  sqrt( sum(R[lower.tri(R, diag=TRUE)]^2) / e )
  
  
  # information criteria:
  # Saturated log-likelihood:
  c <- sampleSize*nVar/2 * log(2 * pi)
  satLL <- ( -c -(sampleSize/2) * log(det(covMat)) - (sampleSize/2)*nVar )
  
  # log likelihood:
  LL <-  -sampleSize * (fitMeasures$fmin -  satLL/sampleSize)
  
  fitMeasures$logl <- LL
  fitMeasures$unrestricted.logl <- satLL
  
  fitMeasures$aic <-  -2*LL + 2* fitMeasures$npar
  
  BIC <- -2*LL + fitMeasures$npar * log(sampleSize)
  fitMeasures$bic <- BIC
  
  # add sample-size adjusted bic
  N.star <- (sampleSize + 2) / 24
  BIC2 <- -2*LL + fitMeasures$npar * log(N.star)
  fitMeasures$bic2 <- BIC2
  
  # Add extended bic:
  fitMeasures$ebic <-  -2*LL + fitMeasures$npar * log(sampleSize) + 4 *  fitMeasures$npar * ebicTuning * log(sampleSize)  
  
  
  # Results object:
  Results <- list(
    network = qgraph::wi2net(invSigma),
    fitMeasures = fitMeasures
  )
  class(Results) <- "ggmFit"
  return(Results)
}

print.ggmFit <- function(x,...){
  name <- deparse(substitute(x))[[1]]
  if (nchar(name) > 10) name <- "object"
  if (name=="x") name <- "object"
  
  cat("\nggmFit object:\n",
      paste0("Use plot(",name,") to plot the network structure"),
      "\n",
      paste0("Fit measures stored under ",name,"$fitMeasures"),
      "\n\n"
  )
  
  fit <- data.frame(Measure = names(x$fitMeasures),
                    Value = goodNum(unlist(x$fitMeasures)))
  rownames(fit) <- NULL
  print(fit)
}

plot.ggmFit <- function(x,...){
  qgraph::qgraph(x$network,...)
}
