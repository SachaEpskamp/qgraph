## qgraph.efa

# EXPLORATORY FACTOR ANALYSIS
# Using factanal(..)

# all arguments of qgraph.loadings are included. plus:

# dat: datrelation matrix on which to perform the EFA.
# factors: vector with how much factors are to be used
# rotation: rotations to be used.

qgraph.efa <- function(dat,factors=1,...,rotation="promax",residuals=TRUE,factorCors=NULL,scores="regression",corMat=nrow(dat)==ncol(dat) && all(dat==t(dat))) {

if (any(class(dat)=="factanal"))
{
	fact <- dat
	if (is.null(fact$scores)) factorCors <- FALSE else factorCors <- TRUE
} else
	{
	if (is.null(factorCors))
	{
		if (corMat) factorCors = FALSE else factorCors = TRUE
	}

	if (corMat) 
	{
		if (factorCors) stop("Raw data must be assigned to 'dat' to correlate factors")
		fact <- factanal(covmat=dat,factors=factors,rotation=rotation)
	} else
	{
		if (factorCors) 
		{
			fact <- factanal(dat,factors=factors,rotation=rotation,scores=scores)
		} else fact <- factanal(dat,factors=factors,rotation=rotation)
	}
}

loadings <- as.matrix(loadings(fact)[1:length(fact$uniqueness),1:fact$factors])

if (residuals) res <- fact$uniqueness else res <- NULL

if (factorCors) rcor <- cor(fact$scores) else rcor <- NULL

Q <- qgraph.loadings(loadings,model="reflective",resid=res,factorCors=rcor,...) 
	
invisible(Q)
}