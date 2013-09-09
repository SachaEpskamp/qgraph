## qgraph.pca

# EXPLORATORY FACTOR ANALYSIS
# Using factanal(..)

# all arguments of qgraph.loadings are included. plus:

# cor: correlation matrix on which to perform the EFA.
# factors: vector with how much factors are to be used
# rotation: rotations to be used.

qgraph.pca=function(cor,factors=1,...,rotation="promax",factorCors=TRUE) {

#require(psych)

if (any(class(cor)=="principal"))
{
	fact <- cor
	factors <- cor$factors
} else
{
	fact <- psych::principal(cor,factors,rotate=rotation)
}
loadings <- loadings(fact)
loadings <- as.matrix(loadings[1:nrow(loadings),1:factors])

if (factorCors) rcor <- fact$r.scores else rcor <- NULL

Q=qgraph.loadings(loadings,model="formative",factorCors=rcor,...) 
	
invisible(Q)
}
