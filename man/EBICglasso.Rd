\name{EBICglasso}
\alias{EBICglasso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Gaussian graphical model using graphical lasso based on extended BIC criterium.
}
\description{
This function uses the \code{\link[glasso]{glasso}} package (Friedman, Hastie and Tibshirani, 2011) to compute a sparse gaussian graphical model with the graphical lasso (Friedman, Hastie \& Tibshirani, 2008). The tuning parameter is chosen using the Extended Bayesian Information criterium  (EBIC) described by Foygel \& Drton (2010). 
}


\usage{
EBICglasso(S, n, gamma = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
A covariance or correlation matrix
}
  \item{n}{
Sample size used in computing \code{S}
}
  \item{gamma}{
EBIC tuning parameter. 0.5 is generally a good choice. Setting to zero will cause regular BIC to be used.
}
  \item{\dots}{
Arguments sent to \code{\link[glasso]{glasso}}
}
}
\details{
The glasso is run for 100 values of the tuning parameter logarithmically spaced between the maximal value of the tuning parameter at which all edges are zero, lamba_max, and lambda_max/100. For each of these graphs the EBIC is computed and the graph with the best EBIC is selected. The partial correlation matrix is computed using \code{\link{wi2net}} and returned.
}
\value{
A partial correlation matrix
}
\references{
Friedman, J., Hastie, T., & Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. Biostatistics, 9(3), 432-441.
Chicago  

Jerome Friedman, Trevor Hastie and Rob Tibshirani (2011). glasso: Graphical lasso-estimation of Gaussian graphical models. R package version 1.7.
http://CRAN.R-project.org/package=glasso

Foygel, R., & Drton, M. (2010, November). Extended Bayesian Information Criteria for Gaussian Graphical Models. In NIPS (pp. 604-612).
Chicago  

Revelle, W. (2014) psych: Procedures for Personality and Psychological Research,
Northwestern University, Evanston, Illinois, USA,
http://CRAN.R-project.org/package=psych Version = 1.4.4.
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}
\examples{
\dontrun{
### Using bfi dataset from psych ###
library("psych")
data(bfi)

# Compute correlations:
CorMat <- cor_auto(bfi[,1:25])

# Compute graph with tuning = 0 (BIC):
BICgraph <- EBICglasso(CorMat, nrow(bfi), 0)

# Compute graph with tuning = 0.5 (EBIC)
EBICgraph <- EBICglasso(CorMat, nrow(bfi), 0.5)

# Plot both:
layout(t(1:2))
BICgraph <- qgraph(BICgraph, layout = "spring", title = "BIC", details = TRUE)
EBICgraph <- qgraph(EBICgraph, layout = "spring", title = "EBIC")

# Compare centrality and clustering:
layout(1)
centralityPlot(list(BIC = BICgraph, EBIC = EBICgraph))
clusteringPlot(list(BIC = BICgraph, EBIC = EBICgraph))
}
}

