\name{ggmFit}
\alias{ggmFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain fit measures of a Gaussian graphical model
}
\description{
Obtain fit measures of a given Gaussian graphical model (GGM). Input can be either a partial correlation matrix, inverse covariance matrix or \code{qgraph} object.
}
\usage{
ggmFit(pcor, covMat, sampleSize, refit = TRUE, ebicTuning = 0.5, 
        nPar, invSigma, tol = 1e-10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pcor}{
Implied partial correlation matrix or \code{qgraph} object.
}
  \item{covMat}{
Observed variance-covariance matrix
}
  \item{sampleSize}{
The sample size used in computing the variance-covariance matrix
}
  \item{refit}{
Logical, should the network be refitted using \code{\link[glasso]{glasso}}?
}
  \item{ebicTuning}{
EBIC tuning parameter.
}
  \item{invSigma}{
Implied inverse variance-covariance matrix. If this object is assigned \code{pcor} is not used.
}
  \item{nPar}{
    Number of parameters, if not specified this is retrieved from the number of zeroes in the inverse variance--covariance matrix. Can be used to compute fit measures of any statistical model (e.g., SEM). 
  }
    \item{tol}{
    Tolerance for setting an edge to zero.
  }
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

\examples{
library("psych")

# Load BFI data:
data(bfi)
bfi <- bfi[,1:25]

# Covariance matrix:
CovMat <- cov(bfi[,1:25], use="pairwise.complete.obs")

# Compute network:
EBICgraph <- qgraph(CovMat, graph = "glasso", sampleSize = nrow(bfi),
                    tuning = 0.5, layout = "spring", title = "BIC", details = TRUE)

# Obtain fit measures:
fitNetwork <- ggmFit(EBICgraph, CovMat, nrow(bfi))
fitNetwork
}
