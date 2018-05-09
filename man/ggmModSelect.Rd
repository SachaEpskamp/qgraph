\name{ggmModSelect}
\alias{ggmModSelect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Unregularized GGM model search
}
\description{
This function will search for an optimal Gaussian graphical model by minimizing the (extended) Bayesian information criterion of unregularized GGM models. Selecting unregularized GGMs according to EBIC has been shown to converge to the true model (Foygel & Drton, 2010). The algorithm starts with refitting models from the glassopath, and subsequently adds and removes edges until EBIC can no longer be improved (see details). Note, contrary to \code{\link{EBICglasso}}, the default for the EBIC hyperparameter gamma is set to 0, indicating BIC model selection.
}
\usage{
ggmModSelect(S, n, gamma = 0, start = c("glasso", "empty", "full"), stepwise = TRUE, 
  considerPerStep = c("subset", "all"), verbose = TRUE, nCores = 1, checkPD = TRUE, 
  criterion = 'ebic', ...)
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
EBIC tuning parameter. 0 (default) leads to BIC model selection. 0.25 or 0.5 are typical choices for more conservative model selection.
}
  \item{start}{
What model should stepwise search start from? \code{"glasso"} to first run glasso to obtain the best fitting model, \code{"empty"} for an empty network, \code{"full"} for a saturated network, or a matrix encoding the starting network.
}
  \item{stepwise}{
Logical indicating if stepwise model search should be used.
}
  \item{considerPerStep}{
\code{"subet"} to only consider changing edges that previously indicated improvement in EBIC, unless changing no edge indicated an improvement to EBIC, in which case all edges are again considered (see details). \code{"all"} will consider changing all edges at every step.
} 
  \item{verbose}{
Logical, should progress reports be printed to the console?
}
  \item{nCores}{
The number of cores to use in testing models.
}
  \item{checkPD}{
If \code{TRUE}, the function will check if \code{S} is positive definite and return an error if not. It is not advised to use a non-positive definite matrix as input as (a) that can not be a covariance matrix and (b) glasso can hang if the input is not positive definite.
}
\item{criterion}{
String indicating an output of \code{\link{ggmFit}} to be minimized
}
  \item{\dots}{
Arguments sent to \code{\link[glasso]{glasso}}
}
}
\details{
The full algorithm is as follows:

1. Run glasso to obtain 100 models

2. Refit all models without regularization

3. Choose the best according to EBIC

4. Test all possible models in which one edge is changed (added or removed)

5. If no edge can be added or changed to improve EBIC, stop here

6. Change the edge that best improved EBIC, now test all other edges that would have also lead to an increase in EBIC again

7. If no edge can be added or changed to improve EBIC, go to 4, else, go to 6.

When \code{stepwise = FALSE}, steps 4 to 7 are ignored. When \code{considerPerStep = "all"}, all edges are considered at every step. Note that this algorithm is very slow in higher dimensions (e.g., above 30-40 nodes). Note that EBIC computation is slightly different as in \code{\link{EBICglasso}} and instead follows the implementation in Lavaan.
}

\value{
A list with the following elements:

\item{graph}{The optimal partial correlation network}
\item{EBIC}{EBIC corresponding to optimal network.}
}
\references{
Foygel, R., & Drton, M. (2010). Extended Bayesian information criteria for Gaussian graphical models. In Advances in neural information processing systems (pp. 604-612).
}
\author{
Sacha Epskamp
}

\examples{
\dontrun{
# Load data:
library("psych")
data(bfi)

# Compute polychoric correlations:
corMat <- cor_auto(bfi[,1:25])

# Optimize network:
Results <- ggmModSelect(corMat, nrow(bfi), gamma = 0.5, nCores = 8)

# Plot results:
qgraph(Results$graph, layout = "spring", cut = 0)
}
}
