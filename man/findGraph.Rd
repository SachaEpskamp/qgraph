\name{findGraph}
\alias{findGraph}

\title{
Search for optimal correlation or partial correlation network
}
\description{
This function can be used to find an optimal correlation or partial correlation network according to extended BIC (EBIC; Foygel and Drton, 2010). The functions \code{\link[ggm]{fitCovGraph}} and \code{\link[ggm]{fitConGraph}} from the ggm package are used in computing these networks (Marchetti, Drton and Sadeghi, 2014).
}
\usage{
findGraph(S, n, type = "cor", gamma = 0.5, method = c('stepup','stepdown','brute'), 
          reverseSteps = TRUE, startSig = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
A sample covariance or correlation matrix. Or a data frame, in which case \code{\link{cor_auto}} will be used.
}
  \item{n}{
The sample size
}
  \item{type}{
\code{"cor"} for estimating a correlation network or \code{"pcor"} for estimating a partial correlation network
}
  \item{gamma}{
The EBIC tuning parameter
}
  \item{method}{
\code{"brute"} for brute force search (testing all possible models), \code{"stepup"} for stepwise up model search and \code{"stepdown"} for stepwise down model search.
}
  \item{reverseSteps}{
Logical. If \code{method} is \code{"stepup"} or \code{"stepdown"}, should the stepping be reversed if a minimum is found? For example, if in stepwise up search a best model is found, should the search be continued by looking at if different edges could be deleted?
}
  \item{startSig}{
Logical. If \code{TRUE} the initial model in if \code{method} is \code{"stepup"} or \code{"stepdown"} is the model in which all edges that are insignificant using Holm adjustment are deleted. Otherwise,  \code{"stepup"} will start with an empty graph and  \code{"stepdown"} with a fully connected graph.
}
}
\details{
Due to the length of computing these models, \code{\link{EBICglasso}} should be preferred in larger datasets.
}
\value{
A (partial) correlation matrix
}
\references{
Foygel, R., & Drton, M. (2010). Extended bayesian information criteria for gaussian graphical models. In Advances in Neural Information Processing Systems (pp. 604-612). Chicago  

Giovanni M. Marchetti, Mathias Drton and Kayvan Sadeghi (2014). ggm: A package for Graphical Markov Models. R package version 2.0. http://CRAN.R-project.org/package=ggm
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

