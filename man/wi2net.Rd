\name{wi2net}
\alias{wi2net}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts precision matrix to partial correlation matrix
}
\description{
A small function that converts a precision matrix (inverse of covariance matrix) to a partial correlation matrix. This can be done by standardizing the precision matrix and changing the sign of the off-diagonal entries. Many methods exist for obtaining a precision matrix (such as the glasso package; Friedman, Hastie and Tibshirani, 2011) but the partial correlation matrix is easier to interpret and more usable in qgraph.
}
\usage{
wi2net(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A precision matrix
}
}
\value{
A partial correlation matrix with diagonal set to zero, returned as a symmetric matrix of class \code{dsyMatrix} from the Matrix package (use \code{as.matrix} to convert to a base matrix)
}
\references{
Jerome Friedman, Trevor Hastie and Rob Tibshirani (2011). glasso: Graphical lasso-estimation of Gaussian graphical models. R package version 1.7.
http://CRAN.R-project.org/package=glasso
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}