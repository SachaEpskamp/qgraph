\name{mat2vec}
\alias{mat2vec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weights matrix to vector
}
\description{
Converts a weights matrix to a vector of weights. If the matrix is symmetrical only upper triangle values are returned in the vector.
}
\usage{
mat2vec(x, diag = FALSE, tol = 1e-10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A weights matrix
}
  \item{diag}{
Logical: should diagonal values be included?
}
  \item{tol}{
Tolerance level
}
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}
