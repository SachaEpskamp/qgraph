\name{VARglm}
\alias{VARglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes a vector autoregressive lag-1 model using GLM
}
\description{
This function computes a VAR model using glm.
}
\usage{
VARglm(x, family, vars, adjacency, icfun = BIC, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A data frame
}
  \item{family}{
The family to be used. Defaults to \code{gaussian} if data is continuous or \code{binomial} if data is binary
}
  \item{vars}{
Vector of variables to predict. If missing all variables are predicted.
}
  \item{adjacency}{
Adjacency matrix. If missing full network is estimated
}
  \item{icfun}{
Information criterium function to be included in the output
}
  \item{\dots}{
Arguments used in the \code{icfun}
}
}
\value{
A list containing:
\item{graph}{The estimated graph}
\item{IC}{The information criterium}
}
\author{
Sacha Epskamp <mail@sachaepskamp.com
}
