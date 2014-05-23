\name{cor_auto}
\alias{cor_auto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automatically compute an apppropriate correlation matrix
}
\description{
This is mainly a wrapper around Lavaan function \code{\link[lavaan]{lavCor}} (Rosseel, 2012) to compute a correlation matrix based on pychoric, polyserial and/or Pearson correlations. The wrapper removes all factors and searches for possible ordinal variabes. A variable is classified as ordinal if it is either \code{ordered} or if it consist of at most 7 unique integer values.
}
\usage{
cor_auto(data, select, detectOrdinal = TRUE, ordinalLevelMax = 7, npn.SKEPTIC = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame
}
  \item{select}{
Variables to select from the data frame (as used in \code{\link[base]{subset}})
}
  \item{detectOrdinal}{
Logical, should ordinal variables be detected? If \code{FALSE} only variables that are \code{ordered} are treated as ordinal variables
}
  \item{ordinalLevelMax}{
Integer specyfying the amount of unique integer values a variable should have to be classified as ordinal
}
  \item{npn.SKEPTIC}{
Logical, should the Nonparanormal SKEPTIC from the huge package be applied if the data is continuous? See \code{\link[huge]{huge.npn}} (Zhao, Liu, Roeder, Lafferty and Wasserman, 2014)
}
}

\value{
A correlation matrix
}
\references{
Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling.
Journal of Statistical Software, 48(2), 1-36. URL
http://www.jstatsoft.org/v48/i02/.

Tuo Zhao, Han Liu, Kathryn Roeder, John Lafferty and Larry Wasserman (2014). huge:
High-dimensional Undirected Graph Estimation. R package version 1.2.6.
http://CRAN.R-project.org/package=huge
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

\examples{
\dontrun{
### Examples from lavCor (lavaan): ###

library("lavaan")

# Holzinger and Swineford (1939) example
HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5",
                                 "x6","x7","x8","x9")]

# Pearson correlations
cor_auto(HS9)

# ordinal version, with three categories
HS9ord <- as.data.frame( lapply(HS9, cut, 3, labels=FALSE) )

# polychoric correlations, two-stage estimation
cor_auto(HS9ord)
}
}
