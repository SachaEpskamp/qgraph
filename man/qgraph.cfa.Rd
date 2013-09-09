\name{qgraph.cfa}
\alias{qgraph.cfa}

\title{
CFA using Structural Equation Modelling
}
\description{
This function performs a simple confirmatory factor analysis using sem (Fox, 2010) or lavaan (Rosseel, 2011).
}
\usage{
qgraph.cfa(S, N, groups=NULL, ..., pkg = "sem", labels=NULL, fun = qgraph, opts = list())
}
\arguments{
  \item{S}{A covariance matrix}
  \item{N}{The number of observations}
  \item{groups}{The groups list, see \code{\link{qgraph}}. This must be a list in which each element is a factor indicating which variables load on the same factor}
  \item{\dots}{Arguments passed to 'fun'}
  \item{pkg}{A string indicating which package should be used for estimating the model. Currently "lavaan" and "sem" (default) are supported}
  \item{labels}{A vector indicating the label of each variable}
  \item{fun}{A function to which the results are send. Defaults to qgraph, but can be any function that can handle the output (e.g. qgraph.lavaan, qgraph.sem, summary, print). If pkg="sem" then this can also be qgraph.loadings.}
  \item{opts}{A list containing arguments that are sent to either \code{\link[sem]{sem}} or \code{\link[lavaan]{cfa}} 
  }}
\details{
This function can be used to perform a simple confirmatory factor analasys using regular qgraph input. 
The function computes a model and then sends it to \code{\link[sem]{sem}} (sem; Fox, 2010) or \code{\link[lavaan]{cfa}} (lavaan; Rosseel, 2011). 
based on the package used either a "sem" object or a "lavaan" object is returned that can be used for manual inspection or to sent to 
\code{\link{qgraph.sem}} or \code{\link{qgraph.lavaan}}.
 
The model that is estimated is a first order factor model in which each variable loads on one factor and the factors 
are correlated. This model is specified with the `groups` argument. This must be a list in which each element 
represents a factor. Each element of the list must be a vector indicating which variables load on the same factor. 
The model is identified by fixing the first loading of each factor to 1, which should be an identifying restriction 
if there are at least 4 variables per factor.

The function also sends its results to another function for visualization. If this is 
\code{\link{qgraph}}, the default, then  a visualization of the standardized coeficients is plotted.

Currently the sem package is better supported in qgraph, but this will change in a future version. Using the lavaan package 
can greatly reduce computation time.
}
\value{
A "sem" object, see \code{\link{sem}}
}
\references{
Sacha Epskamp, Angelique O. J. Cramer, Lourens J.
  Waldorp, Verena D. Schmittmann, Denny Borsboom (2012).
  qgraph: Network Visualizations of Relationships in
  Psychometric Data. Journal of Statistical Software,
  48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.

John Fox with contributions from Adam Kramer <jfox@mcmaster.ca> and Michael Friendly (2010). sem: Structural Equation Models. R package version 0.9-21. http://CRAN.R-project.org/package=sem
}
\author{
Sacha Epskamp (mail@sachaepskamp.com)
}

\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.sem}}
\code{\link{qgraph.lavaan}}
\code{\link{qgraph.loadings}}
\code{\link{qgraph.semModel}}
\code{\link{sem}}
}
\examples{
\dontrun{
# Simulate dataset:
set.seed(2)
eta<-matrix(rnorm(200*5),ncol=5)
lam<-matrix(rnorm(50*5,0,0.15),50,5)
lam[apply(diag(5)==1,1,rep,each=10)]<-rnorm(50,0.7,0.3)
th<-matrix(rnorm(200*50),ncol=50)
Y<-eta\%*\%t(lam)+th

# Create groupslist
gr<-list(1:10,11:20,21:30,31:40,41:50)

# Using "lavaan" package:
res <- qgraph.cfa(cov(Y),N=200,groups=gr,pkg="lavaan",vsize.man=2,vsize.lat=10)

qgraph.lavaan(res,filename="lavaan",legend=FALSE,groups=gr,edge.label.cex=0.6)

# Using "sem" package:
res <- qgraph.cfa(cov(Y),N=200,groups=gr,pkg="sem",vsize.man=2,vsize.lat=10,fun=qgraph.loadings)

qgraph.semModel(res,edge.label.cex=0.6)

qgraph(res,edge.label.cex=0.6)

qgraph.sem(res,filename="sem",legend=FALSE,groups=gr,edge.label.cex=0.6)

### Big 5 dataset ###
data(big5)
data(big5groups)

fit <- qgraph.cfa(cov(big5),nrow(big5),big5groups,pkg="lavaan",opts=list(se="none"),
	vsize.man=1,vsize.lat=6,edge.label.cex=0.5)
print(fit)
}
}
\keyword{qgraph}
\keyword{sem}
\keyword{cfa}
