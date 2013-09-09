\encoding{UTF8}
\name{qgraph.lavaan}
\alias{qgraph.lavaan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph: Structural Equation Modelling}
\description{
This functions uses a "lavaan" object from the \code{\link{lavaan}} package (Rosseel, 2011) and outputs a multi-page pdf file containing path diagram, graphs of the parameter estimates and graphs of the implied and observed covariance and correlation matrices.
}
\usage{
qgraph.lavaan(
	fit,
	...,
	layout="circle",
	groups=NULL,
	vsize.man=3,
	vsize.lat=6,
	filename="qgraph",
	filetype="pdf",
	residuals=TRUE,
	include=1:12,
	curve=0,
	residSize=0.2,
	onefile=TRUE,
	width=12,
	height=8,
	titles=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{fit}{A \code{"\linkS4class{lavaan}"} object containing the fit of a SEM model (obtained from e.g. \code{\link[lavaan]{sem}} and \code{\link[lavaan]{cfa}})}
	\item{...}{arguments passed to \code{\link{qgraph}}. This is both for the path diagram and for the correlation/covariance plots.}
	\item{layout}{The layout used for the path diagram. Can be "tree", "spring", "circle" and "springtree"}
	\item{groups}{An optional list containing the measurement model, see  \code{\link{qgraph}}}
	\item{vsize.man}{Size of the manifest variables in the path diagram}
	\item{vsize.lat}{Size of the latent variables in the path diagram}
	\item{filename}{Name of the file (will be extended with the filetype)}
	\item{filetype}{The filetype to be used. Can be "pdf" to make a pdf (default) or anything else to plot in R. More filetypes will be supported in a future version.}
	\item{residuals}{Omitting residuals is currently not supported for qgraph.lavaan, leave this to TRUE}
	\item{include}{A vector indicating which panels should be included in the output}
	\item{curve}{Numerical value indicating the curve of edges that are on the same level in the "tree" layout, See details. This represent an offset from the middle of the straight edge through where the curved edge must be drawn. 0 indicates no curve, and any other value indicates a curve of that strength. Defaults to 0.2}
	\item{residSize}{Size of the residual arrows}
	\item{onefile}{Logical indicating if a multi-page pdf should be produced. If FALSE each plot will be a new pdf. Use this only with panels=1 and filename="(Arbitrary name)\%03d".}
	\item{width}{Width of each panel, in inches}
	\item{height}{Height of each panel, in inches}
	\item{titles}{Logical indicating if titles should be printed}
}

\note{This is the first isntallment of \code{\link{qgraph.sem}} for the Lavaan package. This function will likely be changed a lot in future installments.}

\details{
This function uses a \code{"\linkS4class{lavaan}"} object and outputs a multi-page pdf file. 
The function reads the 'lavaan' object and creates a residual variable for each variable present 
in the model. Layout options include a tree-layout (layout="tree") in which each variable is 
placed as a node on one of four vertical levels. At the bottom are the residuals of the 
manifest variables placed, Above that the manifest variables, above that the latent variables 
and at the top the residuals of the latent variables. The nodes are evenly spaced horizontally 
in order of appearance in the model (residuals are placed at the same horizontal position of 
their corresponding variable). So the order of specifying in the model defines the order of 
placement in the path diagram. If the 'residuals' argument is FALSE then residuals are omitted 
and this diagram will only contain two levels.

Alternatively the 'spring' layout can be used (layout="spring"). This will use the Fruchterman-reingold algorithm (Fruchterman & Reingold, 1991), which 
has been ported from the 'sna' package (Butts, 2010). This is a force-directed algorithm. It is best to use this in combination with residuals=FALSE. 
Another option is a circular layout (default), which is the same as the tree except that the levels are placed in inner circles rather than horizontal lines.
	
Names for variables used in the model specification are passed to the path diagram. 
To keep the model readable it is advised to limit these names to 3 characters. 
}
\references{
Sacha Epskamp, Angelique O. J. Cramer, Lourens J.
  Waldorp, Verena D. Schmittmann, Denny Borsboom (2012).
  qgraph: Network Visualizations of Relationships in
  Psychometric Data. Journal of Statistical Software,
  48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.

Carter T. Butts <buttsc@uci.edu> (2010). sna: Tools for Social Network Analysis. R package version 2.2-0. http://CRAN.R-project.org/package=sna

Yves Rosseel <Yves.Rosseel@UGent.be> (2011). lavaan: Latent Variable Analysis. R package version 0.4-7. http://CRAN.R-project.org/package=lavaan
  
Fruchterman, T. & Reingold, E. (1991). Graph drawing by force-directed placement. Software - Pract. Exp. 21, 1129?1164.
}
\author{
Sacha Epskamp (mail@sachaepskamp.com)
}
\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.cfa}}
\code{\link{qgraph.sem}}
\code{\link[lavaan]{sem}}
}
\examples{
\dontrun{
## The industrialization and Political Democracy Example 
# Example from lavaan::sem help file:
require("lavaan")
     ## Bollen (1989), page 332
     model <- ' 
       # latent variable definitions
          ind60 =~ x1 + x2 + x3
          dem60 =~ y1 + y2 + y3 + y4
          dem65 =~ y5 + equal("dem60=~y2")*y6 
                      + equal("dem60=~y3")*y7 
                      + equal("dem60=~y4")*y8
     
       # regressions
         dem60 ~ ind60
         dem65 ~ ind60 + dem60
     
       # residual correlations
         y1 ~~ y5
         y2 ~~ y4 + y6
         y3 ~~ y7
         y4 ~~ y8
         y6 ~~ y8
     '
     
     fit <- sem(model, data=PoliticalDemocracy)

# Plot standardized model (numerical):
qgraph.lavaan(fit,layout="tree",vsize.man=5,vsize.lat=10,
	filetype="",include=4,curve=-0.4,edge.label.cex=0.6)

# Plot standardized model (graphical):
qgraph.lavaan(fit,layout="tree",vsize.man=5,vsize.lat=10,
	filetype="",include=8,curve=-0.4,edge.label.cex=0.6)

# Create output document:
qgraph.lavaan(fit,layout="spring",vsize.man=5,vsize.lat=10,
	filename="lavaan")

}
}
\keyword{lavaan}
\keyword{cfa}
\keyword{SEM}
\keyword{path diagram}
\keyword{graph}
\keyword{qgraph}

