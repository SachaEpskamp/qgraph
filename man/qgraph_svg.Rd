\name{qgraph.svg}
\alias{qgraph.svg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph.svg}
\description{
IMPORTANT NOTE: RSVGTipsDevice is a 32-bit only package, so SVG functionality is not available in 64bit versions of R.

This function makes a series of SVG images with hyperlinks to each other using the RSVGTipsDevice package (Plate, 2009). 
The arguments are the same as \code{\link{qgraph}} except that some arguments
can be assigned a vector with multiple options.
}
\usage{
qgraph.svg( input, layout=c( "circular", "spring" ), 
	graph=c( "association", "concentration", "factorial" ),	
	cut=c( 0, 0.2, 0.3, 0.5), filename="qgraph", title="qgraph output",	
	nfact=round(ncol(input)/2,0),	tooltips=NULL,	... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{input}{A weights matrix (correlation matrix is recommended). See \code{\link{qgraph}}}
	\item{layout}{The layout of the graph, see \code{\link{qgraph}}, unlike \code{\link{qgraph}} it is not possible to assign a specific layout.}
	\item{graph}{The graph to be made based on the correlation matrix. See \code{\link{qgraph}}}
	\item{cut}{The cutoff score. See \code{\link{qgraph}}}
	\item{filename}{The name of the SVG files.}
	\item{title}{The title that will be printed at the top of the pictures}
	\item{nfact}{The number of factors that will be used in the EFA that makes the factorial graph. See \code{\link{qgraph}}}
	\item{tooltips}{A vector with for each node the tooltip to be used.}
	\item{...}{Additional arguments that are passed to \code{\link{qgraph}}}
}
\details{
This function works the same as \code{\link{qgraph}} except that the arguments 'layout', 'graph' and 'cut' can be given
a vector with multiple options. The  RSVGTipsDevice package (Plate, 2009) is used to accomplish this. A
SVG picture will be created for each combination of the arguments 'layout', 'graph' and 'cut', with hyperlinks between
them}

\references{
Sacha Epskamp, Angelique O. J. Cramer, Lourens J.
  Waldorp, Verena D. Schmittmann, Denny Borsboom (2012).
  qgraph: Network Visualizations of Relationships in
  Psychometric Data. Journal of Statistical Software,
  48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.

Plate, T. <tplate@acm.org> and based on RSvgDevice by T Jake Luciani <jakeluciani@yahoo.com> (2009). RSVGTipsDevice: An R SVG graphics device with dynamic tips and hyperlinks. R package version 1.0-1.
}
\author{
Sacha Epskamp (mail@sachaepskamp.com)
}
\seealso{
\code{\link{qgraph}}
}
\examples{
\dontrun{
#### VISUALIZE CORRELATION MATRIX ###
eta=matrix(rnorm(200*5),ncol=5)
lam=matrix(0,nrow=100,ncol=5)
for (i in 1:5) lam[(20*i-19):(20*i),i]=rnorm(20,0.7,0.3)
eps=matrix(rnorm(200*100),ncol=100)
Y=eta\%*\%t(lam)+eps

tooltips=paste("item",1:100)
groups=list(1:20,21:40,41:60,61:80,81:100)
names(groups)=paste("Factor",LETTERS[1:5])
# Run qgraph:
qgraph.svg(cor(Y),groups=groups,tooltips=tooltips,vsize=3)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ qgraph}
\keyword{ SVG}
