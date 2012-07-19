\encoding{UTF8}
\name{qgraph.sem}
\alias{qgraph.sem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph: Structural Equation Modelling}
\description{
This functions uses a "sem" object from the \code{\link{sem}} function (from the sem package; Fox, 2010) and outputs a multi-page pdf file containing goodness of fit indices, path diagram, graphs of the parameter estimates and graphs of the implied and observed covariance and correlation matrices.
}
\usage{
qgraph.sem(res, layout="circle",  ..., vsize.man=3, vsize.lat=6, filename="qgraph",
	filetype="pdf", residuals=TRUE, panels=2, include=1:12, latres=TRUE, 
	curve=0, residSize=0.2, onefile=TRUE, width = 7, height = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{res}{The output from the 'sem' function. See details for extra details on specifying the model}
	\item{layout}{The layout used for the path diagram. Can be "tree", "spring", "circle" and "springtree"}
	\item{...}{arguments passed to \code{\link{qgraph}}. This is both for the path diagram and for the correlation/covariance plots.}
	\item{vsize.man}{Size of the manifest variables in the path diagram}
	\item{vsize.lat}{Size of the latent variables in the path diagram}
	\item{filename}{Name of the file (will be extended with the filetype)}
	\item{filetype}{The filetype to be used. Can be "pdf" to make a pdf (default) or anything else to plot in R. More filetypes will be supported in a future version.}
	\item{residuals}{Logical indicating if the residuals should be included in the path diagram. If this is FALSE then residual variances will be shown as colors on the nodes. Default is TRUE}
	\item{panels}{The number of panels to be used in the pdf file. Can be 1, 2, 4 or 8}
	\item{include}{A vector indicating which panels should be included in the output}
	\item{latres}{This is currently not supported, leave to TRUE}
	\item{curve}{Numerical value indicating the curve of edges that are on the same level in the "tree" layout, See details. This represent an offset from the middle of the straight edge through where the curved edge must be drawn. 0 indicates no curve, and any other value indicates a curve of that strength. Defaults to 0.2}
	\item{residSize}{Size of the residual arrows}
	\item{onefile}{Logical indicating if a multi-page pdf should be produced. If FALSE each plot will be a new pdf. Use this only with panels=1 and filename="(Arbitrary name)\%03d".}
	\item{width}{Width of each panel, in inches}
	\item{height}{Height of each panel, in inches}
}

\details{
This function uses a "sem" object and outputs a multi-page pdf file. 
The function reads the 'sem' file and creates a residual variable for each variable present 
in the model. The default layout is a tree-layout (layout="tree") in which each variable is 
placed as a node on one of four vertical levels. At the bottom are the residuals of the 
manifest variables placed, Above that the manifest variables, above that the latent variables 
and at the top the residuals of the latent variables. The nodes are evenly spaced horizontally 
in order of appearance in the model (residuals are placed at the same horizontal position of 
their corresponding variable). So the order of specifying in the model defines the order of 
placement in the path diagram. If the 'residuals' argument is FALSE then residuals are omitted 
and this diagram will only contain two levels.

Alternatively the 'spring' layout can be used (layout="spring"). This will use the Fruchterman-reingold algorithm (Fruchterman & Reingold, 1991) is used, which 
has been ported from the 'sna' package (Butts, 2010). This is a force-directed algorithm. It is best to use this in combination with residuals=FALSE.
	
Names for variables and edges used in the model specification are passed to the path diagram. 
To keep the model readable it is advised to limit these names to 3 characters. If a variable or 
edge name contains an asterisk, the asterisk is omitted and the name will be printed in the symbol 
font. This way Greek letters can be used (e.g. the edge name "l*" will make a lambda character). 
Because the symbol font conveniently uses Arabic numerals, parameter names like beta1 can be 
created with "*b1" in the model.
}
\references{
Sacha Epskamp, Angelique O. J. Cramer, Lourens J.
  Waldorp, Verena D. Schmittmann, Denny Borsboom (2012).
  qgraph: Network Visualizations of Relationships in
  Psychometric Data. Journal of Statistical Software,
  48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.

Carter T. Butts <buttsc@uci.edu> (2010). sna: Tools for Social Network Analysis. R package version 2.2-0. http://CRAN.R-project.org/package=sna

John Fox with contributions from Adam Kramer <jfox@mcmaster.ca> and Michael Friendly (2010). sem: Structural Equation Models. R package version 0.9-21. http://CRAN.R-project.org/package=sem

Fruchterman, T. & Reingold, E. (1991). Graph drawing by force-directed placement. Software - Pract. Exp. 21, 1129?1164.
}
\author{
Sacha Epskamp (qgraph@sachaepskamp.com)
}
\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.cfa}}
\code{\link{qgraph.semModel}}
\code{\link{sem}}
}
\examples{
\dontrun{

require('sem')

### This example is taken from the examples of the sem function. 
### Only names were changed to better suit the path diagram.

# ----------------------- Thurstone data ---------------------------------------
#  Second-order confirmatory factor analysis, from the SAS manual for PROC CALIS

R.thur <- readMoments(diag=FALSE, names=c('Sen','Voc',
        'SC','FL','4LW','Suf',
        'LS','Ped', 'LG'))
    .828                                              
    .776   .779                                        
    .439   .493    .46                                 
    .432   .464    .425   .674                           
    .447   .489    .443   .59    .541                    
    .447   .432    .401   .381    .402   .288              
    .541   .537    .534   .35    .367   .32   .555        
    .38   .358    .359   .424    .446   .325   .598   .452  
            
model.thur <- specifyModel()
    F1 -> Sen,               *l11, NA
    F1 -> Voc,               *l21, NA
    F1 -> SC,                *l31, NA
    F2 -> FL,                *l41, NA
    F2 -> 4LW,               *l52, NA
    F2 -> Suf,               *l62, NA
    F3 -> LS,                *l73, NA
    F3 -> Ped,               *l83, NA
    F3 -> LG,                *l93, NA
    F4 -> F1,                *g1,  NA
    F4 -> F2,                *g2,  NA
    F4 -> F3,                *g3,  NA 
    Sen <-> Sen,             q*1,   NA
    Voc<-> Voc,              q*2,   NA
    SC <-> SC,               q*3,   NA
    FL <-> FL,               q*4,   NA
    4LW <-> 4LW,             q*5,   NA
    Suf<-> Suf,              q*6,   NA
    LS <-> LS,               q*7,   NA
    Ped<-> Ped,              q*8,   NA
    LG <-> LG,               q*9,   NA
    F1 <-> F1,               NA,     1
    F2 <-> F2,               NA,     1
    F3 <-> F3,               NA,     1
    F4 <-> F4,               NA,     1

sem.thur <- sem(model.thur, R.thur, 213)

# Run qgraph:
qgraph.sem(sem.thur,filename="Thurstone tree",layout="tree",edge.label.cex=0.6,curve=0.4,width=8,height=5)

# Spring layout:
qgraph.sem(sem.thur,filename="Thurstone spring",layout="spring",residuals=FALSE,width=5,height=5)}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SEM}
\keyword{path diagram}
\keyword{graph}
\keyword{qgraph}% __ONLY ONE__ keyword per line

