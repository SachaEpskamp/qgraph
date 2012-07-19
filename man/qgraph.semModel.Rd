\name{qgraph.semModel}
\alias{qgraph.semModel}

\title{
qgraph: SEM model pathdiagram
}
\description{
This is a watered down version of \code{\link{qgraph.sem}} that only plots a diagram. It is called if a "mod" object is supplied to \code{\link{qgraph}}}

\usage{
qgraph.semModel(mod, manifest = NULL, layout = "spring", vsize.man = 3, vsize.lat = 6, residuals = TRUE, latres = TRUE, curve = 0.2, residSize = 0.2,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{ A "mod" object (model of the sem package (Fox; 2010) or a "sem" object}
  \item{manifest}{Vector containing the names of the manifest variables}
  \item{layout}{The layout used for the path diagram. Can be "tree", "spring", "circle" and "springtree". Defaults to "spring"}
  \item{vsize.man}{Size of the manifest variables in the path diagram}
  \item{vsize.lat}{Size of the latent variables in the path diagram}
  \item{residuals}{Logical indicating if the residuals should be included in the path diagram. If this is FALSE then residual variances will be shown as colors on the nodes. Default is TRUE}
  \item{latres}{This is currently not supported, leave to TRUE}
  \item{curve}{Numerical value indicating the curve of edges that are on the same level in the "tree" layout, See details. This represent an offset from the middle of the straight edge through where the curved edge must be drawn. 0 indicates no curve, and any other value indicates a curve of that strength. Defaults to 0.2}
  \item{residSize}{Size of the residual arrows}
  \item{\dots}{Arguments passed to \code{\link{qgraph}}}
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
Sacha Epskamp (qgraph@sachaepskamp.com)
}

\seealso{
\code{\link{qgraph}}
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



# Run qgraph:
qgraph(model.thur)

# Tree layout:
qgraph(model.thur,layout="tree",manifest=c('Sen','Voc','SC','FL','4LW','Suf','LS','Ped', 'LG'))

}}
