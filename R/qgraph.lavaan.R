
qgraph.lavaan <- function(
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
{
  warning("This funcion is deprecated,\nUse the 'semPlot' package instead.")
#reqTest <- require("lavaan")
#if (!reqTest) stop("lavaan could not be loaded, is this package installed?")

if (fit@SampleStats@ngroups > 1) stop("qgraph.lavaan currently does not support models based on multiple groups")

# Get #vars and #factors:
#n <- nrow(fit@Model@GLIST$lambda)
#k <- ncol(fit@Model@GLIST$lambda)


# Create empty adjacency and bidir/curve:
#E <- list(
	#adj = matrix(0,2*(n+k),2*(n+k)),
	#bidir = matrix(FALSE,2*(n+k),2*(n+k)),
	#curv = matrix(0,2*(n+k),2*(n+k)),
	#lty = matrix(1,2*(n+k),2*(n+k))
#)
# Insert Lambda:
#E$adj[(n+1):(n+k),1:n] <- t(fit@Model@GLIST$lambda)

# Insert diag theta:
#diag(E$adj[(n+k+1):(n+k+n),1:n]) <- diag(fit@Model@GLIST$theta)

# Insert Beta:
#if (!is.null(fit@Model@GLIST$beta)) E$adj[(n+1):(n+k),(n+1):(n+k)] <- t(fit@Model@GLIST$beta)
#if (!is.null(fit@Model@GLIST$beta)) E$curv[(n+1):(n+k),(n+1):(n+k)] <- curve

# Insert Theta:
#E$adj[(n+k+1):(n+k+n),(n+k+1):(n+k+n)] <- fit@Model@GLIST$theta
#E$bidir[(n+k+1):(n+k+n),(n+k+1):(n+k+n)] <- TRUE

# Insert diag psi:
#diag(E$adj[n+k+n+1:k,n+1:k]) <- diag(fit@Model@GLIST$psi)

# Insert psi:
#E$adj[n+k+n+1:k,n+k+n+1:k] <- fit@Model@GLIST$psi
#E$bidir[n+k+n+1:k,n+k+n+1:k] <- TRUE

# Clear diag:
#diag(E$adj) <- 0

# Set lty:
#for (i in 1:k) lty[n+i,i*k-k+1] <- 2

# Create model matrix:
#E$model <- 1*E$adj!=0

# Let's fool R CMD check!
to <- from <- rhs <- bidir <- op <- NULL

if (is.factor(groups) | is.character(groups)) groups <- tapply(1:length(groups),groups,function(x)x)

if (!is.null(groups))
{
	reorder <- as.vector(unlist(groups))
	groups <- lapply(groups,match,reorder)

}

if (class(fit)!="lavaan") stop("Input must me a 'lavaan' object")

arguments=list(...)
if(is.null(arguments$layout.par)) layout.par=list() else layout.par=arguments$layout.par


output <- paste(filename,".pdf",sep="")

if (filetype=="pdf")
{
	pdf(output,height=height,width=width,onefile=onefile)
} else
{
	width <- NULL
	height <- NULL
}


# First pane: Model statistics
#if (1%in%include)
#{
#par(mar=c(1,1,1,1))
#plot(1, ann = FALSE, axes = FALSE, xlim = c(0, 100), ylim = c(0, 100),
#     type = "n", xaxs = "i", yaxs = "i")
#title("Model statistics")
#text(50,80,paste("Model:
#Observed variables:",res$n,"/",res$m,"
#Number of free parameters:",res$t,"
#Number of observations:",res$N,"
#Number of fixed exogenous variables:",res$n.fix,"
#
#Iterations:",res$iterations,"
#
#Goodness of Fit:
#Chisq:",round(summary(res)$chisq,5),", df:",summary(res)$df,", p=",round(1-pchisq(summary(res)$chisq,summary(res)$df),5),"
#RMSEA:",round(summary(res)$RMSEA[1],5)," (",summary(res)$RMSEA[4]*100,"% CI:",round(summary(res)$RMSEA[2],5)," - ",round(summary(res)$RMSEA[3],5),")
#Goodness-of-fit index:",round(summary(res)$GFI,5),"
#Adjusted goodness-of-fit index:",round(summary(res)$AGFI,5),"
#Bentler-Bonnett NFI:",round(summary(res)$NFI,5),"
#Tucker-Lewis NNFI:",round(summary(res)$NNFI,5),"
#Bentler CFI:",round(summary(res)$CFI,5),"
#SRMR:",round(summary(res)$SRMR,5),"
#BIC:",round(summary(res)$BIC,5)),pos=1)
#}	 


# Extract parameter estimates:
pars <- parameterEstimates(fit,standardized=TRUE)

# Remove mean structure (TEMP SOLUTION)
meanstructure <- pars$op=="~1"
pars <- pars[!meanstructure,]

# Extract variable and factor names:
# varNames <- fit@Model@dimNames$lambda[[1]]
# factNames <- fit@Model@dimNames$lambda[[2]]
Lambda <- inspect(fit,"coef")$lambda
varNames <- rownames(Lambda)
factNames <- colnames(Lambda)
rm(Lambda)

factNames <- factNames[!factNames%in%varNames]

# Extract number of variables and factors
n <- length(varNames)
k <- length(factNames)

# Create edges dataframe
E <- data.frame(
	from = match(pars$lhs,c(varNames,factNames)),
	to = match(pars$rhs,c(varNames,factNames)),
	weight = pars$est,
	stand = pars$std.lv,
	standAll = pars$std.all,
  standNox = pars$std.nox,
	lty = 1,
	curved = 0,
	bidir = FALSE
)

curve <- -1 * curve

# Some more transformations of the edge dataframe:
E$from[pars$op=="~~"] <- E$from[pars$op=="~~"]+n+k
E$to[pars$op=="~~" & pars$lhs != pars$rhs] <- E$to[pars$op=="~~" & pars$lhs != pars$rhs]+n+k
E$bidir[pars$op=="~~"  & pars$lhs != pars$rhs] <- TRUE
E$curved[pars$op=="~" & ((pars$lhs %in% varNames & pars$rhs %in% varNames) | (pars$lhs %in% factNames & pars$rhs %in% factNames))] <- curve
E[pars$op=="~",1:2] <- E[pars$op=="~",2:1]
if (layout=="tree") E$curved[pars$op=="~~"  & pars$lhs != pars$rhs] <- curve
E$lty[fit@ParTable$free[!meanstructure]==0] <- 2

E <- rbind(E,transform(subset(E,bidir),from=to,to=from))
#E <- rbind(E,transform(E[E$bidir,],from=to,to=from))

# Create layout:
l <- layout
if (layout=='tree' | layout=="springtree" | layout=="circle") 
{ 
	l <- matrix(0,nrow=2*(n+k),ncol=2)
	l[n+k+seq_len(n),2] <- -1
	l[seq_len(n),2] <- -0.5
	l[n+seq_len(k),2] <- 0.5
	l[2*n+k+seq_len(k),2] <- 1

	l[n+k+seq_len(n),1]=seq(-1,1,length=n+2)[-c(1,n+2)]
	l[seq_len(n),1]=seq(-1,1,length=n+2)[-c(1,n+2)]
	l[n+seq_len(k),1]=seq(-1,1,length=k+2)[-c(1,k+2)]
	l[2*n+k+seq_len(k),1]=seq(-1,1,length=k+2)[-c(1,k+2)]

  E$curved[E$curved!=0 & l[E$from,2] > 0 & l[E$to,2] > 0 ] <- -1 * E$curved[E$curved!=0 & l[E$from,2] > 0 & l[E$to,2] > 0 ]
  
	#E$curved=0
	#E$fromtype=0
	#E$totype=0

	#un=unique(l[,2])
	#for (i in 1:length(un)) 
	#{
	#	E$fromtype[E$from%in%V$ID[which(l[,2]==un[i])]]=i
	#	E$totype[E$to%in%V$ID[which(l[,2]==un[i])]]=i
	#}

	#E$curved[E$fromtype==E$totype]=curve

	# Reorder residuals:
	
}

if (layout=='circle' | layout=='circulair') 
{
	l2 <- l
	
	#E$curved <- 0

	tl=sum(l[,2] == -0.5)+1
	l2[l[,2] == -0.5,1]=sin(seq(0,2*pi, length=tl))[-tl]
	l2[l[,2] == -0.5,2]=cos(seq(0,2*pi, length=tl))[-tl]
	
	tl=sum(l[,2] == 0.5)+1
	l2[l[,2] == 0.5,1]=0.5*sin(seq(0,2*pi, length=tl)+(pi/(tl-1)))[-tl]
	l2[l[,2] == 0.5,2]=0.5*cos(seq(0,2*pi, length=tl)+(pi/(tl-1)))[-tl]

	srt <- sort(l[l[,2] == -1,1],index.return=TRUE)$ix
	l2[which(l[,2]==-1)[srt],] <- (1+residSize) * l2[l[,2] == -0.5,]
	
	srt <- sort(l[l[,2] == 1,1],index.return=TRUE)$ix
	l2[which(l[,2] == 1)[srt],] <-  (1-2*residSize) * l2[l[,2] == 0.5,]

	
	l <- l2
} else if (layout != "spring")
{
	l[l[,2]==-1,2] <- -0.5 - residSize
	l[l[,2]==1,2] <- 0.5 + residSize
}

if (layout=="springtree")
{
	l[,1] <- NA
	layout.par$constraints <- l
	l <- "spring"
}

V <- data.frame(
	labels = c(varNames,factNames,rep("",n+k)),
	shape = c(rep("square",n),rep("circle",k)),
	size = c(rep(vsize.man,n),rep(vsize.lat,k),rep(0,n+k)),
	border.colors = c(rep("black",n+k),rep("#00000000",n+k))
	)
	

# RUN QGRAPH FOR MODEL:
if (1%in%include) DNPL <- FALSE else DNPL <- TRUE

edgelist <- as.matrix(cbind(E$from,E$to))

Q=qgraph(
	edgelist,
	layout=l,
	edge.labels=E$label,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty,
	border.colors=as.character(V$border.colors),
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=E$bidir,
	filetype="",
	esize=1,
	width=width,
	height=height,
	DoNotPlot=DNPL,
	groups=NULL,
	...)
if (1%in%include & titles) title("Specified model",line=-1)	


# RUN QGRAPH FOR ABSOLUTE PARAMETER ESTIMATES:
if (2%in%include)
{

qgraph(
	edgelist,
	layout=Q$layout,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty,
	border.colors=as.character(V$border.colors),
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=E$bidir,
	filetype="",
	esize=1,
	edge.labels=round(E$weight,2),
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
if (titles) title("Unstandardized model",line=-1)
}

# RUN QGRAPH FOR STANDARDIZED PARAMETER ESTIMATES:
if (3%in%include)
{
	
qgraph(
	edgelist,
	layout=Q$layout,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty,
	border.colors=as.character(V$border.colors),
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=E$bidir,
	filetype="",
	esize=1,
	edge.labels=round(E$stand,2),
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
if (titles) title("Standardized model (std.lv)",line=-1)
}

if (4%in%include)
{
	
qgraph(
	edgelist,
	layout=Q$layout,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty,
	border.colors=as.character(V$border.colors),
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=E$bidir,
	filetype="",
	esize=1,
	edge.labels=round(E$standAll,2),
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
if (titles) title("Standardized model (std.all)",line=-1)
}

if (5%in%include)
{
  
qgraph(
	edgelist,
	layout=Q$layout,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty,
	border.colors=as.character(V$border.colors),
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=E$bidir,
	filetype="",
	esize=1,
	edge.labels=round(E$standNox,2),
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
if (titles) title("Standardized model (std.nox)",line=-1)
}

# RUN QGRAPH FOR WEIGHTED ESTIMATES:

if (6%in%include)
{
edgelist <- as.matrix(cbind(E$from,E$to,E$weight))
qgraph(
	edgelist,
	layout=Q$layout,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty,
	border.colors=as.character(V$border.colors),
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=E$bidir,
	filetype="",
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
if (titles) title("Unstandardized model",line=-1)
}

if (7%in%include)
{
edgelist <- as.matrix(cbind(E$from,E$to,E$stand))
qgraph(
	edgelist,
	layout=Q$layout,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty,
	border.colors=as.character(V$border.colors),
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=E$bidir,
	filetype="",
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
if (titles) title("Standardized model (std.lv)",line=-1)
}

if (8%in%include)
{
edgelist <- as.matrix(cbind(E$from,E$to,E$standAll))
qgraph(
	edgelist,
	layout=Q$layout,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty,
	border.colors=as.character(V$border.colors),
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=E$bidir,
	filetype="",
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
if (titles) title("Standardized model (std.all)",line=-1)
}

if (9%in%include)
{
edgelist <- as.matrix(cbind(E$from,E$to,E$standNox))
qgraph(
  edgelist,
	layout=Q$layout,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty,
	border.colors=as.character(V$border.colors),
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=E$bidir,
	filetype="",
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
if (titles) title("Standardized model (std.nox)",line=-1)
}



# COVARIANCES and CORRELATIONS:

covImp <- round(as.matrix(fit@Fit@Sigma.hat[[1]]),6)
corImp <- round(cov2cor(covImp),6)
covObs <- round(as.matrix(fit@SampleStats@cov[[1]]),6)
corObs <- round(cov2cor(covObs),6)

if (is.null(groups))
{
	gr <- as.factor(subset(pars,op=="=~" & rhs%in%varNames)$lhs)
  #gr <- as.factor(pars[pars$op=="=~" & pars$rhs%in%varNames,]$lhs)
	if (length(gr) != nrow(covObs)) gr <- NULL
} else gr <- groups

maximum=max(abs(c(covObs[upper.tri(covObs)],covImp[upper.tri(covImp)])))	

if (any(10:12 %in% include))
{
	layout(t(1:2))
	par(pty="s")
}

if (10%in%include)
{
qgraph(
	covObs, 
	labels=rownames(covObs), 
	filetype="",
	maximum=maximum,
	diag="col",
	groups=gr,
	legend=FALSE,
	...)
if (titles) title("Observed covariances",line=-1)

qgraph(
	covImp, 
	labels=rownames(covImp), 
	filetype="",
	maximum=maximum,
	diag="col",
	groups=gr,
	legend=FALSE,
	...)
if (titles) title("Implied covariances",line=-1)
}


maximum=max(abs(c(corObs[upper.tri(corObs)],corImp[upper.tri(corImp)])))	

if (11%in%include)
{
qgraph(
	corObs, 
	labels=rownames(corObs), 
	filetype="",
	maximum=maximum,
	groups=gr,
	legend=FALSE,
	...)
if (titles) title("Observed correlations",line=-1)

qgraph(
	corImp, 
	labels=rownames(corImp), 
	filetype="",
	maximum=maximum,
	groups=gr,
	legend=FALSE,
	...)
if (titles) title("Implied correlations",line=-1)
}

covResids <- covObs - covImp
corResids <- corObs - corImp

if (12%in%include)
{
qgraph(
	covResids, 
	labels=rownames(covResids), 
	filetype="",
	diag="col",
	groups=gr,
	legend=FALSE,
	...)
if (titles) title("Residual covariances",line=-1)

qgraph(
	corResids, 
	labels=rownames(corResids), 
	filetype="",
	groups=gr,
	legend=FALSE,
	...)
if (titles) title("Residual correlations",line=-1)
}

if (filetype=="pdf")
{
dev.off()
print(paste("Output stored in ",getwd(),"/",output,sep=""))
}
invisible(Q)
}

