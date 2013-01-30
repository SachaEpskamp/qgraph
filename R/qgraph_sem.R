
qgraph.sem=function(
	res,
	layout="circle",
	...,
	vsize.man=3,
	vsize.lat=6,
	filename="qgraph",
	filetype="pdf",
	residuals=TRUE,
	panels=2,
	include=1:12,
	latres=TRUE,
	curve=0,
	residSize=0.2,
	onefile=TRUE,
	width=7,
	height=7)
{
  warning("This funcion is deprecated,\nUse the 'semPlot' package instead.")
#reqTest <- require("sem")
#if (!reqTest) stop("sem could not be loaded, is this package installed?")
if (!any(class(res)%in%c("sem","semmod"))) stop("Input must be a 'sem' object")

arguments=list(...)
if(is.null(arguments$layout.par)) layout.par=list() else layout.par=arguments$layout.par

if (!(panels%in%c(1,2,4,8))) stop("Only 1, 2, 4 and 8 panels are supported")

output=paste(filename,".pdf",sep="")

if (filetype=="pdf")
{
if (panels==1) pdf(output,height=height,width=width,onefile=onefile)

if (panels==2) 
{
	pdf(output,height=height,width=width*2,onefile=onefile)
	layout(matrix(1:2,nrow=1)) 
}

if (panels==8) 
{
	pdf(output,height=height*4,width=width*2,onefile=onefile)
	layout(matrix(1:8,nrow=4,byrow=T)) 
}

if (panels==4) 
{
	pdf(output,height=height*2,width=width*2,onefile=onefile)
	layout(matrix(1:4,nrow=2,byrow=T)) 
}
} else
{
width <- NULL
height <- NULL
}


# First pane: Model statistics
if (1%in%include)
{
  
semSum <- summary(res,analytic.se=FALSE)
par(mar=c(1,1,1,1))
plot(1, ann = FALSE, axes = FALSE, xlim = c(0, 100), ylim = c(0, 100),
     type = "n", xaxs = "i", yaxs = "i")
title("Model statistics")
text(50,80,paste("Model:
Observed variables:",res$n,"/",res$m,"
Number of free parameters:",res$t,"
Number of observations:",res$N,"
Number of fixed exogenous variables:",res$n.fix,"

Iterations:",res$iterations,"

Goodness of Fit:
Chisq:",round(semSum$chisq,5),", df:",semSum$df,", p=",round(1-pchisq(semSum$chisq,semSum$df),5),"
RMSEA:",round(semSum$RMSEA[1],5)," (",semSum$RMSEA[4]*100,"% CI:",round(semSum$RMSEA[2],5)," - ",round(semSum$RMSEA[3],5),")
Goodness-of-fit index:",round(semSum$GFI,5),"
Adjusted goodness-of-fit index:",round(semSum$AGFI,5),"
Bentler-Bonnett NFI:",round(semSum$NFI,5),"
Tucker-Lewis NNFI:",round(semSum$NNFI,5),"
Bentler CFI:",round(semSum$CFI,5),"
SRMR:",round(semSum$SRMR,5),"
BIC:",round(semSum$BIC,5)),pos=1)
}	 

# Create E
E=data.frame(from=as.numeric(res$ram[,3]),to=as.numeric(res$ram[,2]),
	heads=res$ram[,1],label=rownames(res$ram),parameter=res$ram[,4],weight=res$ram[,5],
	stringsAsFactors=F)

E2=E
	
for (i in 1:length(res$var.names)) {
	E[E2[,1]==i,1]=res$var.names[i]
	E[E2[,2]==i,2]=res$var.names[i]	}

for (i in unique(E$parameter)){
	if (any(E$label[E$parameter==i]=="") & any(E$label[E$parameter==i]!="")) {
	E$label[E$parameter==i & E$label==""]=E$label[E$parameter==i & E$label!=""] }}

E$typefrom="lat"
E$typefrom[E$from %in% colnames(res$S)]="man"
E$typeto="lat"
E$typeto[E$to %in% colnames(res$S)]="man"

E$residual=E$typefrom==E$typeto & E$heads==2


# Set estimated weights
for (i in 1:length(res$coef)) E$weight[E$label==names(res$coef)[i]]=res$coef[i]

# Set fixed labels
E$label[E$label==""]=round(E$weight[E$label==""],2)

# Edit variances:
var=E[E$from==E$to,c(1,6)]
if (residuals==F) E[E$from==E$to,6]=0

#Residuals:
if (residuals) {
E$from[E$residual & E$from!=E$to]=paste("RES",E$from[E$residual & E$from!=E$to],sep="@")
E$to[E$residual & E$from!=E$to]=paste("RES",E$to[E$residual & E$from!=E$to],sep="@")
E$heads[E$from==E$to]=1
E$from[E$from==E$to]=paste("RES",E$from[E$from==E$to],sep="@")
}

# Set bidirectional arrows:
biHeads=E$heads==2
if (any(E$heads==2)) 
{
	E=rbind(E,E[biHeads,])
	E[c(biHeads,rep(FALSE,sum(biHeads))),1:2]=E[c(biHeads,rep(FALSE,sum(biHeads))),2:1]
}

# Residual labels
if (!residuals) E$label[E$residual]=""

# Set nodes:
V=data.frame(labels=as.character(unique(c(t(E[,1:2])))),isresidual=F,type="lat",stringsAsFactors=FALSE)
V$ID=V$labels
ressplit=strsplit(V$labels,"@")
for (i in 1:length(ressplit)) 
{
	V$isresidual[i]=any(ressplit[[i]]=="RES") 
}
for (i in 1:length(ressplit)) 
{
	if(any(ressplit[[i]] %in% colnames(res$S))) V$type[i]='man'
}
V$labels[V$isresidual]=""

# Create layout:
l=layout
if (layout=='tree' | layout=="springtree" | layout=="circle") 
{
	if (residuals) 
	{
		l=matrix(0,nrow=nrow(V),ncol=2)
		l[V$isresidual & V$type=='man',2]=-1
		l[!V$isresidual & V$type=='man',2]=-0.5
		l[!V$isresidual & V$type=='lat',2]=0.5
		l[V$isresidual & V$type=='lat',2]=1

		sum=sum(V$isresidual & V$type=='man')
		l[V$isresidual & V$type=='man',1]=seq(-1,1,length=sum+2)[2:(sum+1)]
		sum=sum(!V$isresidual & V$type=='man')
		l[!V$isresidual & V$type=='man',1]=seq(-1,1,length=sum+2)[2:(sum+1)]
		sum=sum(!V$isresidual & V$type=='lat')
		l[!V$isresidual & V$type=='lat',1]=seq(-1,1,length=sum+2)[2:(sum+1)]
		sum=sum(V$isresidual & V$type=='lat')
		l[V$isresidual & V$type=='lat',1]=seq(-1,1,length=sum+2)[2:(sum+1)]

		E$curved=0
		E$fromtype=0
		E$totype=0

		un=unique(l[,2])
		for (i in 1:length(un)) 
		{
			E$fromtype[E$from%in%V$ID[which(l[,2]==un[i])]]=i
			E$totype[E$to%in%V$ID[which(l[,2]==un[i])]]=i
		}

		E$curved[E$fromtype==E$totype]=curve

		# Reorder residuals:
		l.temp=l
		for (i in 1:nrow(E))
		{
			if (E$residual[i])
			{
				resName=unlist(strsplit(as.character(E[i,1:2]),split="RES@"))
				resName=resName[resName!=""]
				if (length(unique(resName))==1) l.temp[which(V$ID==paste("RES@",resName[1],sep="")),1]=l[which(V$ID==resName[1]),1]
			}
		}
		l=l.temp
				
					

	} else 
	{

		l=matrix(0,nrow=nrow(V),ncol=2)
		l[V$type=='man',2]=-0.5
		l[V$type=='lat',2]=0.5

		sum=sum(V$type=='man')
		l[V$type=='man',1]=seq(-1,1,length=sum+2)[2:(sum+1)]

		sum=sum(V$type=='lat')
		l[V$type=='lat',1]=seq(-1,1,length=sum+2)[2:(sum+1)]

		E$curved=0
		E$fromtype=0
		E$totype=0

		un=unique(l[,2])
		for (i in 1:length(un)) 
		{
			E$fromtype[E$from%in%V$ID[which(l[,2]==un[i])]]=i
			E$totype[E$to%in%V$ID[which(l[,2]==un[i])]]=i
		}

		E$curved[E$fromtype==E$totype]=curve
	} 
	if (layout=="springtree")
	{
		l[,1]=NA
		layout.par$constraints=l
		l="spring"
	}
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

	if (residuals)
	{
		srt <- sort(l[l[,2] == -1,1],index.return=TRUE)$ix
		l2[which(l[,2]==-1)[srt],] <- (1+residSize) * l2[l[,2] == -0.5,]
		
		srt <- sort(l[l[,2] == 1,1],index.return=TRUE)$ix
		l2[which(l[,2] == 1)[srt],] <-  (1-2*residSize) * l2[l[,2] == 0.5,]
	}
	
	l <- l2
} else if (layout != "spring")
{
	l[l[,2]==-1,2] <- -0.5 - residSize
	l[l[,2]==1,2] <- 0.5 + residSize
}

#Set shapes
V$shape='circle'
V$shape[V$labels %in% colnames(res$S)]='square'

# Border colors:
if (residuals)
{
	V$border.colors="black"
	V$border.colors[V$isresidual]="#00000000" 
}

# vertex sizes:
V$size=vsize.lat
V$size[V$labels %in% colnames(res$S)]=vsize.man
if (residuals) V$size[V$isresidual]=1

# Make edgelist:

edgelist <- apply(E[,1:2],2,match,V$ID)

edgelist=as.matrix(edgelist)

# Set lty and labels:
E$lty=1
E$lty[E$parameter==0]=2
E$label[E$parameter==0]=""

# Remove residual variances:
if (!residuals)
{
	diag=id=rep(0,nrow(V))
	for (i in 1:nrow(V))
	{
		id[i]=which(rowSums(edgelist[,1:2]==c(i,i))==2)[1]
		diag[i]=E$weight[id[i]]
	}
	residKeep=which(edgelist[,1]-edgelist[,2]!=0)
	edgelist=edgelist[residKeep,]
	E=E[residKeep,]
} else diag=FALSE

# RUN QGRAPH FOR MODEL:
if (2%in%include) DNPL <- FALSE else DNPL <- TRUE

Q=qgraph(
	edgelist,
	layout=l,
	edge.labels=E$label,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty,
	border.colors=V$border.colors,
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=(E$heads==2),
	filetype="",
	esize=1,
	width=width,
	height=height,
	DoNotPlot=DNPL,
	groups=NULL,
	...)
if (2%in%include) title("Specified model",line=NA)

# RUN QGRAPH FOR ABSOLUTE PARAMETER ESTIMATES:
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
	border.colors=V$border.colors,
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=(E$heads==2),
	filetype="",
	esize=1,
	edge.labels=round(E$weight,2),
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
title("Unstandardized model",line=NA)
}
# RUN QGRAPH FOR STANDARDIZED PARAMETER ESTIMATES:

standcoef=round(standardizedCoefficients(res)[,2],2)
if (sum(biHeads>0)) 
{
	standcoef=c(standcoef,standcoef[biHeads])
}
# Remove residual variances:
if (!residuals)
{
	for (i in 1:length(id)) diag[i]=standcoef[id[i]]
	standcoef=standcoef[residKeep]
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
	border.colors=V$border.colors,
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=(E$heads==2),
	filetype="",
	esize=1,
	edge.labels=standcoef,
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
title("Standardized model",line=NA)
}
# RUN QGRAPH FOR WEIGHTED ESTIMATES:

edgelist=cbind(edgelist,E$weight)
if (5%in%include)
{
qgraph(
	edgelist,
	layout=Q$layout,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	border.colors=V$border.colors,
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=(E$heads==2),
	filetype="",
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
title("Unstandardized model",line=NA)
}
edgelist[,3]=standcoef
if (6%in%include)
{
qgraph(
	edgelist,
	layout=Q$layout,
	curve=E$curved,
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	border.colors=V$border.colors,
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=(E$heads==2),
	filetype="",
	width=width,
	height=height,
	diag=diag,
	groups=NULL,
	...)
title("Standardized model",line=NA)
}

# COVARIANCES and CORRELATIONS:

maximum=max(abs(c(res$S[upper.tri(res$S)],res$C[upper.tri(res$C)])))	

par(mar=c(3,3,3,3))

res$C=round(res$C,6)
res$S=round(res$S,6)


if (7%in%include)
{
qgraph(
	res$S, 
	labels=rownames(res$S), 
	filetype="",
	maximum=maximum,
	diag="col",
	...)
title("Observed covariances",line=NA)

}
if (8%in%include)
{
qgraph(
	res$C, 
	labels=rownames(res$C), 
	filetype="",
	maximum=maximum,
	diag="col",
	...)
	title("Implied covariances",line=NA)
}

maximum=max(abs(c(cov2cor(res$S)[upper.tri(res$S)],cov2cor(res$C)[upper.tri(res$C)])))	

if (9%in%include)
{	
qgraph(round(cov2cor(res$S),5),
	labels=rownames(res$S), 
	filetype="",
	maximum=maximum,
	...)
title("Observed correlations",line=NA)
}

if (10%in%include)
{
qgraph(round(cov2cor(res$C),5), 
	labels=rownames(res$C), 
	filetype="",
	maximum=maximum,
	...)
title("Implied correlations",line=NA)
}

if (11%in%include)
{
qgraph(res$S-res$C, 
	labels=rownames(res$C), 
	filetype="",
	diag="col",
	...)
title("Covariance differences",line=NA)
}


if (12%in%include)
{
qgraph(round(cov2cor(res$S)-cov2cor(res$C),5),
	labels=rownames(res$C), 
	filetype="",
	...)
title("Correlation differences",line=NA)
}

if (filetype=="pdf")
{
dev.off()
print(paste("Output stored in ",getwd(),"/",output,sep=""))
}
}

