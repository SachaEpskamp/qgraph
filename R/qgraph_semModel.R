
qgraph.semModel=function(
	mod,
	manifest=NULL,
	layout="spring",
	vsize.man=3,
	vsize.lat=6,
	residuals=TRUE,
	latres=TRUE,
	curve=0.2,
	residSize=0.2,
	...)
{
  warning("This funcion is deprecated,\nUse the 'semPlot' package instead.")
#reqTest <- require("sem")
#if (!reqTest) stop("sem could not be loaded, is this package installed?")
if (!any(class(mod)%in%c("semmod","sem"))) stop("Input must be a 'semmod' or 'sem' object")

if ("sem"%in%class(mod))
{
	qgraph.sem(mod,include=2,filetype="",...)
} else
{
arguments=list(...)
if(is.null(arguments$layout.par)) layout.par=list() else layout.par=arguments$layout.par

 # Transform model to sem:
if (class(mod)=="semmod")
{
if (is.null(manifest) & layout=="tree") stop("Tree-layout can't be created if 'manifest' is not assigned")

edge <- apply(mod,1,function(x)unlist(strsplit(x[1],split="")))
edge <- lapply(edge,function(x)x[!x%in%c(" ","-")])
heads <- sapply(edge,function(x)sum(c("<",">")%in%x))
fromLeft <- sapply(edge,function(x)any(x==">"))

edge <- lapply(edge,function(x)gsub("<","@SPLIT@",x))
edge <- lapply(edge,function(x)gsub(">","@SPLIT@",x))


edge <- lapply(lapply(edge,function(x)paste(x,collapse="")),strsplit,split="@SPLIT@")
edge <- lapply(edge,function(x)unlist(x)[unlist(x)!=""])

FromTo <- t(sapply(edge,c))

FromTo[!fromLeft,] <- FromTo[!fromLeft,2:1]


param <- t(apply(mod,1,function(x)x[-1]))

param[is.na(param[,1]),1] <- ""
param[is.na(param[,2]),2] <- 0

parameter <- rep(0,nrow(param))
parameter[param[,1]!=""] <- 1:sum(param[,1]!="")

res <- list(var.names=unique(c(FromTo[,2:1])))

FromTo <- matrix(match(FromTo,res$var.names),,2)

res$ram <- data.frame( heads=heads,to=FromTo[,2], from=FromTo[,1], parameter = parameter, value=as.numeric(param[,2]), names=param[,1])

# Create E
E=data.frame(from=as.numeric(res$ram[,3]),to=as.numeric(res$ram[,2]),
	heads=res$ram[,1],label=res$ram$names,parameter=res$ram[,4],weight=res$ram[,5],
	stringsAsFactors=F)
} else 
{
	res <- mod
	manifest <- colnames(res$S)
	
	# Create E
	E=data.frame(from=as.numeric(res$ram[,3]),to=as.numeric(res$ram[,2]),
	heads=res$ram[,1],label=rownames(res$ram),parameter=res$ram[,4],weight=res$ram[,5],
	stringsAsFactors=F)
}



E2=E
	
for (i in 1:length(res$var.names)) {
	E[E2[,1]==i,1]=res$var.names[i]
	E[E2[,2]==i,2]=res$var.names[i]	}

for (i in unique(E$parameter)){
	if (any(E$label[E$parameter==i]=="") & any(E$label[E$parameter==i]!="")) {
	E$label[E$parameter==i & E$label==""]=E$label[E$parameter==i & E$label!=""] }}

E$typefrom="lat"
E$typefrom[E$from %in% manifest]="man"
E$typeto="lat"
E$typeto[E$to %in% manifest]="man"

E$residual=E$typefrom==E$typeto & E$heads==2


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
	if(any(ressplit[[i]] %in% manifest)) V$type[i]='man'
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
	
	E$curved <- 0

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
}

#Set shapes
V$shape='circle'
V$shape[V$labels %in% manifest]='square'

# Border colors:
if (residuals)
{
	V$border.colors="black"
	V$border.colors[V$isresidual]="#00000000" 
}

# vertex sizes:
V$size=vsize.lat
V$size[V$labels %in% manifest]=vsize.man
if (residuals) V$size[V$isresidual]=1

# Make edgelist:
edgelist=E[,1:2]
for (i in 1:nrow(V)) edgelist[edgelist==V$ID[i]]=i
edgelist$from=as.numeric(edgelist$from)
edgelist$to=as.numeric(edgelist$to)

edgelist=as.matrix(edgelist)

# Set lty and labels:
E$lty=1
E$lty[E$parameter==0]=2
E$label[E$parameter==0]=""

# Remove residual variances:
diag=FALSE

# RUN QGRAPH FOR MODEL:
Q=qgraph(
	edgelist[edgelist[,1]!=edgelist[,2],],
	layout=l,
	edge.labels=E$label[edgelist[,1]!=edgelist[,2]],
	curve=E$curved[edgelist[,1]!=edgelist[,2]],
	labels=V$labels,
	shape=V$shape,
	vsize=V$size,
	lty=E$lty[edgelist[,1]!=edgelist[,2]],
	border.colors=V$border.colors,
	layout.par=layout.par,
	directed=TRUE,
	bidirectional=(E$heads==2)[edgelist[,1]!=edgelist[,2]],
	filetype="",
	esize=1)
title("Specified model",line=-1)
invisible(Q)
}
}