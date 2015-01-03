
qgraph.loadings=function( fact, ...)
{

if (class(fact)=="loadings") fact <- fact[1:nrow(fact),1:ncol(fact)]

arguments=list(...)

if (length(arguments)>0)
{
	for (i in 1:length(arguments))
	{
		if (class(arguments[[i]])=="qgraph") 
		{
			if (!is.null(names(arguments[[i]])))
			{
				for (j in 1:length(arguments[[i]]))
				{
					if (!(names(arguments[[i]])[j]%in%names(arguments)))
					{
						arguments[length(arguments)+1]=arguments[[i]][j]
						names(arguments)[length(arguments)]=names(arguments[[i]])[j]
					}
				}
			}
		}
	}
}

if (is.null(rownames(fact))) rownames(fact) <- 1:nrow(fact)

# SET DEFAULT ARGUMENTS:
if(is.null(arguments$resid)) resid=NULL else resid=arguments$resid
if(is.null(arguments$factorCors)) factorCors=NULL else factorCors=arguments$factorCors
if(is.null(arguments$residSize)) residSize=0.1 else residSize=arguments$residSize
if(is.null(arguments$filetype)) filetype="default" else filetype=arguments$filetype
if(is.null(arguments$vsize)) vsize=max((-1/72)*(nrow(fact))+5.35,1) else vsize=arguments$vsize
if(is.null(arguments$groups)) groups=NULL else groups=arguments$groups
if (is.factor(groups) | is.character(groups)) groups <- tapply(1:length(groups),groups,function(x)x)
if(is.null(arguments$color)) color=NULL else color=arguments$color
if(is.null(arguments$model)) model="none" else model=arguments$model
if(is.null(arguments$crossloadings)) crossloadings=FALSE else crossloadings=arguments$crossloadings
if(is.null(arguments$labels))
{
  labels <- TRUE
  if (nrow(fact) <= 20)
  {
    labels <- abbreviate(rownames(fact),3)
  }

} else labels <- arguments$labels
if(is.null(arguments$Fname)) Fname=NULL else Fname=arguments$Fname
if(is.null(arguments$layout)) layout="circle" else layout=arguments$layout
if (layout=="circular") layout <- "circle"
if(is.null(arguments$legend))
{
	if (!is.null(groups) & !is.null(names(groups)) & filetype=="pdf") legend=TRUE else legend=FALSE
} else legend=arguments$legend
if(is.null(arguments$legend.cex)) legend.cex=1 else legend.cex=arguments$legend.cex

# Output arguments:
if(is.null(arguments$filetype)) filetype="default" else filetype=arguments$filetype
if(is.null(arguments$filename)) filename="qgraph" else filename=arguments$filename
if(is.null(arguments$width))
{
	if (is.null(dev.list()[dev.cur()])) width=10 else width=dev.size(units="in")[1]
} else width=arguments$width
if(is.null(arguments$height))
{
	if (is.null(dev.list()[dev.cur()]))
	{
		if (layout=="circle") height=10 else height=5
	} else height=dev.size(units="in")[2]
}	else height=arguments$height
if(is.null(arguments$pty)) pty='m' else pty=arguments$pty
if(is.null(arguments$res)) res=320 else res=arguments$res


# Start output:
if (filetype=='default') if (is.null(dev.list()[dev.cur()])) dev.new(rescale="fixed",width=width,height=height)
if (filetype=='R') dev.new(rescale="fixed",width=width,height=height)
if (filetype=='eps') postscript(paste(filename,".eps",sep=""),height=height,width=width, horizontal=FALSE)
if (filetype=='pdf') pdf(paste(filename,".pdf",sep=""),height=height,width=width)
if (filetype=='tiff') tiff(paste(filename,".tiff",sep=""),units='in',res=res,height=height,width=width)
if (filetype=='png') png(paste(filename,".png",sep=""),units='in',res=res,height=height,width=width)
if (filetype=='jpg' | filetype=='jpeg') jpeg(paste(filename,".jpg",sep=""),units='in',res=res,height=height,width=width)
if (filetype=="svg")
{
# 	if (R.Version()$arch=="x64") stop("RSVGTipsDevice is not available for 64bit versions of R.")
# 	require("RSVGTipsDevice")
  if (!requireNamespace("RSVGTipsDevice", quietly = TRUE)) stop("Please install 'RSVGTipsDevice' package first.")
  RSVGTipsDevice::devSVGTips(paste(filename,".svg",sep=""),width=width,height=height,title=filename)
}

# Rescale dims:
if (pty=='s')
{
	width=height=min(c(width,height))
}

# Parameters:
n=nrow(fact)
k=ncol(fact)

names=names(groups)

# Max loadings:
if (k>1) 
{
	maxload=apply(abs(fact),1,which.max)
	sorted=sort(maxload,index.return=T)

	sort2=sort(apply(fact,2,which.max),index.return=T)$ix

	#IDENTIFY GROUPS:
	if (!is.null(groups) ) 
	{
		identity=vector("numeric",length(groups))

		for (i in 1:length(groups)) 
		{
			identity[i]=as.numeric(names(which.max(table(maxload[groups[[i]]]))))
		}

		if (length(unique(identity))==length(groups)) identified=TRUE else identified=FALSE
	} else identified=FALSE

	if (k<length(groups)) identified=FALSE 

	# crossloadings:
	if (crossloadings) 
	{
		if (identified) 
		{
			for (i in 1:k) 
			{
				fact[groups[[i]],identity[i]]=0 
			} 
		} else 
		{
			for (i in 1:k) 
			{
				fact[maxload==i,i]=0 
			} 
		} 
	}
} 
if (k==1) identified=FALSE

#comupte the edgelist
m=matrix(c(rep(1:n,k),rep(n+1:k,each=n),fact),nrow=n*k,ncol=3)

if (model%in%c("reflective","formative")) directed <- rep(TRUE ,n*k) else directed <- rep(FALSE,n*k)
if (model=="reflective")  m[,1:2] <- m[,2:1]


# Set shapes:
shape=character()
shape[1:n]="square"
shape[(n+1):(n+k)]="circle"

if (!is.null(groups) && k>1 ) identitysort=sort(identity,index=T)$ix

# Set labels:
Glabels=rep("",n+k)
if (!is.logical(labels)) 
{
	Glabels[1:n]=labels 
} else if (is.logical(labels)) 
{
	if (labels == TRUE) 
	{
		Glabels[1:n]=seq(nrow(fact)) 
	} 
}
Glabels[(n+1):(n+k)]=1:k
if (!is.null(names) & identified) 
{
	for (i in 1:k) 
	{
	Glabels[n+i]=names[identitysort[i]] 
	}
}
if (k==1 & !is.null(Fname)) Glabels[n+1]=Fname

#  Vertex sizes
if (length(vsize)==1) vsize=rep(vsize,2)

Gvsize=rep(vsize[1],nrow(fact)+ncol(fact))
Gvsize[(n+1):(n+k)]=vsize[2]


# Set colors:
Gcolor = rep("white",nrow(fact)+ncol(fact))
if (is.null(color) & !is.null(groups)) color=rainbow(length(groups))

if (!is.null(groups)) 
{ 
	for (i in 1:length(groups)) 
	{
	Gcolor[groups[[i]]]<-color[i] 
	} 
}

if (identified) 
{
	for (i in 1:k) 
	{
		Gcolor[n+i]=color[identitysort[i]] 
	}
}

# Set layout:
if (layout!="circle")
{
	l2=l=matrix(0,ncol=2,nrow=n+k)
	l2[,2]=c(rep(-1,n),rep(0,k))
	l2[,1]=c(seq(-1,1,length=n),seq(-1,1,length=k+2)[2:(k+1)])

	if (k>1) 
	{
		if (!identified)
		{
			for (i in 1:n) l[i,]=l2[which(sorted$ix==i),]
		} else
		{
			l[unlist(groups[identitysort]),]<-l2[1:n,]
		}
		l[(n+1):(n+k),]=l2[(n+1):(n+k),] 
	} else l=l2
}
if (layout=="circle")
{
	l2=l=matrix(0,ncol=2,nrow=n+k)
	tl=n+1
	l2[1:n,1]=sin(seq(0,2*pi, length=tl))[-tl]
	l2[1:n,2]=cos(seq(0,2*pi, length=tl))[-tl]
	
	if (k>1)
	{
		if (!identified) for (i in 1:n) l[i,]=l2[which(sorted$ix==i),]
		if (identified)
		{
			l[unlist(groups[identitysort]),]<-l2[1:n,]
		}
		tl=k+1
		l[(n+1):(n+k),1]=0.5*sin(seq(0,2*pi,length=tl)+(1*pi/k))[-tl]
		l[(n+1):(n+k),2]=0.5*cos(seq(0,2*pi,length=tl)+(1*pi/k))[-tl]
		
	} else l[1:n,]=l2[1:n,]
}


### Set residuals ###

curve <- 0

if (!is.null(resid))
{
	if (length(resid)!=n) stop("Length of residuals does not correspond to number of factors")
	m <- rbind(m, cbind( n+k+1:n, 1:n, resid))
	Gvsize <- c(Gvsize,rep(0,n))
	Gcolor <- c(Gcolor, rep("#00000000",n))
	Glabels <- c(Glabels, rep("",n))
	shape <- c(shape,rep("circle",n))
	directed <- c(directed,rep(TRUE,n))
	if (layout!="circle")
	{
		l <- rbind(l, l[1:n,])
		l[n+k+1:n,2] <- -1 - residSize
	} else
	{
		l <- rbind(l, (1+residSize) * l[1:n,])
	}
}


if (!is.null(factorCors))
{
	m <- rbind(m, cbind( rep(n+1:k,times=k), rep(n+1:k,each=k), c(factorCors) ) )
	m <- m[m[,1] != m[,2],]
	if (layout!="circle")
	{
		if (is.null(resid)) curve <- c( rep(0, n*k ), rep(0.4, k^2 - k)) else curve <- c( rep(0, n*k + n ), rep(0.4, k^2 - k))
	}
	directed <- c(directed,rep(TRUE, k^2-k))
}
	
### RUN QGRAPH ###
# class(arguments)="qgraph"
args <- list(input=m,layout=l,vsize=Gvsize,color=Gcolor,labels=Glabels,shape=shape,filetype="",curve=curve,
             height=height,width=width,legend=F,directed=directed,bidirectional=TRUE)

args <- c(args,arguments[!names(arguments) %in% names(args)])

Q <- do.call(qgraph,args)

# 
# Q <- qgraph(m,layout=l,vsize=Gvsize,color=Gcolor,labels=Glabels,shape=shape,filetype="",curve=curve,
# 	height=height,width=width,legend=F,arguments,directed=directed,bidirectional=TRUE)

Q$filetype <- filetype
	
# Legend:

if (legend & filetype=="pdf")
{
	legend.cex=legend.cex*2
	plot(1, ann = FALSE, axes = FALSE, xlim = c(-1, 1), ylim = c(-1 ,1 ),type = "n", xaxs = "i", yaxs = "i")
	legend (0,0, names(groups), col= color ,pch = 19, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
	legend (0,0, names(groups), col= "black" ,pch = 1, xjust=0.5, ,yjust=0.5, cex=legend.cex, bty='n') 
}
else if (legend & filetype!="pdf") warning("Legend in qgraph.loadings only supported for pdf output")
	
if (filetype%in%c('pdf','png','jpg','jpeg','svg','eps','tiff')) dev.off()

class(Q)="qgraph"
invisible(Q)
}

