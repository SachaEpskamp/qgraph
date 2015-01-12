

qgraph.svg=function( input, 
	layout=c( "circular", "spring" ), 
	graph=c( "association", "concentration", "factorial" ),
	cut=c( 0, 0.2, 0.3, 0.5),
	filename="qgraph",
	title="qgraph output",
	nfact=round(ncol(input)/2,0),
	tooltips=NULL,
	... )
{

#if (R.Version()$arch=="x64") stop("RSVGTipsDevice is not available for 64bit versions of R.")

fn=unlist(strsplit(filename,"/"))
filename <- fn[length(fn)]
folder=paste(fn[-length(fn)],collapse="/")
cat("This function will now make a battery of SVG files in the folloing folder: \n")
cat(paste(getwd(),"/",folder,sep=""))
cont=menu(c("Yes","No"),F,paste("\n\n Continue?"))
stopifnot(cont==1)

cat("This function may take a while to run \n\n")

if (!requireNamespace("RSVGTipsDevice", quietly = TRUE)) stop("Please install 'RSVGTipsDevice' package first.")

arguments=list(...)

if(is.null(arguments$groups)) groups=NULL else groups=arguments$groups
if(is.null(arguments$color)) color=NULL else color=arguments$color

# Create layouts:
L=list()
s=1

for (l in layout)
{
	for (g in graph)
	{
		for (c in cut)
		{
			L[[s]]=qgraph(input,layout=l,graph=g,cut=c,nfact=nfact,DoNotPlot=TRUE,...)$layout
			s=s+1
		}
	}
}

# Create Graphs:

for (G in graph)
{
	s=1
	for (l in layout)
	{
		for (g in graph)
		{
			for (c in cut)
			{
			  RSVGTipsDevice::devSVGTips(paste(folder,ifelse(folder=="","","/"),filename,G,l,g,c,".svg",sep=""),width=16,height=10,title=filename)
				
				layout.mat=matrix(0,ncol=14,nrow=10,byrow=T)
				
				layout.mat[1,1:9]=1
				layout.mat[2:10,1:9]=2
				layout.mat[1:8,10:14]=3
				layout.mat[9:10,10:14]=4
				layout(layout.mat)
				par(mar=c(2,2,2,2))
				
				# Legend and Title:
				par(mar=c(0,0,3,0),cex.main=4)
				plot(1, ann = FALSE, axes = FALSE, xlim = c(-1, 1), ylim = c(-1, 1),
					type = "n", xaxs = "i", yaxs = "i")

				title(title)
				
				if (!is.null(groups))
				{
					if (is.null(color)) color=rainbow(length(groups))
					legendsequence=seq(-0.8,0.8,length=length(groups))

					for (i in 1:length(groups))
					{
					  RSVGTipsDevice::setSVGShapeToolTip(desc=names(groups)[i])
						points(legendsequence[i],0,cex=4,pch=15,col=color[i])
					}
				}
				
				# Plot graph:
				qgraph(input,layout=L[[s]],graph=G,cut=c,nfact=nfact,filetype="",
					SVGtooltips=tooltips,legend=F,...)

					
				## Graph choice:
					

				par(mar=c(0,0,0,0))
				plot(1, ann = FALSE, axes = FALSE, xlim = c(-1, 1), ylim = c(-1, 1),
					type = "n", xaxs = "i", yaxs = "i")
				y=0.7
				text(-0.9,y,"Graph:",cex=6,pos=4)	
				for (i in 1:length(graph))
				{
					y=y-0.1
					if (graph[i]==G) 
					{
						text(-0.7,y,graph[i],cex=4,col="gray",pos=4)
					} else 
					{
					  RSVGTipsDevice::setSVGShapeURL(paste(filename,graph[i],l,g,c,".svg",sep=""))
						text(-0.7,y,graph[i],cex=4,pos=4)
					}
					
				}
				y=y-0.1
				text(-0.9,y,"Layout:",cex=6,pos=4)
				for (i in 1:length(layout))
				{
					y=y-0.1
					if (layout[i]==l) 
					{
						text(-0.7,y,layout[i],cex=4,col="gray",pos=4)
					} else 
					{
					  RSVGTipsDevice::setSVGShapeURL(paste(filename,G,layout[i],g,c,".svg",sep=""))
						text(-0.7,y,layout[i],cex=4,pos=4)
					}
					
				}
				y=y-0.1
				text(-0.9,y,"Based on:",cex=6,pos=4)
				for (i in 1:length(graph))
				{
					y=y-0.1
					if (graph[i]==g) 
					{
						text(-0.7,y,graph[i],cex=4,col="gray",pos=4)
					} else 
					{
					  RSVGTipsDevice::setSVGShapeURL(paste(filename,G,l,graph[i],c,".svg",sep=""))
						text(-0.7,y,graph[i],cex=4,pos=4)
					}
					
				}
				y=y-0.1
				text(-0.9,y,"Cutoff:",cex=6,pos=4)
				for (i in 1:length(cut))
				{
					y=y-0.1
					if (cut[i]==c) 
					{
						text(-0.7,y,cut[i],cex=4,col="gray",pos=4)
					} else 
					{
					  RSVGTipsDevice::setSVGShapeURL(paste(filename,G,l,g,cut[i],".svg",sep=""))
						text(-0.7,y,cut[i],cex=4,pos=4)
					}
					
				}	
					
					
				# Info:
				par(mar=c(0,0,0,0))
				plot(1, ann = FALSE, axes = FALSE, xlim = c(-1, 1), ylim = c(-1, 1),
					 type = "n", xaxs = "i", yaxs = "i")
					 
				text(-0.8,0.8,"Created with qgraph package:
				 
				http://sites.google.com/site/qgraphproject",pos=4)
				 
				dev.off()
				s=s+1
				}
			}
		}
	}
}
	
