
qgraph.panel=function(input, ...)
{
Q=list(...)
class(Q)="qgraph"

# Output Q:
if(is.null(Q$filetype)) filetype="default" else filetype=Q$filetype
if(is.null(Q$filename)) filename="qgraph" else filename=Q$filename
if(is.null(Q$width))
{
	if (is.null(dev.list()[dev.cur()])) width=7/2 else width=dev.size(units="in")[1]/2
} else width=Q$width
if(is.null(Q$height))
{
	if (is.null(dev.list()[dev.cur()])) height=7/2 else height=dev.size(units="in")[2]/2
} else height=Q$height
if(is.null(Q$pty)) pty='m' else pty=Q$pty
if(is.null(Q$res)) res=320 else res=Q$res

# Start output:
if (filetype=='default') if (is.null(dev.list()[dev.cur()])) dev.new(rescale="fixed",width=width*2,height=height*2)
if (filetype=='R') dev.new(rescale="fixed",width=width*2,height=height*2)
if (filetype=='eps') postscript(paste(filename,".eps",sep=""),height=height*2,width=width*2, horizontal=FALSE)
if (filetype=='pdf') pdf(paste(filename,".pdf",sep=""),height=height*2,width=width*2)
if (filetype=='tiff') tiff(paste(filename,".tiff",sep=""),units='in',res=res,height=height*2,width=width*2)
if (filetype=='png') png(paste(filename,".png",sep=""),units='in',res=res,height=height*2,width=width*2)
if (filetype=='jpg' | filetype=='jpeg') jpeg(paste(filename,".jpg",sep=""),units='in',res=res,height=height*2,width=width*2)
if (filetype=="svg")
{
# 	if (R.Version()$arch=="x64") stop("RSVGTipsDevice is not available for 64bit versions of R.")
# 	require("RSVGTipsDevice")
  if (!requireNamespace("RSVGTipsDevice", quietly = TRUE)) stop("Please install 'RSVGTipsDevice' package first.")
  RSVGTipsDevice::devSVGTips(paste(filename,".svg",sep=""),width=width*2,height=height*2,title=filename)
}
if (filetype=="tex") stop("Tex is not yet supported in qgraph.panel")

#layout(matrix(1:4,nrow=2,ncol=2))
### PLOT
parOrig <- par(no.readonly=TRUE)
par(mar=c(0,0,0,0))
plot(1, ann = FALSE, axes = FALSE, xlim = c(-2.4, 2.4), ylim = c(-2.4 ,2.4),type = "n", xaxs = "i", yaxs = "i")

width <- par('pin')[1]
height <- par('pin')[2]

qgraph(input,filetype="",Q,layout="circular",graph="association",legend=FALSE,layoutOffset=c(-1.2,1.2),plot=FALSE,width=width,height=height)
qgraph(input,layout="spring",filetype="",Q,graph="association",legend=FALSE,layoutOffset=c(-1.2,-1.2),plot=FALSE,width=width,height=height)
qgraph(input,graph="concentration",layout="spring",filetype="",Q,legend=FALSE,layoutOffset=c(1.2,1.2),plot=FALSE,width=width,height=height)
qgraph(input,graph="factorial",layout="spring",filetype="",Q,legend=FALSE,layout.par=list(area=nrow(input)^2.3,repulse.rad=nrow(input)^2.8),layoutOffset=c(1.2,-1.2),plot=FALSE,width=width,height=height)

if (filetype%in%c('pdf','png','jpg','jpeg','svg','eps','tiff','tex')) 
{
	print(paste("Output stored in ",getwd(),"/",filename,".",filetype,sep=""))
	dev.off()
}
par(parOrig)
invisible(Q)
}
