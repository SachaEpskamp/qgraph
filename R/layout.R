qgraph.layout.fruchtermanreingold=function(edgelist,weights=NULL,vcount=NULL,niter=NULL,max.delta=NULL,area=NULL,cool.exp=NULL,repulse.rad=NULL,init=NULL,groups=NULL,rotation=NULL,layout.control=0.5,constraints=NULL){
  
  Ef<-edgelist[,1]-1
  Et<-edgelist[,2]-1
  #Provide default settings
  ecount=nrow(edgelist)
  if (!is.null(vcount)) n=vcount else n=max(length(unique(c(edgelist))),max(edgelist))
  if (is.null(weights)) weights=rep(1,ecount)
  if(is.null(niter)) niter<-500
  if(is.null(max.delta)) max.delta<-n
  if (length(max.delta)==1) max.delta=rep(max.delta,vcount)
  if(is.null(area)) area<-n^2
  if(is.null(cool.exp)) cool.exp<-1.5
  if(is.null(repulse.rad)) repulse.rad<-area*n
  if(is.null(init)){
    #tempa<-sample((0:(n-1))/n) #Set initial positions randomly on the circle
    #x<-n/(2*pi)*sin(2*pi*tempa)
    #y<-n/(2*pi)*cos(2*pi*tempa)
	
		init=matrix(0,nrow=n,ncol=2)
		tl=n+1
		init[,1]=sin(seq(0,2*pi, length=tl))[-tl]
		init[,2]=cos(seq(0,2*pi, length=tl))[-tl] 
}
  if (any(duplicated(init)))
  {
    init[duplicated(init),] <- init[duplicated(init),] + rnorm(prod(dim(init[duplicated(init),,drop=FALSE])),0,1e-10)
    warning("Duplciated initial placement found. Initial slightly pertubated.")
  }
  
x<-init[,1]
y<-init[,2]

# constraints:
if (is.null(constraints))
{
	Cx=Cy=rep(FALSE,vcount)
} else 
{
	Cx=!is.na(constraints[,1])
	Cy=!is.na(constraints[,2])
}

x[Cx]=constraints[Cx,1]
y[Cy]=constraints[Cy,2]

  #Symmetrize the graph, just in case
  #d<-symmetrize(d,rule="weak",return.as.edgelist=TRUE) 
  #Perform the layout calculation
  layout<-.C("qgraph_layout_fruchtermanreingold_R", as.integer(niter), as.integer(n), as.integer(ecount), as.double(max.delta),
	as.double(area), as.double(cool.exp), as.double(repulse.rad), as.integer(Ef),
	as.integer(Et), as.double(abs(weights)), as.double(x), as.double(y), as.integer(Cx), as.integer(Cy))
  #Return the result

  return(cbind(layout[[11]],layout[[12]]))
}


