qgraph.arrow=function(x,y,x.orig,y.orig,length,angle=30*pi/180,lwd,col="black",
	open=TRUE,Xasp=1,lty=1)
{
  warning("This function is no longer supported. Use qgraph:::DrawArrow")
x1=x+(x.orig-x)*Xasp
x2=x
y1=y.orig
y2=y

r=angle
l=length

dx=x2-x1
dy=y2-y1
d=sqrt(dx^2/Xasp+dy^2)

px=x2-dx*l/d
py=y2-dy*l/d

Rx = cos(r) * (px-x2) - sin(r) * (py-y2) + x2
Ry = sin(r) * (px-x2) + cos(r) * (py-y2) + y2

Lx = cos(-r) * (px-x2) - sin(-r) * (py-y2) + x2
Ly = sin(-r) * (px-x2) + cos(-r) * (py-y2) + y2

if (open) 
{
  lines((c(Rx,x,Lx)-x)/Xasp+x,c(Ry,y,Ly),lwd=lwd,col=col,lty=lty)
} else {
  polygon((c(Lx,x2,Rx)-x)/Xasp+x,c(Ly,y2,Ry),lwd=lwd,col=col,border=NA)
}
}

