# Map user space to inches space:
usr2inX <- function(x)
{
  usr <- par("usr")
  pin <- par("pin")
  (x-usr[1])/(usr[2]-usr[1]) * pin[1]
}

usr2inY <- function(x)
{
  usr <- par("usr")
  pin <- par("pin")
  (x-usr[3])/(usr[4]-usr[3]) * pin[2]
}

# Same but about origin (for atan2):
usr2inX2 <- function(x)
{
  usr <- par("usr")
  pin <- par("pin")
  x/(usr[2]-usr[1]) * pin[1]
}

usr2inY2 <- function(x)
{
  usr <- par("usr")
  pin <- par("pin")
  x/(usr[4]-usr[3]) * pin[2]
}
atan2usr2in <- function(x,y) atan2(usr2inX2(x),usr2inY2(y))%%(2*pi)

# Map inches space to user space:
in2usrX <- function(x)
{
  usr <- par("usr")
  pin <- par("pin")
  usr[1] + x/pin[1] * (usr[2] - usr[1])
}

in2usrY <- function(x)
{
  usr <- par("usr")
  pin <- par("pin")
  usr[3] + x/pin[2] * (usr[4] - usr[3])
}

## Find perpundicular poin to quantile of line:
PerpMid <- function(xy0,xy1,ang=1,cex=1,q=0.5)
{
  # Change xy0 to quantile:
  xy0 <- xy0 + q * (xy1 - xy0)
  
  # Fixed inches size:
  cexIn <-  cex * 0.025 * sqrt(sum(par("pin")^2))
  
  # Rotate about origin:
  xyr <- matrix(c(0,ang,-ang,0),2,2) %*% (c(usr2inX(xy1[1]),usr2inY(xy1[2])) - c(usr2inX(xy0[1]),usr2inY(xy0[2])))  
  
  # Rescale:
  xyr <- xyr * cexIn/sqrt(sum(xyr^2))
  
  # Add origin:
  xyr <- c(usr2inX(xy0[1]),usr2inY(xy0[2])) + xyr
  
  # Map to usr and return:
  return(c(in2usrX(xyr[1]),in2usrY(xyr[2])))
}
