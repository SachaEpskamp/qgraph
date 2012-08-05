

usr2inX <- function(x)
{
  usr <- par("usr")
  pin <- par("pin")
  x/(usr[2]-usr[1]) * pin[1]
}

usr2inY <- function(x)
{
  usr <- par("usr")
  pin <- par("pin")
  x/(usr[4]-usr[3]) * pin[2]
}

atan2usr2in <- function(x,y) atan2(usr2inX(x),usr2inY(y))