# Copied from base R with some changes

pie2 <- function (x, label = "", 
                  radius = 0.8, 
                  pie.bord=.1, 
                  pie.col='white', 
                  pie.col2 = 'grey', 
                  bg = 'white', 
                  border.width = 1) 
  
{
  
  x <- c(1-x, x)
  
  # aux functions
  t2xy <- function(t, radius) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  
  
  # fixed quanities 
  init.angle <- 90
  edges = 200
  angle <- 45
  density = c(NULL, NULL)
  lty = c(NULL, NULL)
  clockwise = FALSE
  col = c(pie.col2, pie.col)
  border = c(TRUE, TRUE)
  radius2 <- radius - radius*pie.bord
  
  n <- 200
  
  
  # compute some aux variables 
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  twopi <-  2 * pi
  
  # set up plotting area
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  plot.window(xlim, ylim, "", asp = 1)
  par("fg")
  
  # browser()
  # plot pie chart
  for (i in 1L:nx) {
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n), radius)
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i], lwd = border.width)
  }
  
  border2 <- TRUE
  # plot node on top
  P2 <- t2xy(seq.int(x[1], x[3], length.out = n), radius2)
  polygon(c(P2$x, 0), c(P2$y, 0), density = NULL, angle = 45, border=TRUE, col=bg, lty=NULL, lwd = border.width)
  P3 <- t2xy(seq.int(x[1], x[3], length.out = n), radius2-radius2*.001)
  polygon(c(P2$x, 0), c(P2$y, 0), density = NULL, angle = 45, border=FALSE, col=bg, lty=NULL, lwd = border.width)
  
  # node labels
  text(0,0,label)
  
}


