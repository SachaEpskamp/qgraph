xkcd_jitter <- function(x, y) {
  len <- length(x)
  rg <- par("usr")
  yjitter <- (rg[4] - rg[3]) / 1000
  xjitter <- (rg[2] - rg[1]) / 1000
  x_mod <- x + rnorm(len) * xjitter
  y_mod <- y + rnorm(len) * yjitter
  return(list(x=x_mod,y=y_mod))
}

#