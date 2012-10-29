xkcd_jitter <- function(x, y, jit = 1000) {
  len <- length(x)
  rg <- par("usr")
  yjitter <- (rg[4] - rg[3]) / jit
  xjitter <- (rg[2] - rg[1]) / jit
  x_mod <- x + rnorm(len) * xjitter
  y_mod <- y + rnorm(len) * yjitter
  return(list(x=x_mod,y=y_mod))
}

#