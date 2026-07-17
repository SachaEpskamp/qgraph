# Black or white, whichever contrasts with the (background) colour. Used for
# titles and legend text, which are otherwise drawn in black and would be
# invisible on the dark background set by themes such as 'neon' and
# 'vaporwave', or a dark user-supplied 'bg'.
contrastColor <- function(bg = par("bg"))
{
  light <- tryCatch(mean(col2rgb(bg)/255) > 0.5, error = function(e) TRUE)
  if (identical(bg, "transparent")) light <- TRUE
  if (light) "black" else "white"
}

addTitle <- function(x, cex = 1, col)
{
  # Default to black or white by contrast with the background, the same way
  # node labels are handled.
  if (missing(col) || is.null(col))
  {
    col <- contrastColor()
  }
  text(par('usr')[1] + (par('usr')[2] - par('usr')[1])/40 ,par("usr")[4] - (par('usr')[4] - par('usr')[3])/40,x, adj = c(0,1),cex=cex, col=col)
}
