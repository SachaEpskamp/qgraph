addTitle <- function(x, cex = 1, col)
{
  # Without a colour the title is drawn in black, which is invisible on the
  # dark background set by themes such as 'neon', 'vaporwave' and 'dracula'.
  # Default to black or white by contrast with the background, the same way
  # node labels are handled.
  if (missing(col) || is.null(col))
  {
    bg <- par("bg")
    light <- tryCatch(mean(col2rgb(bg)/255) > 0.5, error = function(e) TRUE)
    if (identical(bg, "transparent")) light <- TRUE
    col <- if (light) "black" else "white"
  }
  text(par('usr')[1] + (par('usr')[2] - par('usr')[1])/40 ,par("usr")[4] - (par('usr')[4] - par('usr')[3])/40,x, adj = c(0,1),cex=cex, col=col)
}
