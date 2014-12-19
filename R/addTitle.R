addTitle <- function(x)
{
  text(par('usr')[1] + (par('usr')[2] - par('usr')[1])/40 ,par("usr")[4] - (par('usr')[4] - par('usr')[3])/40,x, adj = c(0,1))       
}
