colorblind <- function(n, shift = 0){
  # Taken from: http://jfly.iam.u-tokyo.ac.jp/color/
  if (n > 7) warning("'colorblind' palette only supports 8 colors.")
  Palette <- 
    rgb(
      c(230,86,0,240,204,213,0),
      c(159,180,158,228,121,94,114),
      c(0,233,115,66,167,0,178),
      maxColorValue=255)
  
  Palette[(((shift + 1:n)-1)%%8)+1]
}

# ggplot theme:
ggplot_palette <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Fiftyshades:
shadesOfGrey <- colorRampPalette(c("grey0", "grey100"))
