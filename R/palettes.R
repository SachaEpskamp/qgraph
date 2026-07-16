colorblind <- function(n, shift = 0){
  # Taken from: http://jfly.iam.u-tokyo.ac.jp/color/
  if (n > 8) warning("'colorblind' palette only supports 8 colors.")
  Palette <-
    rgb(
      c(230,86,0,240,204,213,0,0),
      c(159,180,158,228,121,94,114,0),
      c(0,233,115,66,167,0,178,0),
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


neon <- function(n, shift = 0){
  if (n > 6) warning("'neon' palette only supports 6 colors.")
  Palette <-
    c(
      "#ff3f3f",
      "#99FC20",
      "#ff48c4",
      "#f3ea5f",
      "#c04df9",
      "#2bd1fc"
    )

  Palette[(((shift + 1:n)-1)%%6)+1]
}

# Soft candy pastels. Ordered so that neighbouring groups stay far apart in
# colour: every adjacent pair is >= 29 in normal-vision and >= 8 in
# deuteranope/protanope OKLab dE, comparable to the existing 'neon' and 'pride'
# palettes. (For a palette chosen for colour-vision deficiency, use
# theme/palette "colorblind".)
kawaii <- function(n, shift = 0){
  if (n > 6) warning("'kawaii' palette only supports 6 colors.")
  Palette <-
    c(
      "#FF5FA2", # bubblegum
      "#00BCD4", # soda blue
      "#FFD23F", # custard
      "#9B5DE5", # taro
      "#3DD68C", # matcha
      "#F4664A"  # strawberry
    )

  Palette[(((shift + 1:n)-1)%%6)+1]
}

# 1980s retro-futurism, intended for the dark background the theme sets.
vaporwave <- function(n, shift = 0){
  if (n > 6) warning("'vaporwave' palette only supports 6 colors.")
  Palette <-
    c(
      "#FF71CE", # hot pink
      "#05FFA1", # mint
      "#B967FF", # violet
      "#FFFB96", # pale lemon
      "#01CDFE", # cyan
      "#FF6E4A"  # sunset
    )

  Palette[(((shift + 1:n)-1)%%6)+1]
}

# The Dracula palette (https://draculatheme.com), intended for the dark
# background the theme sets.
dracula <- function(n, shift = 0){
  if (n > 6) warning("'dracula' palette only supports 6 colors.")
  Palette <-
    c(
      "#FF79C6", # pink
      "#50FA7B", # green
      "#BD93F9", # purple
      "#F1FA8C", # yellow
      "#8BE9FD", # cyan
      "#FFB86C"  # orange
    )

  Palette[(((shift + 1:n)-1)%%6)+1]
}
