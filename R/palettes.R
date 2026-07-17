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

# Journal-style palettes, using the colours documented in the ggsci package
# (https://nanx.me/ggsci/):

# Nature Publishing Group:
npg <- function(n, shift = 0){
  if (n > 10) warning("'npg' palette only supports 10 colors.")
  Palette <-
    c(
      "#E64B35",
      "#4DBBD5",
      "#00A087",
      "#3C5488",
      "#F39B7F",
      "#8491B4",
      "#91D1C2",
      "#DC0000",
      "#7E6148",
      "#B09C85"
    )

  Palette[(((shift + 1:n)-1)%%10)+1]
}

# The Lancet (Oncology):
lancet <- function(n, shift = 0){
  if (n > 9) warning("'lancet' palette only supports 9 colors.")
  Palette <-
    c(
      "#00468B",
      "#ED0000",
      "#42B540",
      "#0099B4",
      "#925E9F",
      "#FDAF91",
      "#AD002A",
      "#ADB6B6",
      "#1B1919"
    )

  Palette[(((shift + 1:n)-1)%%9)+1]
}

# Journal of the American Medical Association:
jama <- function(n, shift = 0){
  if (n > 7) warning("'jama' palette only supports 7 colors.")
  Palette <-
    c(
      "#374E55",
      "#DF8F44",
      "#00A1D5",
      "#B24745",
      "#79AF97",
      "#6A6599",
      "#80796B"
    )

  Palette[(((shift + 1:n)-1)%%7)+1]
}

# New England Journal of Medicine:
nejm <- function(n, shift = 0){
  if (n > 8) warning("'nejm' palette only supports 8 colors.")
  Palette <-
    c(
      "#BC3C29",
      "#0072B5",
      "#E18727",
      "#20854E",
      "#7876B1",
      "#6F99AD",
      "#FFDC91",
      "#EE4C97"
    )

  Palette[(((shift + 1:n)-1)%%8)+1]
}

# Science (AAAS):
aaas <- function(n, shift = 0){
  if (n > 10) warning("'aaas' palette only supports 10 colors.")
  Palette <-
    c(
      "#3B4992",
      "#EE0000",
      "#008B45",
      "#631879",
      "#008280",
      "#BB0021",
      "#5F559B",
      "#A20056",
      "#808180",
      "#1B1919"
    )

  Palette[(((shift + 1:n)-1)%%10)+1]
}

# Gothic vampire palette, intended for the parchment background the theme
# sets. All colours are dark, so the automatic node-label contrast gives
# white labels. Ordered so that neighbouring groups stay far apart in hue
# (the two reds, crimson and velvet, are maximally separated).
dracula <- function(n, shift = 0){
  if (n > 6) warning("'dracula' palette only supports 6 colors.")
  Palette <-
    c(
      "#A4133C", # crimson
      "#22223B", # midnight ink
      "#A98307", # old gold (candlelight)
      "#4B244A", # gothic plum
      "#344E41", # graveyard yew
      "#6D213C"  # velvet burgundy
    )

  Palette[(((shift + 1:n)-1)%%6)+1]
}
