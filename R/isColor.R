isColor <- function(x) {
  sapply(x, function(X) {
    if (!is.logical(X)) tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE) else FALSE
  })
}