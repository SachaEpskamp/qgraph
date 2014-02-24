fixnames <- function(x,name="")
{
  if (is.null(names(x)))
  {
    names(x) <- paste0(name,seq_along(x))
  }
  
  return(ifelse(names(x)=='',paste0(name,seq_along(x)), names(x)))
}