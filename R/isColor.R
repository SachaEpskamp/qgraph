isColor <- function(x)
{
  return(x%in%colors() | grepl("^#(\\d|[a-f]){6,8}$",x,ignore.case=TRUE))
}