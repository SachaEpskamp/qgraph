# This function was copied directly from D3network package:
# https://github.com/christophergandrud/d3Network/blob/cc224daa983e59dbe743c18e1ccb621bdd0ce509/R/utils.R

toJSONarray <- function(dtf){
  clnms <- colnames(dtf)
  
  name.value <- function(i){
    quote <- '';
    if(class(dtf[, i])!='numeric' && class(dtf[, i])!='integer'){
      quote <- '"';
    }
    paste('"', i, '" : ', quote, dtf[,i], quote, sep='')
  }
  objs <- apply(sapply(clnms, name.value), 1, function(x){paste(x, 
                                                                collapse=', ')})
  objs <- paste('{', objs, '}')
  
  res <- paste('[', paste(objs, collapse=', '), ']')
  
  return(res)
}