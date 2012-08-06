# Original code from sem package, by John Fox and Adam Kramer.

standcoefmsem <- function (object, ...) 
{
  Res <- list()
  groups <- object$groups
  G <- length(groups)
  param.names <- object$param.names
  ram <- object$ram
  A <- object$A
  P <- object$P
  par <- coef(object)
  for (g in 1:G) {
    par.names <- param.names[ram[[g]][, 4]]
    par.gr <- par[par.names]
    t <- length(par.gr)
    par.posn <- ram[[g]][, 4] != 0
    ram[[g]][par.posn, 4] <- 1:t
    group <- list(coeff = par.gr, t = t, ram = ram[[g]], 
                  A = A[[g]], P = P[[g]], par.posn = par.posn, param.names = par.names)
    class(group) <- "sem"
    Res[[g]] <- standardizedCoefficients(group, ...)
  }
  return(Res)
}