#' Derivative of the log of the numerator of the full posterior function (Equation 8)
#'
#' Adds to functions that are already created (derivative of log prior, derivative of log likelihood)
#'
#' @param omega A matrix which contains information about how a document is compared with all other documents.
#' @param g An unknown vector of real numbers 
#' @param y A vector of outcomes of pairwise comparison of documents 
#'
#' @return A vector representing the derivative of psi 
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname dPsi
#' @export
#' 
dPsi <- function(omega, g, y){
  a <- dLogPri(omega, g)
  b <- dLogLike(g,y)
  return (a + b)
}



