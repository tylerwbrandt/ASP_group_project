#' Newton method prequel, called in newtonMethod (Equation 9.5)
#'
#' Function needed for the Newton Approximation method
#'
#' @param omega A matrix which contains information about how a document is compared with all other documents.
#' @param g An unknown vector of real numbers 
#' @param y A vector of outcomes of pairwise comparison of documents 
#'
#' @return A vector
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname newtonMethodPrequel
#' @export
#' 
newtonMethodPrequel <- function(omega, g, y){
  rightVec <- dPsi(omega, g, y)
  leftMatrix <- solve(d2Psi(omega, g, y))
  matrixMult <- leftMatrix %*% rightVec
  return (matrixMult)
}

