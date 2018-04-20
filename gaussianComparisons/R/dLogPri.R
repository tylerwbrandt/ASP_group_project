#' Derivative of the log prior function (Equation 4)
#'
#' Returns the product of the negative of the inverse of the omega matrix multiplied by the g vector 
#'
#' @param omega A matrix which contains information about how a document is compared with all other documents.  
#' @param g An unknown vector of real numbers 
#'
#' @return A vector that is the derivative of the log prior function 
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname dLogPri
#' @export

dLogPri <- function(omega, g){
  if (det(omega) == 0){
    diag(omega) <- diag(omega) + .01
  }
  inv_omega <- solve(omega)
  neg_inv_omega <- -1*inv_omega
  return (neg_inv_omega %*% g)
}




