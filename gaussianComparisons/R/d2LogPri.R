#' Second derivative of the log prior function (Equation 5)
#'
#' Returns the negative of the inverse of the omega matrix  
#'
#' @param omega A matrix which contains information about how a document is compared with all other documents.  
#' @param g An unknown vector of real numbers 
#'
#' @return A matrix that is the second derivative of the log prior function 
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname d2LogPri
#' @export
#' 
d2LogPri<-function(omega,g){
  if (det(omega) == 0){
    diag(omega) <- diag(omega)+.01
  }
  inv_omega <- (solve(omega))
  neg_inv_omega <- -1*(inv_omega)
  return(neg_inv_omega)
}



