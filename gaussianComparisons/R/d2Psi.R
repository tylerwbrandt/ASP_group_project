#' Second derivative of the log of the numerator of the full posterior function (Equation 9)
#'
#' Adds to functions that are already created (derivative of log prior, derivative of log likelihood)
#'
#' @param omega A matrix which contains information about how a document is compared with all other documents.
#' @param g An unknown vector of real numbers 
#' @param y A vector of outcomes of pairwise comparison of documents 
#'
#' @return A matrix representing the second derivative of psi 
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname d2Psi
#' @export
#' 
d2Psi<- function(omega,g, y){  
    output<-(d2LogLike(g,y)+d2LogPri(omega,g))
    return(output)
}

