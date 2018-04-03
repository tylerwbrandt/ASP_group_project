#' Second derivative of the log of the numerator of the full posterior function (Equation 9)
#'
#' Adds to functions that are already created (derivative of log prior, derivative of log likelihood)
#'
#' @param omega A matrix which contains information about how a document is compared with all other documents.
#' @param g An unknown vector of real numbers 
#' @param y A vector of outcomes of pairwise comparison of documents 
#'
#' @return A vector representing the second derivative of psi 
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname d2Psi
#' @export
#' 
d2psi<- function(omega,g, y){  
    output<-(d2LogLike(g,y)+d2logpri(omega,g))
    return(output)
}

