#' Derivative of log-likelihood function (Equation 6)
#'
#' Returns the derivative of log-likelihood of each ith element with respect to g
#'
#' @param g An unknown vector of real numbers 
#' @param y A vector of outcomes of pairwise comparison of documents 
#'
#' @return A vector with ith element of gradient with respect to g
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname dLogLike
#' @export
#' 
dLogLike<-function(g,y){
  output<-c()
  for (i in 1:length(y)){
    num<-y[i]*dnorm(g[i])
    output[i]<-num/pnorm(y[i]*g[i])
  }
  return(output)
}


