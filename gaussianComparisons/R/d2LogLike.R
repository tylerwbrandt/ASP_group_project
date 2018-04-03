#' Second derivative of log-likelihood function (Equation 7)
#'
#' Returns the second derivative of log-likelihood of each ith element with respect to g
#'
#' @param g An unknown vector of real numbers 
#' @param y A vector of outcomes of pairwise comparison of documents 
#'
#' @return A diagonal Hessian matrix, with ith element along the diagonal 
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname d2LogLike
#' @export
#' 
d2LogLike<-function(g,y){
  derivativeLL<-dLogLike(g,y)
  output<-diag(x=0,length(y),length(y))
  for (i in 1:length(y)){
    firstTerm<- ((dnorm(g[i]))^2)/(pnorm(y[i]*g[i])^2)
    secondTerm<-g[i]*derivativeLL[i]
    output[i,i]<-(-1*firstTerm)-secondTerm
  }
  return(output)
}

