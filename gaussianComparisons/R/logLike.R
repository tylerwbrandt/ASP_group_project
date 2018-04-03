#' Log-likelihood function (Equation 5.9)
#'
#' Returns the log-likelihood given a set of observed preferences y and the difference function g
#'
#' @param g An unknown vector of real numbers 
#' @param y A vector of outcomes of pairwise comparison of documents 
#'
#' @return A vector containing the log-likelihood of each comparison 
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname logLike
#' @export
#' 
logLike<-function(g,y){
  result<-c()
  for (i in 1:length(y)){   # obtains CDF of each y[i]*g[i]
    x<-y[i]*g[i]
    xi<-log(pnorm(x,0,1))
    result<-c(result,xi)
  }
  output<-sum(result)       # sum of all CDF
  return(output)            # returns sum of all CDF 
}
