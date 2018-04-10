#' Finding prior of g (equation 1)
#'
#' Function that will calculate the prior on g
#'
#' @param omega the omega matrix 
#'
#' @return A vector, the prior on g 
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname gPrior
#' @export

gPrior<-function(omega){
  gPrior<-mvrnorm(n=1,rep(0,ncol(omega)),omega)
}
