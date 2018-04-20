#' Finding big E of g star  (middle of page 6)
#'
#' Function that will find the second term of the s function
#'
#' @param doc1 The document id of the first document you wish to compare
#' @param doc2 The document id of the second document you wish to compare
#' @param omega1 The omega matrix
#' @param cleaned_data The dataset you are given with comparisons, cleaned using cleaner 
#' @param sigma The hyperprior chosen to identify the latent space
#' @param tolerance The tolerance to use for Newton Method convergence
#'
#' @return A double
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname bigEGStar
#' @export
bigEGStar<-function(doc1, doc2, omega1, cleaned_data, sigma, tolerance){
  c<-sqrt(pi*log(2)/2)
  rhoSquared<-rhoSquaredMaker(doc1, doc2, omega1, cleaned_data, sigma, tolerance)
  mu<-muMaker(doc1, doc2, omega1, cleaned_data, sigma, tolerance)
  outside<-c/sqrt(rhoSquared+(c^2))
  inside<-exp(-1*mu^2/(2*rhoSquared+(c^2)))
  return(outside*inside)
}