#' Finding final s (bottom of page 5)
#' Function that will the s function
#'
#' @param doc1 The document id of the first document you wish to compare
#' @param doc2 The document id of the second document you wish to compare
#' @param dataset The dataset you are given with comparisons
#' @param g An unknown vector of real numbers 
#' @param sigma The hyperprior chosen to identify the latent space
#' @param tolerance The tolerance to use for Newton Method convergence
#'
#' @return A double
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname finalS
#' @export
finalS<-function(doc1, doc2, dataset, g, sigma, tolerance){
  hPart<-lowersaseH(muMaker(doc1, doc2, dataset, g, sigma, tolerance)/sqrt(1+rhoSquaredMaker(doc1, doc2, dataset, g, sigma, tolerance)))
  ePart<- bigEGstar(doc1, doc2, dataset, g, sigma, tolerance)
  return(hPart-ePart)
}