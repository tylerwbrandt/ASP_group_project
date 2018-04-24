#' Finding final s (bottom of page 5)
#' 
#' Function that will the s function
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
#' @rdname finalS
#' @export
finalS<-function(doc1, doc2, omega1, cleaned_data, sigma, tolerance){
  little_omega <- rep(0, nrow(cleaned_data))
  for (i in 1:nrow(cleaned_data)){
    if (doc1 == cleaned_data$first[i]){
      little_omega[i] <- little_omega[i] + sigma^2
    }
    if (doc1 == cleaned_data$second[i]){
      little_omega[i] <- little_omega[i] - sigma^2
    }
    if (doc2 == cleaned_data$first[i]){
      little_omega[i] <- little_omega[i] - sigma^2
    }
    if (doc2 == cleaned_data$second[i]){
      little_omega[i] <- little_omega[i] + sigma^2
    }
  }
  hPart<-lowercaseH(muMaker(doc1, doc2, omega1, cleaned_data, sigma, tolerance, little_omega)/sqrt(1+rhoSquaredMaker(doc1, doc2, omega1, cleaned_data, sigma, tolerance, little_omega)))
  ePart<- bigEGStar(doc1, doc2, omega1, cleaned_data, sigma, tolerance, little_omega)
  return(hPart-ePart)
}