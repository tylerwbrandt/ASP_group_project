#' Finding maximum information comparison
#'
#' Function that will calculate the comparison that gives the most information
#'
#' @param dataset The dataset you are given with comparisons
#' @param sigma The hyperprior chosen to identify the latent space
#' @param tolerance The tolerance to use for Newton Method convergence
#' @param nComp The number of comparions that the user wants to do in the next round of comparisons 
#'
#' @return A vector with two entries containing document ids for your new comparison
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname maxInfoComp
#' @export
maxInfoComp <- function(dataset, sigma, tolerance, nComp){
  x <- combn(unique(dataset$document_id), 2)
  omega1 <- omega(dataset, sigma)
  # Calculate s values for each new comparison
  sSet <- list()
  for (i in 1:ncol(x)){
    sValue <- finalS(x[1,i], x[2,i], omega1, cleaner(dataset), sigma, tolerance)
    sSet[[i]] <- c(x[1,i], x[2,i], sValue)
  }
  # Find maximum information
  sSet <- sSet[order(sapply(sSet, function(x) x[3]), decreasing = TRUE)]
  # Return comparison document ids
  return (sSet[1:nComp])
}

