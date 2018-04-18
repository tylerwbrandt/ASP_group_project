#' Finding maximum information comparison
#'
#' Function that will calculate the comparison that gives the most information
#'
#' @param dataset The dataset you are given with comparisons
#' @param sigma The hyperprior chosen to identify the latent space
#' @param tolerance The tolerance to use for Newton Method convergence
#'
#' @return A vector with two entries containing document ids for your new comparison
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname maxInfoComp
#' @export
maxInfoComp <- function(dataset, sigma, tolerance){
  x <- combn(unique(dataset$document_id), 2)
  new_data <- cleaner(dataset)
  # Remove comparisons already made
  for (i in 1:ncol(x)){
    j <- 1
    while (j <= nrow(new_data)){
      if (x[1, i] == new_data$first[j] & x[2, i] == new_data$second[j]){
        x[1,i] <- 0
        x[2,i] <- 0
        j <- 0
      } else if (x[1, i] == new_data$second[j] & x[2, i] == new_data$first[j]){
        x[1,i] <- 0
        x[2,i] <- 0
        j <- 0
      }
      j <- j + 1
    }
  }
  i <- 1
  while (i <= ncol(x)){
    if (x[1,i]==0 & x[2,i]==0){
      x<-x[,-i]
      i<-0
    }
    i<-i+1
  }
  # Calculate s values for each new comparison
  sSet <- NULL
  for (i in 1:ncol(x)){
    sValue <- finalS(x[1,i], x[2,i], data_montgomery, 1, .01)
    sSet <- c(sSet, sValue)
  }
  # Find maximum information
  indexMax <- which.max(sSet)
  # Return comparison document ids
  return (c(x[1][indexMax], x[2][indexMax]))
}



