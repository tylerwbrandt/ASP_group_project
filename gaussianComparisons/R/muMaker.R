#' Finding mu hat for equation 14 (bottom of page 4)
#'
#' Function that will find expected value of g for a new comparison
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
#' @rdname muMaker
#' @export
muMaker <- function(doc1, doc2, omega1, cleaned_data, sigma, tolerance){
  # cleaned_data <- cleaner(dataset)
  # omega1 <- omega(dataset, sigma)
  g_hat <- newtonMethod(omega1, cleaned_data$y, tolerance)
  log_like_matrix <- dLogLike(g_hat, cleaned_data$y)
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
  little_omega_t <- t(little_omega)
  mu_hat <- little_omega_t %*% log_like_matrix
  return (mu_hat[1][1])
}

