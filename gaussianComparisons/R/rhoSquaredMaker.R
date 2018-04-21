#' Finding rho hat squared for equation 15 (top of page 5)
#'
#' Function that will calculate the Rasmussen and Williams equation
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
#' @rdname rhoSquaredMaker
#' @export
rhoSquaredMaker <- function(doc1, doc2, omega1, cleaned_data, sigma, tolerance){
  g_hat <- newtonMethod(omega1, cleaned_data$y, tolerance)
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
  omega_hat <- d2Psi(omega1, g_hat, cleaned_data$y)
  # omega_hat_inverse <- solve(omega_hat)
  matrix_mult1 <- omega_hat %*% little_omega
  matrix_mult2 <- little_omega_t %*% matrix_mult1
  rho_squared <- 2 * sigma^2 - matrix_mult2
  return(rho_squared)
}
