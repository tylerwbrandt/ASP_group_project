muMaker <- function(doc1, doc2, dataset, g, sigma, tolerance){
  cleaned_data <- cleaner(dataset)
  omega1 <- omega(dataset, sigma)
  g_hat <- newtonMethod(omega1, g, cleaned_data$y, tolerance)
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
  return (mu_hat)
}

