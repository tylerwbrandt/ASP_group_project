omega_test <- omega(gdata, 2)
g_test <- sample(1:10, 11, replace = T)

## Derivative of the log prior function

dlogpri <- function(omega, g){
  if (det(omega) == 0){
    diag(omega) <- diag(omega) + .01
  }
  inv_omega <- solve(omega)
  neg_inv_omega <- -inv_omega
  return (neg_inv_omega %*% g)
}

dlogpri(omega_test, g_test)

diag(omega_test) <- diag(omega_test) + .01
(-solve(omega_test)) %*% g_test


