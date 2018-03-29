omega_test <- omega(gdata, 2)
omega_test <- diag(8,11,11)
omega_test[2,1] <- omega_test[1,2] <- -4

dlogpri <- function(omega, g){
  if (det(omega) == 0){
    diag(omega) <- diag(omega) + .001
  }
  inv_omega <- solve(omega)
  neg_inv_omega <- -inv_omega
  return (neg_inv_omega %*% g)
}


solve(omega_test)
omega_test
diag(omega_test) <- diag(omega_test) + .01

det(omega_test)
