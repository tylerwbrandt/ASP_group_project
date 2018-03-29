## Function needed for the Newton Approximation method
## Called in newton_method (Equation 9.5)

newton_method_prequel <- function(omega, g, y){
  rightVec <- dpsi(omega, g, y)
  leftMatrix <- solve(d2psi(omega, g, y))
  matrixMult <- leftMatrix %*% rightVec
  return (matrixMult)
}

newton_method_prequel(omega_test, g_test, y_test)
