## Equation 9.5

newton_method_prequel <- function(omega, g, y){
  rightVec <- dpsi(omega, g, y)
  leftMatrix <- solve(d2psi(omega, g, y))
  matrixMult <- leftMatrix %*% rightVec
  return (matrixMult)
}

newton_method <- function(omega, g, y, tolerance){
  g_1 <- g
  g_2 <- g_1 - newton_method_prequel(omega, g, y)
  # Make sure each entry is less than the tolerance
  while (sum(ifelse(abs(g_2 - g_1) < tolerance, 1, 0)) != length(g_2)){
    g_1 <- g_2
    g_2 <- g_1 - newton_method_prequel(omega, g_1, y)
  }
  return (g_2)
}

newton_method_prequel(omega_test, g_test, y_test)
newton_method(omega_test, g_test, y_test, .01)
