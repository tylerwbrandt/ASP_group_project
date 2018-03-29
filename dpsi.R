dpsi <- function(omega, g, y){
  a <- dlogpri(omega, g)
  b <- dLogLike(y,g)
  return (a + b)
}

y_test <- gdata$y

dlogpri(omega_test, g_test)
dLogLike(y_test, g_test)

dlogpri(omega_test, g_test) + dLogLike(y_test, g_test)
dpsi(omega_test, g_test, y_test) == dlogpri(omega_test, g_test) + dLogLike(y_test, g_test)

