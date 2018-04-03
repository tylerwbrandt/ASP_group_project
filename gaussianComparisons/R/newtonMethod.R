#' Newton appriximation function (Equation 9.5)
#'
#' Calculating the approximate distribution using Newton's method to iteratively to find gHat
#'
#' @param omega A matrix which contains information about how a document is compared with all other documents.
#' @param g An unknown vector of real numbers 
#' @param y A vector of outcomes of pairwise comparison of documents 
#' @param tolerance A given value of tolerance 
#'
#' @return A vector gHat which is the approximate distribution
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname newtonMethod
#' @export
#' 
newtonMethod <- function(omega, g, y, tolerance){
  g_1 <- g
  g_2 <- g_1 - newtonMethodPrequel(omega, g, y)
  # Make sure each entry is less than the tolerance
  while (sum(ifelse(abs(g_2 - g_1) < tolerance, 1, 0)) != length(g_2)){
    g_1 <- g_2
    g_2 <- g_1 - newtonMethodPrequel(omega, g_1, y)
  }
  return (g_2)
}
