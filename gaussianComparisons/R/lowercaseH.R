#' lowercase H function at the top of page 6
#'
#' Function to be used to calculate capital H
#'
#' @param x a number between 0 and 1
#'
#' @return A double
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname lowercaseH
#' @export
lowercaseH <- function(x){
  part1 <- -x * log(x)
  part2 <- (1-x) * log(1-x)
  output <- part1 - part2
  return (output)
}
