#' Calculating the omega matrix (Equation 1.5)
#'
#' Returns the omega matrix given an input of pair-wise comparisons and a sigma value 
#'
#' @param dataset A dataframe of comparisons, with each document being identified by an unique id. 
#' @param sigma A numeric value of sigma, which is a hyper-prior chosen to identify the latent space. 
#'
#' @return A matrix which contains information about how a document is compared with all other documents. 
#' @author Group <\email{group@@wustl.edu}
#' 
#' @rdname omega
#' @export
omega <-function(dataset,sigma){
  new_data <- cleaner(dataset)
  omega_matrix <- matrix(data = 0, nrow=(nrow(new_data)),ncol=(nrow(new_data)))
  ## Iterate through matrix by row and column. Find entries comparisons that contain common doc
  ## Add sigma^2 or negative sigma^2 to those entries
  for(i in 1:(nrow(new_data))){
    for (j in 1:(nrow(new_data))){
      if(new_data$first[i] == new_data$first[j]){
        omega_matrix[i,j] <- omega_matrix[i,j] + sigma^2
      }
      if(new_data$first[i] == new_data$second[j]){
        omega_matrix[i,j] <- omega_matrix[i,j] - sigma^2
      }
      if(new_data$second[i] == new_data$first[j]){
        omega_matrix[i,j] <- omega_matrix[i,j] - sigma^2
      }
      if(new_data$second[i] == new_data$second[j]){
        omega_matrix[i,j] <- omega_matrix[i,j] + sigma^2
      }
    }
  }
  return (omega_matrix)
}
