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
#' @rdname Omega
#' @export

## May have to change the input to this function regarding the dataset
## It is possible that we have to work with the SQL dataset and change it into what we need first
omega <-function(dataset,sigma){
  omega_matrix <- matrix(data = 0, nrow=(nrow(dataset)),ncol=(nrow(dataset)))
  ## Iterate through matrix by row and column. Find entries comparisons that contain common doc
  ## Add sigma^2 or negative sigma^2 to those entries
  for(i in 1:(nrow(dataset))){
    for (j in 1:(nrow(dataset))){
      if(dataset$first[i] == dataset$first[j]){
        omega_matrix[i,j] <- omega_matrix[i,j] + sigma^2
      }
      if(dataset$first[i] == dataset$second[j]){
        omega_matrix[i,j] <- omega_matrix[i,j] - sigma^2
      }
      if(dataset$second[i] == dataset$first[j]){
        omega_matrix[i,j] <- omega_matrix[i,j] - sigma^2
      }
      if(dataset$second[i] == dataset$second[j]){
        omega_matrix[i,j] <- omega_matrix[i,j] + sigma^2
      }
    }
  }
  return (omega_matrix)
}


