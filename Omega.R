gdata <- data.frame(c(1,1,1,2,2,2,3,3,4,4,4), c(2,3,4,1,3,4,1,4,1,2,3),
                    c(1,1,1,-1,-1,-1,-1,1,-1,1,-1))
colnames(gdata) <- c("first", "second", "y")
View(gdata)

## May have to change the input to this function regarding the dataset
## It is possible that we have to work with the SQL dataset and change it into what we need first

omega <-function(dataset,sigma){
  omega_matrix <- matrix(data = 0, nrow=(nrow(dataset)),ncol=(nrow(dataset)))
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

omega(gdata, 2)


