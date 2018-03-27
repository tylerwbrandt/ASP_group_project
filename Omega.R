gdata <- data.frame(c(1,1,1,2,2,2,3,3,4,4,4), c(2,3,4,1,3,4,1,4,1,2,3),
                    c(1,1,1,-1,-1,-1,-1,1,-1,1,-1))
colnames(gdata) <- c("first", "second", "y")
View(gdata)

omega <-function(dataset,sigma){
  omega_matrix <- matrix(data = 0, nrow=(nrow(dataset)),ncol=(nrow(dataset)))
  for(i in 1:(nrow(data))){
    for (j in 1:(nrow(data))){
      if(data$first[i] == data$first[j]){
        
      }
      else if(data$first[i] == data$second[j]){
        
      }
      else if(data$second[i] == data$first[j]){
        
      }
      else if(data$second[i] == data$second[j]){
        
      }
    }
  }
}

