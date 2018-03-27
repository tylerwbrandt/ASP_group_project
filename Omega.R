gdata <- data.frame(c(1,1,1,2,2,2,3,3,4,4,4), c(2,3,4,1,3,4,1,4,1,2,3),
                    c(1,1,1,-1,-1,-1,-1,1,-1,1,-1))
colnames(gdata) <- c("first", "second", "y")
View(gdata)

omega <-function(gdata,sigma){
  omega_matrix <- matrix(data = NA, nrow=(length(gdata)),ncol=(length(gdata)))
  for(i in 1:(length(glist))){
    for (j in 1:(length(glist))){
      if(i[1]==j[1]&i[2]==j[2]){
        
      }
      else if(i[1]==j[2]&i[2]==j[1]){
        
      }
      else if(i[1]==j[1]){
        
      }
      else if(i[1]==j[2]){
        
      }
      else if(i[2]==j[1]){
        
      }
      else if(i[2]==j[2]){
        
      }
    }
  }
}