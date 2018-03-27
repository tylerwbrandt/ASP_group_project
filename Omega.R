gdata <- data.frame(c(1,1,1,2,2,2,3,3,4,4,4), c(2,3,4,1,3,4,1,4,1,2,3),
                    c(1,1,1,-1,-1,-1,-1,1,-1,1,-1))
colnames(gdata) <- c("first", "second", "y")
View(gdata)

omega<-function(glist,sigma){
  matrix(data = NA, nrow=(length(glist)),ncol=(length(glist)))
  for(i in 1:(length(glist))){
    for (j in 1:(length(glist))){
      
    }
  }
}