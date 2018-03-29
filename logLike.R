# Log-likelihood function 

logLike<-function(y,g){
  result<-c()
  for (i in 1:length(y)){   # obtains CDF of each y[i]*g[i]
    x<-y[i]*g[i]
    xi<-log(pnorm(x,0,1))
    result<-c(result,xi)
  }
  output<-sum(result)       # sum of all CDF
  return(output)            # returns sum of all CDF 
}


# Sample data 
y1<-c(1,1,-1,1,-1)
g1<-c(2,3,4,5,6)
logLike(y1,g1)
