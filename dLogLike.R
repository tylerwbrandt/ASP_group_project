#Found derivative of log-likelihood
#Returned vector with ith element of gradient with respect to g
dLogLike<-function(y,g){
  output<-c()
  for (i in 1:length(y)){
    num<-y[i]*dnorm(g[i])
    output[i]<-num/pnorm(y[i]*g[i])
  }
  return(output)
}

#Example
y1<-c(1,1,-1,1,-1)
g1<-c(2,3,4,5,6)
logLike(y1,g1)
dLogLike(y1,g1)
