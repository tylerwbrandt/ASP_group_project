dLogLike<-function(y,g){
  output<-c()
  for (i in 1:length(y)){
    num<-y[i]*dnorm(g[i])
    output[i]<-num/pnorm(y[i]*g[i])
  }
  return(output)
}

y1<-c(1,1,-1,1,-1)
g1<-c(2,3,4,5,6)
logLike(y1,g1)
dLogLike(y1,g1)
