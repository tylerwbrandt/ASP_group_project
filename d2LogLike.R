#Found second derivative of log-likelihood
d2LogLike<-function(y,g){
  derivativeLL<-dLogLike(y,g)
  output<-diag(x=0,length(y),length(y))
  for (i in 1:length(y)){
    firstTerm<- ((dnorm(g[i]))^2)/(pnorm(y[i]*g[i])^2)
    secondTerm<-g[i]*derivativeLL[i]
    output[i,i]<-(-1*firstTerm)-secondTerm
  }
  return(output)
}

y1<-c(1,1,-1,1,-1)
g1<-c(2,3,4,5,6)
logLike(y1,g1)
dLogLike(y1,g1)
d2LogLike(y1,g1)
