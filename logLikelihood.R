Lg<-function(y,g){
  result<-c()
  for (i in 1:length(y)){
    x<-y[i]*g[i]
    xi<-log(pnorm(x,0,1))
    result<-c(result,xi)
  }
  output<-sum(result)
  return(output)
}

derivativeLg<-function(y,g){
  denom<-Lg(y,g)
  output<-c()
  for (i in 1:length(y)){
    num<-y[i]*dnorm(g[i])
    output[i]<-num/denom
  }
  return(output)
}

secondDerivativeLg<-function(y,g){
  LL<-Lg(y,g)
  derivativeLL<-derivativeLg(y,g)
  output<-diag(x=0,length(y),length(y))
  for (i in 1:length(y)){
    firstTerm<- ((dnorm(g[i]))^2)/LL^2
    secondTerm<-g[i]*derivativeLL[i]
    output[i,i]<-(-1*firstTerm)-secondTerm
  }
  return(output)
}

y1<-c(1,1,-1,1,-1)
g1<-c(2,3,4,5,6)
Lg(y1,g1)
derivativeLg(y1,g1)
secondDerivativeLg(y1,g1)
