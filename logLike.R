logLike<-function(y,g){
  result<-c()
  for (i in 1:length(y)){
    x<-y[i]*g[i]
    xi<-log(pnorm(x,0,1))
    result<-c(result,xi)
  }
  output<-sum(result)
  return(output)
}

y1<-c(1,1,-1,1,-1)
g1<-c(2,3,4,5,6)
logLike(y1,g1)
