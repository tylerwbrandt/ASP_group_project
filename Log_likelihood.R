logLikelihood<-function(g, y){
  sum<-0
  for(i in 1:length(g)){
    sum=sum+log(pnorm(g*y))
  }
  return(sum)
}
