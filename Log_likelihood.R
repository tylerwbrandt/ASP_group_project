logLikelihood<-function(g){
  sum<-0
  for(i in 1:length(g)){
    sum<-sum+log(ecdf(g))
  }
  return(sum)
}