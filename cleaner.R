cleaner<- function(dataset){

  newset1<-dataset[c(seq(1,nrow(dataset),2)),]
  newset2<-dataset[c(seq(2,nrow(dataset),2)),]
  colnames(newset1)<-c("comparison_id","first","y")
  colnames(newset2)<-c("comparison_id","second","y") 
  newset3<- merge(newset1,newset2[, c(1,2)], by = "comparison_id")
  newset3$y<- ifelse(newset3$y==0,-1,1)
  return(newset3)
  
}


