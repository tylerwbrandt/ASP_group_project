d2logpri<-function(omega,g){
  if (det(omega) == 0){
  diag(omega)<-diag(omega)+.01}
  inv_omega<-(solve(omega))
  neg_inv_omega<- -1*(inv_omega)
  return(neg_inv_omega)
}



