d2psi<- function(omega,g, y){  
    output<-(d2LogLike(y,g)+d2logpri(omega,g))
           return(output)
}

