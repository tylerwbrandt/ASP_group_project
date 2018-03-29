d2psi<- function(y,g,omega){  
    output<-(d2LogLike(y,g)+d2logpri(omega,g))
           return(output)
}

