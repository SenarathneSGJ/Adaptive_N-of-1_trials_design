ResponseY<- function(X,theta,Patient,b0,b1){
  
  Ey <- (theta[1]+b0)+(theta[2]+b1)*X
  Ey2=1/(1+exp(-Ey))
  p=Ey2*exp(theta[3])
  q=(1-Ey2)*exp(theta[3])
  
  Y <- rbeta(length(Ey),p,q)
  return(Y)
}
