ResponseY<- function(X,theta,b0,b1){
  
  Ey <- (theta[1]+b0)+(theta[2]+b1)*X
  Y <- rnorm(length(Ey),Ey,exp(theta[3]))
  return(Y)
}
