LogLike2 <- function(data.mat,para,fixed_eff){
  
  B.mat <- matrix(fixed_eff[1:2],ncol=1)
  n1 <- length(para)/2
  b0 <- para[1:n1]
  b1 <- para[-c(1:n1)]
 
  Ey=c()
  for(i in 1:nrow(data.mat)) {
    Ey[i] <- (B.mat[1]+b0[data.mat$Patient[i]]) +(B.mat[2]+b1[data.mat$Patient[i]])*data.mat$X[i]
  }
  
  Log_lkY.Z <- sum(dnorm(data.mat$Y,Ey,exp(fixed_eff[3]),log=T))

  Log_lkb0 <- sum(dnorm(para[1:n1],0,exp(fixed_eff[4]),log=T))
  Log_lkb1 <- sum(dnorm(para[-c(1:n1)],0,exp(fixed_eff[5]),log=T))
  log_likelihood <- Log_lkY.Z + Log_lkb0 + Log_lkb1
  
  return(log_likelihood)
}
