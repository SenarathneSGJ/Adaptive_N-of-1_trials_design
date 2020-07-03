LogLikeF <- function(data.mat,para,para2,mu_pr,sigma_pr){
  
  B.mat <- matrix(para[1:2],ncol=1)
  
  post_dist2=Laplace_approx2(fixed_eff=as.numeric(para[1:5]),data.mat=data.mat,theta=para2,log_postf1=log_post2,mu_pr=mu_pr,sigma_pr=sigma_pr)
  U_bar=post_dist2$par
  Hu=optimHess(fixed_eff=as.numeric(para[1:5]),data.mat=data.mat,par=U_bar,fn=log_post2,mu_pr=mu_pr,sigma_pr=sigma_pr)
  #Hu=post_dist2$hessian
  
  post.SigU <- solve(Hu)
  CholU <- chol(post.SigU)
 
  n1 <- length(U_bar)/2
  b0 <- U_bar[1:n1]
  b1 <- U_bar[-c(1:n1)]
  
  Ey=c()
  for(i in 1:nrow(data.mat)) {
    Ey[i] <- (B.mat[1]+b0[data.mat$Patient[i]]) +(B.mat[2]+b1[data.mat$Patient[i]])*data.mat$X[i]
  }
  
  Log_lkY.Z <- sum(dnorm(data.mat$Y,Ey,exp(para[3]),log=T))
  Log_lkb0 <- sum(dnorm(U_bar[1:n1],0,exp(para[4]),log=T))
  Log_lkb1 <- sum(dnorm(U_bar[-c(1:n1)],0,exp(para[5]),log=T))

  log_likelihood <- Log_lkY.Z+Log_lkb0+Log_lkb1-0.5*log(det(Hu)) #t(Ui-Ubar.mat)%*%Hu%*%(Ui-Ubar.mat)   #-0.5*log(det(Hu/(2*pi)))
  
  return(log_likelihood)
}