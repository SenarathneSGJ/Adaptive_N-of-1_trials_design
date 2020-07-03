LogLikeFN <- function(data.mat,para,mu_pr,sigma_pr){
  
  B.mat <- matrix(para[1:2],ncol=1)

  Ey <- B.mat[1] +(B.mat[2]*data.mat$X)
  SdY <- exp(para[3])
  Sdb0 <- exp(para[4])
  Sdb1 <- exp(para[5])
  xd <- matrix(cbind(1,data.mat$X),ncol=2)
  
  if(nrow(data.mat)>1){
    covmat <- diag(rep(SdY^2,length(Ey))) + xd%*%diag(c(Sdb0,Sdb1)^2)%*%t(xd)
    log_likelihood <- dmvnorm(data.mat$Y,Ey,covmat,log=TRUE)
  }else{
    covmat <- SdY^2 + xd%*%diag(c(Sdb0,Sdb1)^2)%*%t(xd)
    log_likelihood <- dnorm(data.mat$Y,Ey,sqrt(covmat),log=TRUE)# #t(Ui-Ubar.mat)%*%Hu%*%(Ui-Ubar.mat)   #-0.5*log(det(Hu/(2*pi)))
    
  }

  return(log_likelihood)
}