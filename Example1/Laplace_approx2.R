Laplace_approx2<- function(data.mat,theta,log_postf1,fixed_eff,mu_pr,sigma_pr)
  {
  #lp.approx <- optim(par=theta,X=X,Z=Z, Y=Y,fiexd_eff=fiexd_eff,fn=log_postf1, hessian=TRUE,method="L-BFGS-B",lower=c(apply(prior[,6:45],2,min)),upper=c(apply(prior[,6:45],2,max)))
  lp.approx <-nlminb(start=theta,data.mat=data.mat,fixed_eff=fixed_eff,mu_pr=mu_pr,sigma_pr=sigma_pr, objective =log_postf1,lower=c(apply(prior[,-c(1:5)],2,min)),upper=c(apply(prior[,-c(1:5)],2,max))) #control = list(eval.max=10000,iter.max=10000,sing.tol=1e-18,rel.tol=1e-14),
  
  lp.approx
}