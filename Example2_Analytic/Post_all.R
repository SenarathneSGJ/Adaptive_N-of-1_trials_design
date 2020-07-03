Post_all <- function(data.mat,para,mu_pr,sigma_pr){
  post_dist=Laplace_approx(data.mat=data.mat,theta=as.numeric(para[1:5]),log_postf1=log_postF,mu_pr=mu_pr,sigma_pr=sigma_pr)
  mu_post1=post_dist$par
  hessian1=optimHess(data.mat=data.mat,par=mu_post1,fn=log_postF,mu_pr=mu_pr,sigma_pr=sigma_pr)
  sigma_post1=solve(hessian1)

  post_dist2=Laplace_approx2(fixed_eff=mu_post1,mu_pr=mu_pr,sigma_pr=sigma_pr,data.mat=data.mat,theta=as.numeric(para[-c(1:5)]),log_postf1=log_post2)
  U_bar=post_dist2$par
  hessianU=optimHess(fixed_eff=mu_post1,data.mat=data.mat,mu_pr=mu_pr,sigma_pr=sigma_pr,par=U_bar,fn=log_post2)
  post_sigmaU=solve(hessianU)
  #lp_post2=rmvnorm(n=10000,mean=U_bar, sigma = post_sigmaU)
  
  post_mean <- c(mu_post1,U_bar)
  post_sig <- superMatrix(sigma_post1,post_sigmaU)
  
  out=list(post_mean,post_sig)
  return(out)
}