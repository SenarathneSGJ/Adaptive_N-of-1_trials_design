combine_ute <- function(data.mat,theta,mu.p,sig.p){
  
  post.out <- Post_all(data.mat=data.mat,para=theta,mu_pr=mu.p[1:5],sigma_pr=sig.p[1:5,1:5])
  mu_post=post.out[[1]]
  Sigma_post=post.out[[2]]
  
  iSigma_prior <- solve(sig.p)
  log_det_prior <- log(det(sig.p))
  log_det_post <- log(det(Sigma_post))
  num_par <-  length(mu_post)
  
  kld.out <- 0.5*(trace_mat(iSigma_prior%*%Sigma_post) + t(mu.p-mu_post)%*%iSigma_prior%*%(mu.p-mu_post)-num_par + log_det_prior-log_det_post)
  
  return(list(mu_post,Sigma_post,kld.out,log_det_post)) 
}