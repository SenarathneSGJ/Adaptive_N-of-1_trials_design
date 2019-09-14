log_post2 <- function(data.mat,theta,fixed_eff,mu_pr,sigma_pr)
  {
    #log.prior<- dnorm(fixed_eff,mu_prior,sd_prior,log=TRUE)
    log.like <- sum(LogLike2(data.mat=data.mat,para=theta,fixed_eff=fixed_eff))
 
	log.like[is.na(log.like) | log.like== Inf | log.like == -Inf] <- -3e+2
    Neg_log_post=-1*(log.like)
    #print(Neg_log_post)
    return(Neg_log_post)
  }