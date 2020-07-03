log_postF <- function(data.mat,theta,mu_pr,sigma_pr)
  {
    log.prior<- dmvnorm(x=theta,mean=mu_pr,sigma=sigma_pr,log=TRUE)
    log.like <- sum(LogLikeFN(data.mat=data.mat,para=theta,mu_pr=mu_pr,sigma_pr=sigma_pr))
	
	  log.like[is.na(log.like) | log.like== Inf | log.like == -Inf] <- -3e+2
    Neg_log_post=-1*(log.prior + log.like)
    #print(Neg_log_post)
    return(Neg_log_post)
  }