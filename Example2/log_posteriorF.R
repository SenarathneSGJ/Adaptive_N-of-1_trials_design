log_postF <- function(data.mat,theta,para2,mu_pr,sigma_pr)
  {
    log.prior<- dmvnorm(x=theta[1:5],mean=mu_pr,sigma=sigma_pr,log=TRUE)
    log.like <- sum(LogLikeF(data.mat=data.mat,para=theta,para2=para2,mu_pr=mu_pr,sigma_pr=sigma_pr))
	
    log.like[is.na(log.like) | log.like== Inf | log.like == -Inf] <- -3e+2
    Neg_log_post=-1*(log.prior + log.like)
    
    return(Neg_log_post)
  }
