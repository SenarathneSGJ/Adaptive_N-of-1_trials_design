
Seq_d <- function(N.Trt,Trt.seq,N,N.cyc,mu_pr,sigma_pr,D.method){
 
  mu_post.all = mu_pr
  sigma_post.all = sigma_pr
  data_all=data.frame()

  for(j in 1:N.cyc){      
    for(p in 1:N){
      for(k in 1:N.Trt){
        samp_T <- opt_dP(mu.p=mu_post.all,sig.p=sigma_post.all,Patient=p,Cyc=j,Obs=k,D.method)    #optimal treatment 
        y.m<-ResponseY(X=samp_T,theta=True_para[1:5],b0=True_para[5+p],b1=True_para[5+N+p])
        
        data_p <- data.frame(X=samp_T,Y=y.m,Patient=p)
        data_all <- rbind(data_all,data_p)
        
        if(j==1){
          Int_theta=rep(1,(5+2*p))
        }else{
          Int_theta=rep(1,(5+2*N))
        }
        
        out.p <- Post_all(data.mat=data_all,para=Int_theta,mu_prior,Sigma_prior)
        mu_post.all=out.p[[1]]
        sigma_post.all=as.matrix(nearPD(out.p[[2]])$mat)
    	  #out.new <- list(data_all,mu_post.all,sigma_post.all) #Save intermidiate data and posterior distributions
    	  #loc <- paste("Iter",100,(j-1)*N+p,".RData",sep="")
      	#save(out.new,file=loc)
      }      
    }
  }
  out.list <- list(data_all,mu_post.all,sigma_post.all)
  return(out.list)
}