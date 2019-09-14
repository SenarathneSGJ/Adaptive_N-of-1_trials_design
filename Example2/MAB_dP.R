MAB_dP=function(mu.p,sig.p,Patient,Cyc,Obs){
  
  n1=5000
  
  if(Cyc==1 & Obs==1){
    mn_para <- mu.p[1:5]
    sig_para <- sig.p[1:5,1:5]
    post_samp <- rmvnorm(n1,mn_para,sig_para)
    Lb0 <- rnorm(n1,0,exp(post_samp[,4]))
    Lb1 <- rnorm(n1,0,exp(post_samp[,5]))
  }else{
   post_samp_all <- rmvnorm(n1,mu.p,sig.p)
   post_samp <- post_samp_all[,1:5]
   n2 <-  (ncol(post_samp_all)-5)/2
   Lb_all <- post_samp_all[,-(1:5)]
   Lb0 <- Lb_all[,Patient]
   Lb1 <- Lb_all[,(n2+Patient)]
  }

  YT1 <- c()
  YT2 <- c()
  for(j in 1:nrow(post_samp)){
    YT1[j] <- ResponseY(X=0,theta=post_samp[j,],b0=Lb0[j],b1=Lb1[j])
    YT2[j] <- ResponseY(X=1,theta=post_samp[j,],b0=Lb0[j],b1=Lb1[j])
  }
  Pr.mat <-  sum(YT1>YT2)/5000
  samp_trt <- rbinom(1,1,Pr.mat)
return(samp_trt)
}