exp.crit <- function(d,B,mu.p,sig.p,Patient,Cyc,Obs)
{
  crit=c()
  pr.samp <- rmvnorm(B,mu.p,sig.p)
  Main_effects <- pr.samp[,1:5]
  
  if(Obs==1 & Cyc==1){
    Lb0 <- rnorm(B,0,exp(Main_effects[,4]))
    Lb1 <- rnorm(B,0,exp(Main_effects[,5]))
    sig.b <- diag(exp(diag(sig.p)[4:5]))
    sig_pr <- superMatrix(sig.p[1:5,1:5],sig.b)
    mu_pr <- c(mu.p[1:5],0,0)

  }else{
    b.mat <- pr.samp[,-(1:5)]
    n1 <- ncol(b.mat)/2
    Lb0 <- b.mat[,Patient]
    Lb1 <- b.mat[,(n1+Patient)]
    mu_b <- mu.p[-(1:5)]
    mu_pr <- c(mu.p[1:5],mu_b[Patient],mu_b[n1+Patient])
    sig.b <- sig.p[-(1:5),-(1:5)]
    sig_pr <- superMatrix(sig.p[1:5,1:5],sig.b[c(Patient,n1+Patient),c(Patient,n1+Patient)])
  }
  
  for(k in 1:B)
  {
    Y<-ResponseY(X=d,theta=Main_effects[k,],b0=Lb0[k],b1=Lb1[k])
    data.mat <- data.frame(X=d,Y,Patient=1)
    crit.out <- combine_ute(data.mat=data.mat,theta=c(Main_effects[k,],Lb0[k],Lb1[k]),mu.p=mu_pr,sig.p=sig_pr)
	  crit[k] <- crit.out[[3]]
  }
  
  print(B)
  return(mean(crit,na.rm=T))
}
