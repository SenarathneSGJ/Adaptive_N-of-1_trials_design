opt_dP=function(mu.p,sig.p,Patient,Cyc,Obs,D.method){
  
  if(D.method==1){
    ute_X0 <-  exp.crit(d=0,B=500,mu.p,sig.p,Patient,Cyc,Obs)
    ute_X1 <- exp.crit(d=1,B=500,mu.p,sig.p,Patient,Cyc,Obs)
    
    d_prop <- 0
    d_prop[ute_X0 < ute_X1] <- 1
  }else if(D.method==2){
    d_prop <- MAB_dP(mu.p,sig.p,Patient,Cyc,Obs)
  }else{
    d_prop <- design_mat[Patient,((Cyc-1)*N.Trt+Obs)]   #random design is sellected from the generated treatment sequence 
  }
 
  return(d_prop)
}