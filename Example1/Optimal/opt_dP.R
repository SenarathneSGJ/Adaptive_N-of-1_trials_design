opt_dP=function(mu.p,sig.p,Patient,Cyc,Obs){
  
  ute_X0 <-  exp.crit(d=0,B=500,mu.p,sig.p,Patient,Cyc,Obs)
  ute_X1 <- exp.crit(d=1,B=500,mu.p,sig.p,Patient,Cyc,Obs)
  
  d_prop <- 0
  d_prop[ute_X0 < ute_X1] <- 1
 
return(d_prop)
}