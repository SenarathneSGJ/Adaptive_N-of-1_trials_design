
library(mvtnorm)
library(nlme)
library(psych)
library(Matrix)

source("Laplace_approx.R")
source("Laplace_approx2.R")
source("log_posteriorF.R")
source("log_posterior2.R")
source("Log_likeF.R")
source("Log_like2.R")
source("ResponseY.R")
source("Seq_design.R")
source("Post_all.R")
source("opt_dP.R")
source("MAB_dP.R")
source("Exp_crit.R")
source("trace_mat.R")
source("combine_ute.R")

load("True_para1.RData")
True_para <- out[[1]] #Parameters

N <- length(True_para[-(1:5)])/2  #Number of patients
Trt <- 0:1            #Treatments
N.Trt <- length(Trt)  #Number of treatments
N.cyc=3               #Number of treatment cycles
P.num <- 1:N          #Patient number

mu_prior <- c(0,0,2.5,2.5,2.5)      #prior means
sd_prior <- c(100,100,1.6,1.6,1.6)  #prior standard deviation
Sigma_prior <- diag(sd_prior^2)     #prior variance-covariance matrix

#A sample from the prior
M=10000
Pr.B0 <- rnorm(M,mu_prior[1],sd_prior[1])
Pr.B1 <- rnorm(M,mu_prior[2],sd_prior[2])
Log.sigY <- rnorm(M,mu_prior[3],sd_prior[3])
Log.sigb0 <- rnorm(M,mu_prior[4],sd_prior[4])
Log.sigb1 <- rnorm(M,mu_prior[5],sd_prior[5])

prior1=data.frame(Pr.B0,Pr.B1,Log.sigY,Log.sigb0,Log.sigb1)
prior=prior1

Nb=500
Lb0 <- rnorm(Nb,mu_prior[4],sd_prior[4])
Lb1<- rnorm(Nb,mu_prior[5],sd_prior[5])

for(i in 1:(2*N)){
  if(i<=N){
    zi <-paste("b0",i,sep="")
    prior<-cbind(prior,rnorm(M,0,exp(Lb0)))
    colnames(prior)[i+5]=zi
  }else{
    zi <-paste("b1",i-N,sep="")
    prior<-cbind(prior,rnorm(M,0,exp(Lb1)))
    colnames(prior)[i+5]=zi
  }
  
}

mu_pr=colMeans(prior)  #prior means of the joint distribution
sigma_pr=cov(prior)    #prior variance-covariance matrix of the joint distribution

D.method=1 #D.method: 1=optimal,2=MAB and 3=random 

if(D.method!=3){
  out=Seq_d(N.Trt=N.Trt,Trt.seq=Trt,N=N,N.cyc=N.cyc,mu_pr=mu_prior,sigma_pr=Sigma_prior,D.method=D.method)
}else{
  design_mat <- matrix(ncol=(N.cyc*N.Trt),nrow=N)
  for(i in 1:N){
    samp_i <- c()
    for(j in 1:N.cyc){
      samp_j <- sample(0:1)
      samp_i <- c(samp_i,samp_j)
    }
    design_mat[i,] <- samp_i
  }
  out=Seq_d(N.Trt=N.Trt,Trt.seq=Trt,N=N,N.cyc=N.cyc,mu_pr=mu_prior,sigma_pr=Sigma_prior,D.method=D.method)
}

save(out, file = v_name)
