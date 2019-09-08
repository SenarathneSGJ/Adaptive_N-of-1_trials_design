
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
source("Exp_crit.R")
source("trace_mat.R")
source("combine_ute.R")

N <- 20               #number of patients
Trt <- 0:1            #Treatments
N.Trt <- length(Trt)  #number of treatments
P.num <- 1:N          #patient number

#Parameters for design scenario 1
B0=25
B1= -1
Sigma.Y <- 3
Sigma.B0 <- 1.5
Sigma.B1 <- 1.5
set.seed(111)
b0 <- rnorm(N,0,Sigma.B0)
b1 <- rnorm(N,0,Sigma.B1)

True_para <- c(B0,B1,log(Sigma.Y),log(Sigma.B0),log(Sigma.B1),b0,b1)

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

mu_pr=colMeans(prior)  # prior means of the joint distribution
sigma_pr=cov(prior)    # prior variance-covariance matrix of the joint distribution

out=Seq_d(P.seq=2,Trt.seq=Trt,Q=N,J=3,mu_pr=mu_prior,sigma_pr=Sigma_prior)
save(out, file = "out.RData")


