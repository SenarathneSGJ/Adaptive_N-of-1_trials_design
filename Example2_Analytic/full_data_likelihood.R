
library(lme4)
set.seed(1)
N <- 50
n <- 6
y <- rep(0,N*n)
x <- y
id <- y
i <- 0
treat <- c(0,1,0,1,0,1)
for (j in 1:N){
  mui <- rnorm(1,2,0.5)
  alphai <- rnorm(1,1,0.5)
  for(k in 1:n){
    i <- i+1
    e <- rnorm(1,0,1)
    y[i] <- mui + alphai*treat[k] + e
    x[i] <- treat[k]
    id[i] <- j
  }
}

logpost <- function(theta,y,x,mu.prior,sd.prior){
  lp <- theta[1] + theta[2]*x
  kn1 <- exp(theta[3])
  kn2 <- exp(theta[4])
  nug <- exp(theta[5])
  xd <- matrix(cbind(1,x),ncol=2)
  covmat <- diag(rep(nug,length(lp))) + xd%*%diag(c(kn1,kn2))%*%t(xd)
  covmat <- round(covmat,4)
  if(is.positive.definite(covmat)){
    logprior <- sum(dnorm(theta,mean=mu.prior,sd=sd.prior,log=TRUE))
    loglike <- dmvnorm(y,lp,covmat,log=TRUE)#-length(lp)/2*log(2*pi) - 0.5*log(det(covmat)) - 0.5*t(y-lp)%*%solve(covmat)%*%(y-lp)
    out <- logprior + loglike}else{out<- 100}
  if(is.finite(out)){out <- out}else{out <- 100}
  -out
}

opt.logpost <- optim(par = c(2,1,0,0,0),fn=logpost,lower=c(-20,-20,-4,-4,-4),upper=c(20,20,4,4,4),y=y,x=x,mu.prior=c(0,0,0,0,0),sd.prior=c(20,20,2,2,2),method="L-BFGS-B",hessian=TRUE)
#opt.logpost <- nmkb(par = c(2,1,-0.57,-0.69,-0.12),fn=logpost,lower=c(-20,-20,-4,-4,-4),upper=c(20,20,4,4,4),y=y,x=x,mu.prior=c(0,0,0,0,0),sd.prior=c(20,20,0.2,0.2,0.2))
opt.logpost$par
sqrt(exp(opt.logpost$par[3:5]))

lmer(y~x + (x|id))
