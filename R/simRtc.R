#' Simulates minutes of physical activity in continuos time
#' 
#' Simulates minutes of physical activity in continuous time
#' 
#' 
#' @param n Number of individuals in the simulated data.
#' @param beta True coefficient for the binary covariate.
#' @param nu Shape and rate parameter for Gamma distribution, in which the
#' subject specific random variable Z_i was generated.
#' @param mu Baseline mean minutes of physical activity per bout.
#' @param inf Whether to generate data with informative observation and
#' censoring times.
#' @param nobs Average number of physical activity bouts.
#' @return A simulated dataset is returned with four columns: [ID, time, min,
#' x1, phi].  The number of physical activity minutes 'min' is generated as
#' mu*Z_i*exp(X_i*beta), where the expected number of bouts is 'nobs'.
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @keywords accelerometer
#' @examples
#' 
#' ##
#' ## Simulauting data for a 50 individuals 
#' ## with noninformative observation and censoring time patterns
#' ##
#' simdata <- simRtc(n=50,beta=-.4,nu=5,mu=12,inf=FALSE,nobs=7)
#' 
#' ##
#' ## Simulauting data for a 50 individuals 
#' ## with informative observation and censoring time patterns
#' ##
#' simdata <- simRtc(n=50,beta=-.4,nu=5,mu=12,inf=TRUE,nobs=c(7,2))
#' 
#' @export
#' @importFrom utils head tail 
#' @importFrom stats rbinom rgamma rpois na.omit aggregate
simRtc <- function(n,beta,nu,mu,inf,nobs){
  
  op = options()
  on.exit({
    options(op)
  })
  options(warn=-1)
  
  x1=rbinom(n,1,0.5); 
  x=cbind(x1) 
  beta1=beta 
  parms=c(beta1)
  phi=rgamma(n,shape=nu,rate=nu)
  
  # Generating the number of observations for each subject
  # Non-informative case
  if(inf==FALSE){
    k=rpois(n,(nobs-1))+1; 
  }
  
  # Generating the number of observations for each subject
  # Informative case
  if(inf==TRUE){
    # nobs[1] events for group 1
    # nobs[2] event for group 2
    k1=rpois(n,(nobs[1]-1))+1; 
    k2=rpois(n,(nobs[2]-1))+1;
    k=k1*(ifelse(x==1 & phi<=1,1,0))+  k2*(ifelse(x==1 & phi<=1,0,1))
  }
  
  K=max(k);
  # generating the random time gaps between events for each subject
  y = matrix(data = NA, nrow = n, ncol=K);
  for (i in 1:n){
    y[i,1:k[i]]=stats::rexp(k[i],8/(60*24*7))
  } 
  
  # generating observation time points for each subject
  t=matrix(data = NA, nrow = n, ncol=K);
  for (i in 1:n){
    for (j in 2:K){
      t[i,1] = y[i,1]
      t[i,j] = y[i,j]+t[i,j-1]
    }
  }
  
  # generating the number of events between time intervals
  z=matrix(data = NA, nrow = n, ncol=K);
  xparms=c();for (s in 1:nrow(x)){xparms[s]<-sum(x[s,]*parms)}
  for (i in 1:n){
    z[i,1]<-rpois(1,mu*exp(xparms[i])*phi[i])
    if (k[i]>1){
      z[i,2:k[i]]<-rpois(k[i]-1,mu-mu*exp(xparms[i])*phi[i])
    }
  }
  
  TestD<-list(t=t, x=x, z=z, k=k, K=K)
  simdata <- NULL
  
  for(m in 1:n){
    mydata <-      data.frame(ID=m,
                              time=as.numeric(round(as.numeric(na.omit(TestD$t[m,])),0)+1),
                              x1=TestD$x[m,1],
                              min=as.numeric(na.omit(TestD$z[m,])))
    
    mydata <-      data.frame(ID=m,aggregate(min~time,data=mydata,FUN=sum),
                              x1=TestD$x[m,1]
    )
    newdata <- mydata[ which(mydata$min!=0), ]
    
    if(nrow(newdata)==0){
      savedata <-    data.frame(ID=m,
                                time=as.numeric(round(as.numeric(na.omit(TestD$t[m,]))[TestD$k[m]],0)+1),
                                x1=TestD$x[m,1],
                                #x2=TestD$x[m,2],
                                min=0)
    }
    if(nrow(newdata)>0){
      savedata <-    newdata
    }
    simdata <- rbind(simdata,savedata)
  }
  myphi = data.frame(ID=1:n,phi=phi)
  simdata <- merge(simdata, myphi, by ="ID")
  simdata
}
