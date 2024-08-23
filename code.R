#read in first 1000 data points
my_data <- read.delim(file.choose())
n<-my_data[1:1000,1]
x<-my_data[1:1000,2]

#inititalization
wtau=0.01
wtheta=0.002
wti=0.002
tau=0.01
theta=0.001
cotimes=matrix(data=0.001,nrow=1000)

acc_tau=0
acc_theta=0
acc_ti=0

#priors
tau_mean=0.005
theta_mean=0.001

lnp<-c(0,tau,theta,cotimes)
#log prior function
lnpf<- function(tau, theta, cotimes)
{
  sumpt2<-0
  for (i in range(1:length(cotimes)))
    {
    Pi<- .75-(.75*(exp(((-8/3)*(tau+cotimes[i])))))
    add<-log(2/theta)-((2/theta)*cotimes[i])+(x[i]*log(Pi))+((n[i]-x[i])*log(1-Pi))
    sumpt2=sumpt2+add
    }
  final=-(1/tau_mean*tau)-(1/theta_mean*theta)+sumpt2
  newlnp<-c(final, tau, theta, cotimes)
  return(newlnp)
}

#new values function
change_tau<- function(oldlnp, windowsize)
{
  ta=oldlnp[2]
  u=runif(1, min = 0, max = 1)
  taunew <- ta + ((u-0.5)*windowsize)
  if (taunew <= 0)
    taunew = -(taunew)
  print(taunew)
  newlnp=lnpf(taunew,oldlnp[3],oldlnp[4:1000])
  ratio= newlnp[1] - oldlnp[1]
  v=runif(1, min = 0, max = 1)
  if (ratio < v)
    {tau<<-taunew
    acc_tau<<-acc_tau+1
    lnp[2]=taunew}
  return(lnp)  
}

change_theta<- function(oldlnp, windowsize)
{
  th=oldlnp[3]
  u=runif(1, min = 0, max = 1)
  thetanew <- th + ((u-0.5)*windowsize)
  if (thetanew <= 0)
    thetanew = -(thetanew)
  print(thetanew)
  newlnp=lnpf(oldlnp[2],thetanew,oldlnp[4:1000])
  ratio= newlnp[1] - oldlnp[1]
  v=runif(1, min = 0, max = 1)
  if (ratio < v)
  {theta<<-thetanew
  acc_theta<<-acc_theta+1
  lnp[3]=thetanew}
  return(lnp)  
}

change_coaltimes<- function(oldlnp,windowsize){
  for (j in 1:1000)
    {
    jold=oldlnp[j+3]
    u=runif(1, min = 0, max = 1)
    jnew <- jold + ((u-0.5)*windowsize)
    Pj<- .75-(.75*(exp(((-8/3)*(tau+jold)))))
    Pjnew<- .75-(.75*(exp(((-8/3)*(tau+jnew)))))
    lnpdiff=-2/oldlnp[3]*(jnew - jold) + (x[j]*log(Pjnew/Pj))+ ((n[j]-x[j])*log((1-Pjnew)/(1-Pj)))
    v=runif(1, min = 0, max = 1)
    if (lnpdiff < v) 
      {cotimes[j]<<-cotimes[j]+lnpdiff
      lnp[j+3]= lnp[j+3]+lnpdiff
      acc_ti<<-acc_ti+1
      }
    }
    return(lnp)
}


lnp=lnpf(tau,theta,cotimes)
lnp=change_tau(lnp,wtau)
lnp=change_theta(lnp,wtheta)
lnp=change_coaltimes(lnp,wti)


for (i in 1:1000){
  
  
  
}