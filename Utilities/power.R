mcol=c('black','darkred','darkblue','darkgreen','purple4')
mlty=5:1

I=c(1,2,3,4,5) #number of participants

ncp=seq(0,4,.01)

cut=qt(.95,I-1) #alpha level, qt = quantile function

pow=function(I,ncp) {
  cut=qt(.95,I-1)
  pt(lower.tail=F,cut,I-1,ncp)+pt(-cut,I-1,ncp) #pt = distribution function
}

b=outer(I,ncp,pow)

#pdf('power-v-ncp.pdf',width=5,height=5)
#par(mar=c(4,4,1,1),cex=1.2,mgp=c(2,1,0))
#matplot(ncp,t(b),typ='l',lty=mlty,col=mcol,ylim=c(0,1),ylab='Power',xlab="Noncentrality",lwd=2)
#legend(0,1,title="# of People",legend=I,lwd=2,col=mcol,lty=mlty)
#dev.off()

myncp=function(I,K,s2,nu,delta) {
  a=nu^2*I*K/(K*delta+2*s2)
  sqrt(a)
}





#s2 is intra-subject variability
#delta is inter-subject variability
#nu is effect size


myPower=function(I,K,s2,nu,delta)
{
  ncp=sqrt(nu^2*I*K/(K*delta+2*s2))
  cut=qt(.975,I-1)
  return(pt(lower.tail=F,cut,I-1,ncp)+pt(-cut,I-1,ncp))	
}

myPanel=function(s2,nu,delta)
{
  b=outer(I,K,myPower,s2=s2,nu=nu,delta=delta)
  matplot(K,t(b),typ='l',lty=mlty,lwd=2,col=mcol,ylim=c(0,1),ylab="Power",xlab="Trials Per Condition")
  K0=1000/I
  pow0=myPower(I,K0,s2,nu,delta)
  #lines(K0,pow0,lty=2)
  points(K0,pow0,col=mcol,pch=19,cex=1.3)
  return(pow0)
}


#aux <- expand.grid(Participants = I, Observations = K, Intra_Subject_Variability = c(0.1^2,0.3^2),Inter_Subject_Variability = c(0.01^2,0.05^2), Effect_Size = c(0.05,0.1))
#
#aux$Power <- myPower(aux$Participants,aux$Observations,aux$Intra_Subject_Variability,aux$Inter_Subject_Variability,aux$Effect_Size)
#
#ggplot(aux,aes(Observations,Power)) +
#  geom_line()

I=c(2,4,6,8,10) #participants
K=1:40 #observations per participant
s2 = 0.15 #s2 is intra-subject variability
delta = 0.15 #delta is inter-subject variability
nu = 0.15 #nu is effect size: different

pdf('tradeoffVerySmall.pdf',width=10,height=5)
par(mfrow=c(1,2),mar=c(4,4,1,1),cex=1.2,mgp=c(2,1,0))

myPanel(s2^2,nu,delta^2) #case one
mtext(side=3,adj=0,cex=1,"a") #: small effect (0.02); var(intra)/var(inter) = 6

myPanel(s2^2,nu*2,delta^2) #case two
mtext(side=3,adj=0,cex=1,"b") #: small effect (diff of 0.1); var(intra)/var(inter) = 6"

par(xpd=T)
legend(70,0.5,title="# of People",legend=I,lwd=2,col=mcol,lty=mlty,bg="white")
dev.off()


pdf('tradeoffSmall.pdf',width=10,height=5)
par(mfrow=c(1,2),mar=c(4,4,1,1),cex=1.2,mgp=c(2,1,0))

myPanel(s2=.2^2,nu=.015,delta=0.04^2) #case one
mtext(side=3,adj=0,cex=1,"A: Time (smallest diff.)")

myPanel(s2=.2^2,nu=.028,delta=0.04^2) #case one
mtext(side=3,adj=0,cex=1,"B: Time (middle")

myPanel(s2=.2^2,nu=.047,delta=0.04^2) #case one
mtext(side=3,adj=0,cex=1,"B: Time (middle")



myPanel(s2=.5^2,nu=0.1963,delta=0.1^2) #case two
mtext(side=3,adj=0,cex=1,"B: Space")

myPanel(s2=.5^2,nu=0.3884,delta=0.1^2) #case two
mtext(side=3,adj=0,cex=1,"B: Space")

myPanel(s2=.5^2,nu=0.6613,delta=0.1^2) #case two
mtext(side=3,adj=0,cex=1,"B: Space")



par(xpd=T)
legend(75,1.1,title="# of People",legend=I,lwd=2,col=mcol,lty=mlty,bg="white")
dev.off()

######
?mtext
###distributions of responses for values chosen above (assuming they're normally distributed)
plot(density(rnorm(100,mean = -0.1, sd = 0.1))) #variability within one subject
plot(density(rnorm(20,mean = -0.1, sd = 0.2))) #variability across subjects (comparing means per subject)




effect=seq(0,.15,.001)
shape=2
plot(effect,dgamma(effect,shape,scale=.010),typ='l')
lines(effect,dgamma(effect,shape,scale=.020))
lines(effect,dgamma(effect,shape,scale=.040))
?dgamma
sd=sqrt(2)*c(.01,.02,.04)



sd(rnorm(1000,1,0.1) - rnorm(1000,0.98,0.1))

