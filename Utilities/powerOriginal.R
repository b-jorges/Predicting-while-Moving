
mcol=c('black','darkred','darkblue','darkgreen','purple4')
mlty=5:1
I=c(5,10,20,50,100)
ncp=seq(0,4,.01)
cut=qt(.95,I-1)
pow=function(I,ncp) {
  cut=qt(.975,I-1)
  pt(lower.tail=F,cut,I-1,ncp)+pt(-cut,I-1,ncp)
}

b=outer(I,ncp,pow)

pdf('power-v-ncp.pdf',width=5,height=5)
par(mar=c(4,4,1,1),cex=1.2,mgp=c(2,1,0))
matplot(ncp,t(b),typ='l',lty=mlty,col=mcol,ylim=c(0,1),ylab='Power',xlab="Noncentrality",lwd=2)
legend(0.5,1,title="# of People",legend=I,lwd=2,col=mcol,lty=mlty)
dev.off()


myncp=function(I,K,s2,nu,delta) {
  a=nu^2*I*K/(K*delta+2*s2)
  sqrt(a)
}



I=c(5,10,20,50,100)
K=5:100




myPower=function(I,K,s2,nu,delta)
{
  ncp=sqrt(nu^2*I*K/(K*delta+2*s2))
  cut=qt(.975,I-1)
  return(pt(lower.tail=F,cut,I-1,ncp)+pt(-cut,I-1,ncp))	
}

myPanel=function(s2,nu,delta)
{
  b=outer(I,K,myPower,s2=s2,nu=nu,delta=delta)
  matplot(K,t(b),typ='l',lty=mlty,lwd=2,col=mcol,ylim=c(0,1),ylab="Power",xlab="Trials Per Condition, K")
  K0=1000/I
  pow0=myPower(I,K0,s2,nu,delta)
  #lines(K0,pow0,lty=2)
  points(K0,pow0,col=mcol,pch=19,cex=1.3)
  return(pow0)
}

pdf('tradeoff.pdf',width=10,height=5)
par(mfrow=c(1,2),mar=c(4,4,1,1),cex=1.2,mgp=c(2,1,0))
myPanel(s2=.3^2,nu=.040,delta=.028^2)
mtext(side=3,adj=0,cex=1.3,"A")
myPanel(s2=.1^2,nu=.040,delta=.12^2)
mtext(side=3,adj=0,cex=1.3,"B")
par(xpd=T)
legend(75,0.7,title="# of People",legend=I,lwd=2,col=mcol,lty=mlty,bg="white")
dev.off()
######

effect=seq(0,.15,.001)
shape=2
plot(effect,dgamma(effect,shape,scale=.010),typ='l')
lines(effect,dgamma(effect,shape,scale=.020))
lines(effect,dgamma(effect,shape,scale=.040))

sd=sqrt(2)*c(.01,.02,.04)