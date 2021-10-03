getXYpos <- function(t1,vh,vv,g=9.82){
	 #g <- 9.82
	 
	 #l<-length(t)
	# p <- cbind(t,rep(vh,l),rep(vv,l)) 
#	 p <- cbind(t1,vh,vv)

#	 x <- p[,1]*p[,2]
#	 y <- -((g*p[,1]^2)/2)+p[,3]*p[,1]

	x <- t1*vh
	y <- -((g*t1^2)/2)+vv*t1

	 res <- list(x=x,y=y)
	 res
}


getParabolicCurvature <- function(g,vh){
  g/(vh^2)
}

# define a min-max normalize() function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

DegreeToRadians <- function(degree){
  radians <- degree*0.01745
  radians
}

RadiansToDegree <- function(radians){
  degree <- radians/0.01745
  degree
}
 
ElevAngleAwayFromObserver <- function(t,velV,velH,g){

#  (1) ElevAngle = tan(y/x) ##ok
#  (2) y(t) = -(g*t^2)/2)+velV*t ##ok
#  (3) x(t) = t*velH ##ok

#  (4, (1) with (2) and (3)) ElevAngle = tan(((-(g*t^2)/2)+velV*t)/(t*velH)) ##ok

  y <- -(g*t^2)/2+velV*t
  x <- t*velH
  ElevAngle <- atan(y/x)
  ElevAngle}

ElevPrimeAway <- function(t,velV,velH,g){
  (ElevAngleAwayFromObserver(t,velV,velH,g) - ElevAngleAwayFromObserver(t-0.01,velV,velH,g))/0.01
}

#temps quan el target es en posicio x
timeAtx <- function (x,vh)
{
    l<-length(x)
  #  p <- cbind(x,rep(vh,l))
    p <- cbind(x,vh)

    p[,1] / p[,2]
}

gethdist <- function(vh,vv,g=9.82){
  (2*vh*vv)/g
}

getYAtPeak <- function(vv,g){
  (vv^2/g)/2
}

getVV <- function(t,vv,g=9.82)
{
#vv(t) = vi * sin(A)+gt
#vv(t) = vv(0) + gt

#	g <- 9.82 # m*s2
	(vv-(g*t))
}

getVr <- function(t,vh,vv,g=9.82)
{
#	g <- 9.82
	((2*vh^2 + (vv+g*t)*g*t)/(sqrt(4*vh^2 + (vv+g*t)^2)))
}

getElevation <- function(t,vh,g=9.82)
{

	#g <- 9.82 # m*s2

(atan((g*t)/(2*vh)))
}

getElevation2 <- function(t,vv,vh,g=9.82)
{
  
  #g <- 9.82 # m*s2
  
  (atan((vv-g*t)/(2*vh)))+1.57
}

getElevationPrime <- function(t,vh,g=9.82){
  (((atan((vv-g*t)/(2*vh)))+1.57)-((atan((vv-g*(t+0.001))/(2*vh)))+1.57))/0.001
                        
}


getElevationPrime <- function(t,vh,g=9.82)
{
	#g <- 9.82 # m*s2
	
	(g/(2*vh))*cos(getElevation(t,vh))^2
}



getTheta <- function(t,size,vh,vv,ttc,factor=1.0)
{
  #size = Radius
  XYt0 =getXYpos(ttc,vh,vv)
  xyt <- getXYpos(t,vh,vv)
  Xt = XYt0$x - xyt$x #remaining X distance at display 1
  dtot <- sqrt(Xt*Xt + xyt$y*xyt$y)  # remaining distance
  thetat = atan(size/dtot)*factor
  (thetat*2) # because size is Radius
}

ttcElevation <- function(t,size,vh,vv,ttc,factor=1,g=9.82)
{
  #size = Radius
#	g <- 9.82 # m*s2
	ea <- getElevation(t,vh)
	eaprime <- getElevationPrime(t,vh)
  theta <- getTheta(t,size,vh,vv,ttc,factor=factor)
  
	(2/g) * (2*size*eaprime) /(theta*cos(ea)) # 27/07/2012 2*size becuae size is radius

}

getTimetoYIsZero <- function(vv,g=9.82){
  (2*(vv/g))
}

getVisualAngle <- function(t,size,vh,vv,g=9.82){
  x <- getTimetoYIsZero(vv,g)*vh - t*vh
  y <- -((g*t^2)/2)+vv*t
  z <- (x^2+y^2)^0.5
  2*atan((size/2)/z)
}

getVisualAngleYIndependent <- function(t,size,vh,vv,g=9.82,y){
  x <- (getTimetoYIsZero(vv,g)*vh - t*vh)    #x-distance
  f <- (-(y-0.565)) #position of eyes
  y <- (f-((g*t^2)/2)+vv*t) #y-distance
  z <- ((x^2+y^2)^0.5) #distance to object
  2*atan((size/2)/z) #Visual Angle (two times angle from midpoint to edges)
}

getVisualAnglePrimeYIndependentDer <- function(t,s,h,v,g,y){
  f <- -(y-0.565)
 -(s*(2*(v-g*t)*(-(g*t^2)/2+v*t+f)-2*h*((2*h*v)/g-h*t)))/(2*((-(g*t^2)/2+v*t+f)^2+((2*h*v)/g-h*t)^2)^(3/2)*(s^2/(4*((-(g*t^2)/2+v*t+f)^2+((2*h*v)/g-h*t)^2))+1))
}

getVisualAnglePrimeYIndependent <- function(t,size,vh,vv,g,y){
  x <- (getVisualAngleYIndependent(t,size,vh,vv,g,y) - getVisualAngleYIndependent(t-0.001,size,vh,vv,g,y))/0.001
  x
}



ElevationAngleYIndependent <- function(t,g,v,h,y){
  #f <- -(y-0.565) ##height of eyes, 0.565 because 26.5cm from floor to screen, and 20cm to start and end plane of ball, and then eye height is body height minus 10cm
  #Height <- f-((g*t^2)/2)+v*t
  #Distance <- (gethdist(h,v,g)-t*h)
  #x <- atan(Height/Distance)
  x <- atan((-y+0.565-((g*t^2)/2)+v*t)/((2*h*v)/g-t*h)) ##this is the above four lines in one formula
  y <- atan((-y+0.565-((g*0^2)/2)+v*0)/((2*h*v)/g-0*h))
  -y+x
}

ElevationAnglePrimeYIndependent <- function(t,g,v,h,y){
  (ElevationAngleYIndependent(t,g,v,h,y) - ElevationAngleYIndependent(t-0.001,g,v,h,y))/0.001
}


ElevationAngleYIndependentVertical <- function(t,g,v,h,y){
  #f <- -(y-0.565) ##height of eyes, 0.565 because 26.5cm from floor to screen, and 20cm to start and end plane of ball, and then eye height is body height minus 10cm
  #Height <- f-((g*t^2)/2)+v*t
  #Distance <- (gethdist(h,v,g)-t*h)
  #x <- atan(Height/Distance)
  x <- atan((-y+0.565-((g*t^2)/2)+v*t)/((2*h*v)/g-t*h)) ##this is the above four lines in one formula
  x <- DegreeToRadians(90+RadiansToDegree(x))
}

ElevationAngleYIndependentHoriz <- function(t,g,v,h,y){
  #f <- -(y-0.565) ##height of eyes, 0.6 because 26.5cm from floor to screen, and 20cm to start and end plane of ball, and then eye height is body height minus 10cm
  #Height <- f-((g*t^2)/2)+v*t
  #Distance <- (gethdist(h,v,g)-t*h)
  #x <- atan(Height/Distance)
  x <- atan((-y+0.565-((g*t^2)/2)+v*t)/((2*h*v)/g-t*h)) ##this is the above four lines in one formula
  x}

ElevationAnglePrimeYIndependentHoriz <- function(t,g,v,h,y){
  (ElevationAngleYIndependentHoriz(t,g,v,h,y) - ElevationAngleYIndependentHoriz(t-0.001,g,v,h,y))/0.001}


ElevationAnglePrimeYIndependentInitRef <- function(t,g,v,h,y){
  (ElevationAngleYIndependentInitRef(t,g,v,h,y) - ElevationAngleYIndependentInitRef(t-0.001,g,v,h,y))/0.001}


#ElevationAnglePrimeYIndependentHoriz <- function(t,g,v,h,y){
#  x <- ((v-g*t)/(2*h*v/g-h*t)+h*(-g*t^2/2+v*t-y+113/200)/(2*h*v/g-h*t)^2)/((-g*t^2/2+v*t-y+113/200)^2/(2*h*v/g-h*t)^2+1) #just first derivative of Elevation Angle
#  x}


getVisualAnglePrime <- function(t,size,vh,vv,gval=9.82){
  -size*(2*(vv-gval*t)*(vv*t-gval*t^2/2)-2*vh*(2*vh*vv/gval-vh*t))/(2*((vv*t-gval*t^2/2)^2+(2*vh*vv/gval-vh*t)^2)^(3/2)*(size^2/(4*((vv*t-gval*t^2/2)^2+(2*vh*vv/gval-vh*t)^2))+1))
}




#Prior, Likelihood and matched Posterior
getPosteriorFromLikelihoodAndPrior <- function(meanLH=0,sdLH=1,meanP=2,sdP=0.75){
  xLH <- seq(meanLH-6,meanLH+6,length=10000)
  yLH <- dnorm(xLH,meanLH,sdLH)
  xP <- seq(meanP-6,meanP+6,length=10000)
  yP <- dnorm(xP,meanP,sdP)
  
  meanPP <- (sdLH^2*meanP+sdP^2*meanLH)/(sdP^2+sdLH^2)
  sdPP <- sqrt(1/((1/sdLH^2)+1/(sdP^2)))
  
  xPP <- seq((meanPP-6),(meanPP+6),length=10000)
  yPP <- dnorm(xPP,meanPP,sdPP)
  
  x <- xLH
  y <- yLH
  aux1 <- cbind(x,y)
  aux1 <- data.frame(aux1)
  aux1$id <- "LH"
  
  x <- xP
  y <- yP
  aux2 <- cbind(x,y)
  aux2 <- data.frame(aux2)
  aux2$id <- "P"
  
  x <- xPP
  y <- yPP
  aux3 <- cbind(x,y)
  aux3 <- data.frame(aux3)
  aux3$id <- "PP"
  
  aux <- rbind(aux1,aux2,aux3)
  
  aux
}

drawBayGraphs <- function(meanLH=0,sdLH=1,meanP=2,sdP=0.75, title=""){
  
  xLH <- seq(meanLH-6,meanLH+6,length=10000) #Likelihood x values
  yLH <- dnorm(xLH,meanLH,sdLH) #Likelihood y values
  xP <- seq(meanP-6,meanP+6,length=10000) #Prior x values
  yP <- dnorm(xP,meanP,sdP) #Prior y values
  
  meanPP <- (sdLH^2*meanP+sdP^2*meanLH)/(sdP^2+sdLH^2) #Mean Posterior
  sdPP <- sqrt(1/((1/sdLH^2)+1/(sdP^2))) #SD Posterior
  
  xPP <- seq((meanPP-6),(meanPP+6),length=10000) #Posterior x values
  yPP <- dnorm(xPP,meanPP,sdPP) #Posterior y values
  
  x <- xLH
  y <- yLH
  aux1 <- cbind(x,y)
  aux1 <- data.frame(aux1)
  aux1$id <- "LH"
  
  x <- xP
  y <- yP
  aux2 <- cbind(x,y)
  aux2 <- data.frame(aux2)
  aux2$id <- "P"
  
  x <- xPP
  y <- yPP
  aux3 <- cbind(x,y)
  aux3 <- data.frame(aux3)
  aux3$id <- "PP"
  
  aux <- rbind(aux1,aux2,aux3)
  
  ggplot(aux,aes(x,y,color=factor(id))) +
    geom_line(size=1) +
    theme(
      legend.title=element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()) +
    labs(title = title) +
    scale_x_continuous(name="Stimulus Strength") +
    scale_y_continuous(name="Probability") +
    scale_colour_manual(values=ColorScheme3, name="",
                          labels=c("Likelihood", "Prior", "Posterior"))
}

#Prior, Likelihood and matched Posterior, no legend
drawBayGraphsNoLegend <- function(meanLH=0,sdLH=1,meanP=2,sdP=0.75, title=""){
  
  xLH <- seq(meanLH-6,meanLH+6,length=10000)
  yLH <- dnorm(xLH,meanLH,sdLH)
  xP <- seq(meanP-6,meanP+6,length=10000)
  yP <- dnorm(xP,meanP,sdP)
  
  meanPP <- (sdLH^2*meanP+sdP^2*meanLH)/(sdP^2+sdLH^2)
  sdPP <- sqrt(1/((1/sdLH^2)+1/(sdP^2)))
  
  xPP <- seq((meanPP-6),(meanPP+6),length=10000)
  yPP <- dnorm(xPP,meanPP,sdPP)
  
  x <- xLH
  y <- yLH
  aux1 <- cbind(x,y)
  aux1 <- data.frame(aux1)
  aux1$id <- "LH"
  
  x <- xP
  y <- yP
  aux2 <- cbind(x,y)
  aux2 <- data.frame(aux2)
  aux2$id <- "P"
  
  x <- xPP
  y <- yPP
  aux3 <- cbind(x,y)
  aux3 <- data.frame(aux3)
  aux3$id <- "PP"
  
  aux <- rbind(aux1,aux2,aux3)
  
  ggplot(aux,aes(x,y,color=factor(id))) + 
    theme_cowplot() +
    geom_line(size=1) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()) +
    labs(title = title) +
    scale_x_continuous(name="Stimulus Strength") +
    scale_y_continuous(name="Probability") +
    theme(legend.title=element_blank()) + 
    scale_colour_discrete(name="",
                          labels=c("Likelihood", "Prior", "Posterior")) +
    theme(legend.position = "")
}

getDFfor_Elev_Prime_Rel<- function(vh,vv,gval,y){
  TimeTo0 <- getTimetoYIsZero(vv,gval)
  t <- seq(0+TimeTo0/100,TimeTo0,TimeTo0/100)
  Elevation <- getElevation(t,vh,gval)
  ElevationYIndependent <- ElevationAngleYIndependent(t,gval,vv,vh,y)
  ElevationYIndependentHoriz <- ElevationAngleYIndependentHoriz(t,gval,vv,vh,y)
  #ElevationYIndependentInitRef <- ElevationAngleYIndependentInitRef(t,gval,vv,vh,y)
  ElevationPrime <- getElevationPrime(t,vh,gval)
  ElevationAnglePrimeYIndependent <- ElevationAnglePrimeYIndependent(t,gval,vv,vh,y)
  ElevationAnglePrimeYIndependentHoriz <- ElevationAnglePrimeYIndependentHoriz(t,gval,vv,vh,y)
  #ElevationAnglePrimeYIndependentInitRef <- ElevationAnglePrimeYIndependentInitRef(t,gval,vv,vh,y)
  aux <- cbind(t,Elevation,ElevationYIndependent,ElevationYIndependentHoriz,ElevationPrime,ElevationAnglePrimeYIndependent,ElevationAnglePrimeYIndependentHoriz)
  aux <- as.data.frame(aux)
  aux$RelElevationPrime <- aux$ElevationPrime/aux$Elevation
  aux$RelativeElevationPrimeYIndependentHoriz <- aux$ElevationAnglePrimeYIndependentHoriz/aux$ElevationYIndependentHoriz
  aux$RelativeElevationPrimeYIndependent <- aux$ElevationAnglePrimeYIndependent/aux$ElevationYIndependent
  aux$InvRelativeElevationPrimeYIndependent <- aux$ElevationYIndependent/aux$ElevationAnglePrimeYIndependent
  aux$tScaled <- seq(0.01,1,1/100)
  aux$tScaledto100 <- seq(1,100,1)
  aux}

getDFfor_VisualAngle_Prime_Rel<- function(vv,vh,gval,size=0.033,y){
  TimeTo0 <- getTimetoYIsZero(vv,gval)
  t <- seq(0+TimeTo0/100,TimeTo0,TimeTo0/100)
  VisualAngle <- getVisualAngle(t=t,size=size,vh=vh,vv=vv,g=gval)
  VisualAnglePrime <- getVisualAnglePrime(t=t,size=size,vh=vh,vv=vv,gval=gval)
  VisualAngleYIndependent <- getVisualAngleYIndependent(t,size,vh,vv,gval,y)
  VisualAnglePrimeYIndependent <- getVisualAnglePrimeYIndependent(t,size,vh,vv,gval,y)
  aux <- cbind(t,VisualAngle,VisualAnglePrime,VisualAngleYIndependent,VisualAnglePrimeYIndependent)
  aux <- as.data.frame(aux)
  aux$RelVisualAnglePrime <- aux$VisualAnglePrime/aux$VisualAngle
  aux$RelVisualAnglePrimeYIndependent <- aux$VisualAnglePrimeYIndependent/aux$VisualAngleYIndependent
  aux$InvRelVisualAnglePrimeYIndependent <- aux$VisualAngleYIndependent/aux$VisualAnglePrimeYIndependent
  aux$tScaled <- seq(0.01,1,1/100)
  aux$tScaledto100 <- seq(1,100,1)
  aux}


#Extract G from optic variables for more or less eye levelled parabolas
getGFromOptics1 <- function(g,vv,vh,size=0.033){
  TTC <- getTimetoYIsZero(vv = vv, g = g)-0.02
  ActualG <- g
  t <- seq(0,TTC,TTC/1000)
  Elev <- getElevation(t = t,vh = vh,g = g)
  ElevPrime <- getElevationPrime(t = t,vh = vh,g = g)
  Visual <- getVisualAngle(t = t,size = size,vh = vh, vv = vv, g = g)
  VisualPrime <- getVisualAnglePrime(t = t,size = size,vh = vh, vv = vv, gval = g)
  tPlus1 <- t+TTC/1000
  Percentage <- seq(0,100,1/10)
  VisualPrimeAttPlus1 <- getVisualAnglePrime(t = tPlus1,size = size, vv = vv,vh = vh, gval = g)
  g <- ((2*ElevPrime*size)/(Visual*cos(Elev)))*((Visual/(tPlus1-t)^2)*((1/VisualPrimeAttPlus1)^0.5-(1/VisualPrime)^0.5)^2+ElevPrime*tan(Elev))
  df <- as.data.frame(cbind(g,t,Elev,ElevPrime,Visual,VisualPrime,tPlus1,VisualPrimeAttPlus1,ActualG,Percentage))
}

#Adapting the previous solution ...
getGFromOptics <- function(g,velV,velH,size=0.033,y=1.85){
  TTC <- getTimetoYIsZero(vv = velV, g = g)-0.02
  ActualG <- g
  t <- seq(0,TTC,TTC/1000)
  Elev <- ElevationAngleYIndependent(t,g,velV,velH,y)
  ElevPrime <- ElevationAnglePrimeYIndependent(t,g,velV,velH,y)
  Visual <- getVisualAngleYIndependent(t,size,velH,velV,g,y)
  VisualPrime <- getVisualAnglePrimeYIndependent(t,size,velH,velV,g,y)
  tPlus1 <- t+TTC/1000
  Percentage <- seq(0,100,1/10)
  VisualPrimeAttPlus1 <- getVisualAnglePrimeYIndependent(tPlus1,size,velH,velV,g,y)
  g <- ((2*ElevPrime*size)/(Visual*cos(Elev)))*((Visual/(tPlus1-t)^2)*((1/VisualPrimeAttPlus1)^0.5-(1/VisualPrime)^0.5)^2+ElevPrime*tan(Elev))
  df <- as.data.frame(cbind(g,t,Elev,ElevPrime,Visual,VisualPrime,tPlus1,VisualPrimeAttPlus1,ActualG,Percentage))
}



getPosterior <- function(meanLH=0,sdLH=1,meanP=2,sdP=0.75, title=""){
  meanPP <- (sdLH^2*meanP+sdP^2*meanLH)/(sdP^2+sdLH^2)
  sdPP <- sqrt(1/((1/sdLH^2)+1/(sdP^2)))
  PP <- c(meanPP,sdPP)}

##error function
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

##binomial smooth whatever
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)}

##Tangential velocity
vTangential <- function(t,velV,velH,g){
  vv <- getVV(t,velV,g)
  vh <- velH
  vTan <- (vv^2+vh^2)^0.5
  vTan
}



##Distance to eye
distanceToSides <- function(t,size,velH,velV,g,y){
  ((size*2)/(2*cos((pi-getVisualAngleYIndependent(t,size,velH,velV,g,y))/2)))/2
}

distance <- function(t,size,velH,velV,g,y){
  a <- distanceToSides(t,size,velH,velV,g,y)
  c <- size*2
  h <- (a^2-(c/2)^2)^0.5
  h
}

yDistanceToEyeHeight <- function(t,g,velV,velH,y,size){
  distance(t,size,velH,velV,g,y)*sin((ElevationAngleYIndependentHoriz(t,g,velV,velH,y)))  
}

ElevationAngleWithStartLevel <- function(t,g,velV,velH,y,size){
  atan((yDistanceToEyeHeight(t,g,velV,velH,y,size) + (y-0.565))/
    ((distance(t,size,velH,velV,g,y)^2 - yDistanceToEyeHeight(t,g,velV,velH,y,size)^2)^0.5))
}


getYVelocity <- function(t,g,velV,velH,y,size,timesteps=0.01){
  a <- -distance(t,size,velH,velV,g,y)*sin(DegreeToRadians(90-RadiansToDegree(ElevationAngleYIndependent(t,g,velV,velH,y))))
  b <- -distance(t+timesteps,size,velH,velV,g,y)*sin(DegreeToRadians(90-RadiansToDegree(ElevationAngleYIndependent(t+timesteps,g,velV,velH,y))))
  c <- abs(b-a)/timesteps
  c
}

getGravity <- function(t,g,velV,velH,y,size,timesteps=0.01){
  abs((getYVelocity(t+timesteps,g,velV,velH,y,size)-getYVelocity(t,g,velV,velH,y,size))/timesteps)
}


##diffusion modelling
pf.fun <- function(x,t,a,n=2,m=1,phi=0.1,ro=5)
{0.5 *(1+erf( (a*(x^m)*t^(n/2))/(sqrt(phi*(2*ro+a*x^m)))))}


#based on differences in relative elev primes
EDifDiffusion <- function(subject,a,m,time=0.5){
  x4 <- x2[x2$id==subject,]
  x4$title <- sprintf("%s (a = %s)",subject,a)
  x4$DifPred <- pf.fun(x2$RelativeElevationPrimeYIndependentDifAv,time,a=a,m=m)
  ggplot(x4) + geom_line(aes(RelativeElevationPrimeYIndependentDifAv,DifPred),col=ColorScheme3[2]) + 
    binomial_smooth(aes(RelativeElevationPrimeYIndependentDifAv,correct),col="black",se=F) +
    facet_grid(. ~ title)
}

#based on difference in visual angle stuff
VDifDiffusion <- function(subject,a,m,time=0.5){
  x4 <- x2[x2$id==subject,]
  x4$title <- sprintf("%s (a = %s)",subject,a)
  x4$DifPred<- pf.fun(x4$RelativeVisualAnglePrimeAv,time,a=a,m=m)
  ggplot(x4) + geom_line(aes(RelativeVisualAnglePrimeAv,DifPred),col=ColorScheme3[3]) + 
    binomial_smooth(aes(RelativeVisualAnglePrimeAv,correct),col="black",se=F)+
    facet_grid(. ~ title)
}

#based on difference in Elev Prime/Elev*Vis
EEVDifDiffusion <- function(subject,a,m,time=0.5){
  x4 <- x2[x2$id==subject,]
  x4$title <- sprintf("%s (a = %s)",subject,a)
  x4$DifPred<- pf.fun(x4$ElevationPrimeYIndependentDifAvRelToElevAndVis,time,a=a,m=m)
  ggplot(x4) + geom_line(aes(ElevationPrimeYIndependentDifAvRelToElevAndVis,DifPred),col=ColorScheme3[3]) + 
    binomial_smooth(aes(ElevationPrimeYIndependentDifAvRelToElevAndVis,correct),col="black",se=F)+
    facet_grid(. ~ title)
}


ttc <- function(g,ElevPrime,Elev,Visu,size){
  (2*size*ElevPrime)/(g*Visu*cos(Elev))
}

gEstimate <- function(ttc,g,ElevPrime,Elev,Visu,size){
  -(2*size*ElevPrime)/(ttc*Visu*cos(Elev))
}

AddBinnedValues <- function(x){
  y <- data.frame(x=x,y=x)
  z <- binr::bins(x,target.bins = 7,max.breaks = 7, exact.groups = T)
  j <- binr::bins.getvals(z)
  for(i in 1:length(y$x)){
    if (y$x[i]>j[1][[1]] & y$x[i]<=j[2][[1]]){
      y$y[i] <- (z$xval[1]+j[2][[1]])/2}
    if (y$x[i]>j[2][[1]] & y$x[i]<=j[3][[1]]){
      y$y[i] <- (j[2][[1]]+j[3][[1]])/2}
    if (y$x[i]>j[3][[1]] & y$x[i]<=j[4][[1]]){
      y$y[i] <- (j[3][[1]]+j[4][[1]])/2}
    if (y$x[i]>j[4][[1]] & y$x[i]<=j[5][[1]]){
      y$y[i] <- (j[4][[1]]+j[5][[1]])/2}
    if (y$x[i]>j[5][[1]] & y$x[i]<=j[6][[1]]){
      y$y[i] <- (j[5][[1]]+j[6][[1]])/2}
    if (y$x[i]>j[6][[1]] & y$x[i]<=j[7][[1]]){
      y$y[i] <- (j[6][[1]]+j[7][[1]])/2}
    if (y$x[i]>j[7][[1]]){
      y$y[i] <- (z$xval[length(z$xval)]+j[3][[1]])/2}
  }
  y$y
}

#####air resistance stuff
getThetaRho <- function(t,size,vh,vv,x,y,idis)
{
  #size = Radius
  
  Xt = idis - x  #remaining X distance at display 1
  dtot <- sqrt(Xt*Xt + y*y)  # remaining distance
  thetat = atan(size/dtot)
  (thetat*2) # because size is Radius
}

#analitic functions for air drag

x_t_airdrag <- function(t,vh,vv,m=0.057,c=0.5,g=9.82)
{
  vt <- m*g/c
  Vo <- sqrt(vh*vh+vv*vv)
  alpha=asin(vv/Vo)
  
  return( ((Vo*vt)/g) *cos(alpha)*(1-exp(-g*t/vt)) ); #wolfram
  
}

y_t_airdrag <- function(t,vh,vv,m=0.057,c=0.5,g=9.82)
{
  vt <- m*g/c
  Vo <- sqrt(vh*vh+vv*vv)
  alpha=asin(vv/Vo)
  return ( (vt/g)*(Vo*sin(alpha)+vt)*(1-exp(-g*t/vt))-vt*t )
  
}

Time_to_y_is_zero_drag <- function(vv,g){
  m <- 0.057
  c <- 0.005
  vt <- m*g/c
  Vo <- sqrt(1+vv*vv)
  alpha <- asin(vv/Vo)
  2*(-vt/g)*log(-vt/(Vo*sin(alpha) + vt)) ##not sure why *2 is necessary, but data say it issss
  2*(vt/g)*log((Vo*sin(alpha) + vt)/vt)
}

####Curvature for parabolas with airdrag (2nd temporal derivative) ... actually spatial, but approximately enough I guess
Curvature_Drag <- function(t,vh,vv,m=0.057,c=0.005,g=9.82){
  vt <- m*g/c
  Vo <- sqrt(vh*vh+vv*vv)
  alpha=asin(vv/Vo)
  
  -(g*(vt+sin(alpha)*Vo)*exp(-(g*t)/vt))/vt}

#getParabolicCurvature <- function(g,vh){
#  g/(vh^2)}

vel3d <- function (t, x, y = rep(0, length(t)), z = rep(0, length(t)), 
                   lag = 2) 
{
  dxdt <- base::diff(x, lag = lag)/base::diff(t, lag = lag)
  dydt <- base::diff(y, lag = lag)/base::diff(t, lag = lag)
  dzdt <- base::diff(z, lag = lag)/base::diff(t, lag = lag)
  vel <- sqrt(dxdt^2 + dydt^2 + dzdt^2)
  vel <- c(rep(NA, lag), vel)
  vel
}


dxdt <- function (t, x, lag = 2) 
{
  dxdt <- base::diff(x, lag = lag)/base::diff(t, lag = lag)
  c(rep(NA, lag), dxdt)
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  #library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

getRadiusOfCurvature <- function(g,t,vv,vh){
  x <- vh*t
  abs((1+(-((g*1/vh)^2)*x + vv/vh)^2)^(2/3)/(-(g*1/vh)^2))
}
#Derivation:
#y(t) = -gt^2/2 + vv*t
#
#x(t) = t*vh
#t = x(t)/vh
#
#y(x) = -(g*(1/vh)^2*x^2)/2 + vv*x/vh
#y'(x) =  (-((g*1/vh)^2)*x + vv/vh)
#y''(x) = (-(g*1/vh)^2)
#Radius of Curvature:
#(1+y'(x)^(2/3))/y''(x)

getCurvature <- function(g,t,vv,vh){
  1/getRadiusOfCurvature(g,t,vv,vh)
}



####get matched velocities for parabolic motion
#derivation:

#(1. normal parabola) y(x) = -(g*(1/vh)^2*x^2)/2 + vv*x/vh (normal parabola)
#(2. matched parabola) y2(x) = (vv2/vh2)*x
#We know g, vh, vv for the normal parabola
#We know y(t) for the normal parabola: y(t) = -(g*t^2)/2 + vv*t
#vv2 should be constant
#problem!!! vh2 is thought of as constant, too!


#(A. From start to peak: if t <= TimeToYIsZero/2)
#(vv2/vh2)*x = -(g*(1/vh)^2*x^2)/2 + vv*x/vh
#vh2(x) = vv2/(-(g*(1/vh)^2*x)/2 + vv/vh)
#x(t) = vh*t
#vh2(t) = vv2/(-(g*(1/vh)^2*(vh*t))/2 + vv/vh)

#(B. From peak  to end: if t > TimeToYIsZero/2)
#(-vv2/vh2)*x = -(g*(1/vh)^2*x^2)/2 + vv*x/vh
#vh2(x) = -vv2/(-(g*(1/vh)^2*x)/2 + vv/vh)
#x(t) = vh*t
#vh2(t) = vv2/((g*(1/vh)^2*(vh*t))/2 - vv/vh)

#vv2 has to be such that vv2 = (maxheight/timetoyiszero*0.5)
#MaxHeight = (vv^2/g)/2
#TimeToPeak = Flighttime/2 = vv/g
#vv2 = (vv^2/g)/2)/(vv/g)
#vv2 = vv/2

getMatchedVelH <- function(t,g,vh,vv){
  aux <- data.frame(t=t,g=g,vh=vh,vv=vv)
  aux$TimeAtPeak <- getTimetoYIsZero(aux$vv,aux$g)/2
  aux$vv2 <- aux$vv/2
  aux$vh2[aux$t <= aux$TimeAtPeak] <- aux$vv2[aux$t <= aux$TimeAtPeak]/(-(aux$g[aux$t <= aux$TimeAtPeak]*(1/aux$vh[aux$t <= aux$TimeAtPeak])^2*(aux$vh[aux$t <= aux$TimeAtPeak]*aux$t[aux$t <= aux$TimeAtPeak]))/2 + aux$vv[aux$t <= aux$TimeAtPeak]/aux$vh[aux$t <= aux$TimeAtPeak])
  aux$vh2[aux$t > aux$TimeAtPeak] <- (aux$vv2[aux$t > aux$TimeAtPeak])/((aux$g[aux$t > aux$TimeAtPeak]*(1/aux$vh[aux$t > aux$TimeAtPeak])^2*(aux$vh[aux$t > aux$TimeAtPeak]*aux$t[aux$t > aux$TimeAtPeak]))/2 - aux$vv[aux$t > aux$TimeAtPeak]/aux$vh[aux$t > aux$TimeAtPeak])
  
  aux
}

GetMatchedParabola <- function(g,vv,vh,t){
  #creating the standard parabola
  t1 <- seq(0,t,t/199)
  x <- vh*t1
  y_of_x <- -(g*(1/vh)^2*x^2)/2 + vv*x/vh
  
  #dividing the y-range in equal parts (to simulate constant y-velocity)
  y <- seq(0,max(y_of_x),max(y_of_x)/99)
  
  #getting x_of_y as reverse function of y(x); divided in two segments
  x1_y <- (-vv/vh + (vv^2/vh^2-4*g*y/(2*vh^2))^0.5)/(-2*g/(2*vh^2)) #rising part
  x2_y <- (-vv/vh - (vv^2/vh^2-4*g*y/(2*vh^2))^0.5)/(-2*g/(2*vh^2)) #falling part

  #velV_new is constant: vv0/2
  #y_of_t is therefore (vv0/2)*t[1stPart] for the rising part,
  #y_of_t[1stPart] = vv/2*t[1stPart]
  #and 
  #y_of_t[2ndPart] = MaxHeight/2-(vv/2)*t[1stPart] for the falling part
  #MaxHeight = (vv^2/g)
  #y_of_t[2ndPart] = (vv^2/g)/2-(vv/2)*t1[1stPart]
  
  #to get x_of_t, we plug in y_of_t in x_of_y:
  x1_t <- (-vv/vh + (vv^2/vh^2-4*g*(vv/2*t1[1:100])/(2*vh^2))^0.5)/(-2*g/(2*vh^2)) #rising part
  x2_t <- (-1)*x1_t+2*max(x1_t)

  #x2_t <- (-vv/vh - (vv^2/vh^2-4*g*(((vv^2/g)/2-(vv/2)*t1[101:200])/(2*vh^2))^0.5)/(-2*g/(2*vh^2))) #falling part
  #x2_t <- x2_t*(-1)

  #put both vectors together for x_of_y and x_of_t and y_of_t
  x_y <- c(x1_y,sort(x2_y))
  x_t <- c(x1_t,sort(x2_t))
  
  yFinal <- c(y,seq(max(y_of_x),0,-max(y_of_x)/99))
  
  data.frame(t=t1,x_y=x_y,y=yFinal,x_t=x_t)
}

MaxHeightParabola <- function(g,vv){
  (vv^2/g)/2  
}


MatchedParabola_ConstantVelV <- function(g,velV,velH){
  #create standard parabola
  vv <- velV
  vh <- velH
  t1 <- seq(0,(round(velV/g,2)),0.01)
  t2 <- seq(round(velV/g,2)+0.01,round(2*velV/g,2),0.01) #time series from starting position back to the ground in steps of 0.01s
  t_overall <- c(t1,t2)
  y_of_t_Standard <- -(g*t_overall^2)/2 + velV*t_overall #y(t) for standard parabola
  x_of_t_Standard <- velH*t_overall #x(t) for standard parabola
  Percentage <- seq(100,0,-100/(length(t_overall)-1))
  t_end <- 2*velV/g
  #y(t) for first half of Constant velV parabola
  #derivation:
  #velV_new = average velocity across first half of parabola, which is half of the original vertical velocity
  #y(t) is just the new velocity multiplied with t, because the velocity is constant
  y_of_t_ConstantVelV_FirstHalf <- velV/2*t1
  #y(t) for first half of Constant velV parabola
  #Derivation:
  #y(t) = MaxHeight - velV_new*(t - vv/g)
  #t - vv/g means the time starts from 0 at peak
  t3 <- t2-round(velV/g,2)
  y_of_t_ConstantVelV_SecondHalf <- (velV^2/g)/2-(velV/2)*(t3) 
  
  #getting x_of_y as reverse function of y(x)
  #derivation:
  #x(t) = velH*t solved for t and plugged into
  #y(t) = (g*t^2)/2+velV*t
  #resulting y(x) solved for x is divided into two segments
  #Then I plugged in the new formula for y(t) to get x(t)
  x_of_t_ConstantvelV_FirstHalf <- (-vv/vh + (vv^2/vh^2-4*g*(vv/2*t1)/(2*vh^2))^0.5)/(-2*g/(2*vh^2)) #rising part
  x_of_t_ConstantvelV_FirstHalf[length(x_of_t_ConstantvelV_FirstHalf)] <- x_of_t_Standard[length(x_of_t_ConstantvelV_FirstHalf)]
  
  #I did some geometrical tricks for the second half because we know it's just a mirrored version of the first half:
  #so this is x_of_t_ConstantvelV_FirstHalf but "point mirrored" at the peak:
  x_of_t_ConstantvelV_SecondHalf <- 2*(-vv/vh + (vv^2/vh^2-4*g*(vv/2*(vv/g))/(2*vh^2))^0.5)/(-2*g/(2*vh^2)) -
    (-vv/vh + (vv^2/vh^2-4*g*(vv/2*(2*vv/g-t2))/(2*vh^2))^0.5)/(-2*g/(2*vh^2))
  data.frame(x_standard = x_of_t_Standard, y_standard = y_of_t_Standard, 
             x = c(x_of_t_ConstantvelV_FirstHalf,
                                     x_of_t_ConstantvelV_SecondHalf),
             y = c(y_of_t_ConstantVelV_FirstHalf,
                                     y_of_t_ConstantVelV_SecondHalf),
             t = c(t1,t2),
             Percentage = Percentage,
             t_end = t_end)
}

getXPos <- function(vh,t){
  vh*t
}

getYPos <- function(vv,g,t){
  -(g*t^2)/2+vv*t
}

#getMatchedVelocities <- function(velV,velH,g){
#  v_t_of_t <-   (velV_of_t^2+velH^2)^0.5
#  vh_new <- v_t_of^2-
#}

getVisualAnglesForCave <- function(zPos){
  x1 <- -atan((zPos/1.13))
  x2 <- atan(((1-zPos)/1.13))
  c(x1,RadiansToDegree(x1),x2,RadiansToDegree(x2))
}

getVVandVHStartPosIndependent <- function(minElev,maxElev,g,presTime,x0){
  
  velV1 <- (-(g*presTime)/2+
              (((g*presTime)/2)^2-
                 4*(tan(minElev)/(2*tan(maxElev))-1)*
                 ((((tan(minElev))^2)*x0*g)/tan(maxElev)-tan(minElev)*x0*g))^0.5)/
    (2*(tan(minElev)/(2*tan(maxElev))-1))
  velV2 <- (-(g*presTime)/2-
              (((g*presTime)/2)^2-
                 4*(tan(minElev)/(2*tan(maxElev))-1)*
                 ((((tan(minElev))^2)*x0*g)/tan(maxElev)-tan(minElev)*x0*g))^0.5)/
    (2*(tan(minElev)/(2*tan(maxElev))-1))
  
  if(velV1 > 0){
    velV <- velV1}
  if(velV2 > 0){
    velV <- velV2}
  
  velH <- (((velV^2)/(2*g)+x0*tan(minElev))/tan(maxElev)-x0)*(g/velV)
  
  y0 <- x0*tan(minElev)
  c(velV, velH, y0)
}


GetParabolasWithConstantTangentialSpeed <- function(g,vx,vy,x0=0,y0=0, framerate = 75){
  
  t <- seq(0,(2*(vy/g)),0.00001)
  x <- vx*t+x0
  y <- y0+vy*t-1/2*g*t^2
  
  print(c(t[length(t)],vx,vy))
  
  c <- c()
  a <- vy/vx
  b <- -g/vx^2
  c[1] <- a+b*x[1]
  c[2] <- a+b*x[length(x)]
  
  lunghezza_arco <- (sqrt(c[2]^2+1)*c[2]+asinh(c[2])-(sqrt(c[1]^2+1)*c[1]+asinh(c[1])))/(2*b)
  
  number_of_element <- framerate*t[length(t)] #75 Hz
  i1 <- 2
  arc_length <- 0
  index_CostSpeed <- c()
  for (i in 1:(number_of_element-1)){
    
    while (arc_length<lunghezza_arco/number_of_element){
      #     arc_length <- arc_length+sqrt(diff(x[(i1-1):i1])^2+diff(y[(i1-1):i1])^2)
      arc_length <- arc_length+((x[(i1-1)]-x[i1])^2+(y[(i1-1)]-y[i1])^2)^0.5
      i1 <- i1+1
    }
    index_CostSpeed[i] <- i1
    arc_length <- 0
  }
  
  df <- data.frame(x=x[index_CostSpeed],y=y[index_CostSpeed],t_end=round(t[length(t)],2), t = round(seq(0,round(t[length(t)],2),round(t[length(t)],2)/(number_of_element-2)),3), vx = vx, vy = vy, g = g)
  df
}


GetParabolasWithConstantTangentialSpeed_1s <- function(g,vx,vy,x0=0,y0=0){
  
  t <- seq(0,(2*(vy/g)),0.00001)
  x <- vx*t+x0
  y <- y0+vy*t-1/2*g*t^2
  
  print(c(t[length(t)],vx,vy))
  
  c <- c()
  a <- vy/vx
  b <- -g/vx^2
  c[1] <- a+b*x[1]
  c[2] <- a+b*x[length(x)]
  
  lunghezza_arco <- (sqrt(c[2]^2+1)*c[2]+asinh(c[2])-(sqrt(c[1]^2+1)*c[1]+asinh(c[1])))/(2*b)
  
  number_of_element <- 75 #75 Hz
  i1 <- 2
  arc_length <- 0
  index_CostSpeed <- c()
  for (i in 1:(number_of_element-1)){
    
    while (arc_length<lunghezza_arco/number_of_element){
      #     arc_length <- arc_length+sqrt(diff(x[(i1-1):i1])^2+diff(y[(i1-1):i1])^2)
      arc_length <- arc_length+((x[(i1-1)]-x[i1])^2+(y[(i1-1)]-y[i1])^2)^0.5
      i1 <- i1+1
    }
    index_CostSpeed[i] <- i1
    arc_length <- 0
  }
  
  df <- data.frame(x=x[index_CostSpeed],y=y[index_CostSpeed],t_end=round(t[length(t)],2), t = round(seq(0,round(t[length(t)],2),round(t[length(t)],2)/(number_of_element-2)),3), vx = vx, vy = vy, g = g)
  df
}


GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

WFtoSD = function(WF){
  
  FitSD = function(WF,SD){
    ((pnorm(1-WF,1,SD) - 0.25)^2)^0.5}
  
  optimize(FitSD,WF = WF,interval = c(0,10))$minimum
  
}
