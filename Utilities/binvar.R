library(dplyr)
library(fields)
library(binr)

binvar <- function(x,n=10,breaks=NULL,probs=NULL,method=1,...)
{
  
  if(method==2){
    if(is.null(probs))
      stop("defineix talls de probabilitat (probs) per crear els bins")
    #cut(.df$value,breaks=quantile(.df$value,probs=seq(0,1,0.1),na.rm=TRUE))
    vec <- quantile(x,probs=probs,na.rm=TRUE)
    names(vec) <- NULL
    #  print(vec)
    
    #res <- vec[closer(vec,x)] 
    res <- f2n(cut(x,breaks=vec,labels=vec[-1],include.lowest = TRUE))
  }
  #algo 1
  
  if(method==1){
    cen <- stats.bin(x,x,N=n,breaks=breaks)$centers
    res <- cen[closer(cen,x)]
  }
  res
}

AddBinnedValues <- function(x){
  y <- data.frame(x=x,y=x)
  z <- bins(x,target.bins = 7,max.breaks = 7, exact.groups = T)
  j <- bins.getvals(z)
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
