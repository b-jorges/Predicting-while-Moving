myUtils <- new.env(parent=baseenv())
attr( myUtils , "name" ) = "myUtils"

myUtils$home <-"/Users/joan1"
myUtils$homeR <- paste(myUtils$home,"/OneDrive - Universitat de Barcelona/R",sep="")
myUtils$so_libpath <- paste(myUtils$home,"/OneDrive - Universitat de Barcelona/myLibs/bin/R",sep="")

myUtils$ed <- function(name,dir="/Users/joan1/OneDrive - Universitat de Barcelona/R")
{
  name <- as.character(substitute(name))
  p <- file.path(dir,paste(name))
  # print(p)
  # file.edit(file=p,editor="RStudio")
  file.edit(p)
}
myUtils$installFromCran <- function(packs)
  install.packages(packs, repos = "https://cran.revolutionanalytics.com")

myUtils$pdet2 <- function(pattern="01",x)
{
  #	pattern <- as.character(substitute(pattern))
  if(length(x)>9974)
    stop("Vector too long")
  grepRaw(pattern,gsub("[[:space:]]|,","",toString(as.numeric(x))),fixed=T,all=T)  
}

myUtils$lst <- function(envir=myUtils) ls(envir)

myUtils$lstype<-function(type='closure'){ 
  inlist<-ls(.GlobalEnv)
  if (type=='function') type <-'closure'
  typelist<-sapply(sapply(inlist,get),typeof)
  return(names(typelist[typelist==type]))
}

myUtils$dirR <- function()
{
  dir(homeR)  
}


myUtils$GetColorHexAndDecimal <- function(color)
{
  c <- col2rgb(color)
  sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}

myUtils$findex <- function(f)
{
  if(is.factor(f)==FALSE)
    stop("f not a factor")
  lb <- levels(f)
  match(f,lb)
}

myUtils$inrange <- function(x,r)
{
  
  (x>r[1] & x <r[2])
  
}

myUtils$non_overlap <- function(x,fac=0.02)
{
  
  o <- order(x)	
  i1 <- abs(diff(x[o]))<fac
  y <- x[o[i1]] -fac
  x[o[i1]] <- y
  x
}


myUtils$qw <- function(width=5,height=5,...){
  #x11(":0.0",width=width,height=height)
  quartz(width=width,height=height,...)
}

myUtils$mmax <- function(m)
  which(m==max(m),arr.ind = T)
myUtils$mmin <- function(m)
  which(m==min(m),arr.ind = T)


# flip <- function(x)
# {
# 	(x[seq(length(x),1,by=-1)])
# }


#s<-function(f) source(f)

myUtils$toDeg <- function(x)
{
  (x*180/pi)
}

myUtils$toRad <- function(x) 
{
  (x*pi/180)
}

myUtils$norm01 <- function(x,min=0,max=1)
{
  minx <- min(min(x,na.rm=T))
  maxx<-max(max(x,na.rm=T))
  
  xrange<-maxx-minx
  
  y<-(x-minx)/xrange
  y <- (max-min)*y + min
  y
}

myUtils$erf <- function(x) 2 * pnorm(x / sqrt(2)) - 1

myUtils$dirR <- function()
{
  dir(path=paste(home,"/R",sep=""))
}

myUtils$last <- function(x)
{
  (x[length(x)])
}

myUtils$def <- function(func,dir="/Users/joan1/OneDrive - Universitat de Barcelona")
{
  func <- as.character(substitute(func))
  source(file.path(dir,"R",paste(func,".R",sep="")))				
}

myUtils$len<-function(x,...)
{
  length(x)
}

myUtils$obs <- function(x,...)
{
  length(x[-which(is.na(x)==TRUE)]) 
  
}

myUtils$f2n <- function(fac)
{
  if(nargs()==0){
    print("f2n(factor)")
    return("(c) Joan LM")
  }
  as.numeric(fac)->id
  (as.numeric(levels(fac)[id]))
  
}


myUtils$ff <- function(fname)
{
  
  fname <- as.character(substitute(fname))
  p1 <- paste("cat",paste(homeR,"/*.R",sep=""),"|",paste(so_libpath,"/findRfunc",sep=""))
  (system(paste(p1,fname,">kk")))
  
  source("kk")
  a <- system("cat kk")
  system("rm kk")
  a
}

myUtils$ul <- function(lib=paste(so_libpath,"/motor.so",sep="") )
{
  dyn.unload(lib)
}


myUtils$closer <- function(x,tomatch)
{
  ind <- rep(0,length(tomatch))
  
  for(i in 1:length(tomatch)){
    ind[i] <- which.min(abs(x-tomatch[i]))[1]
  }
  ind
}


myUtils$drop.col <- function(x,drops)
{
  x[,!(names(x) %in% drops)]
}

myUtils$drop.row <- function(x,drops)
{
  x[-drops,]
}

myUtils$drop.rep <- function(x)
{
  d <- diff(x)
  x[c(T, d!=0)]
}


myUtils$wait <- function(t)
{
  start <- proc.time()[3]
  while((proc.time()[3] - start) < t){
    
  }
  
}


myUtils$insert.row <- function(dfr,row2=NULL,after=2)
{
  
  rbind(dfr[1:after,],row2,dfr[(after+1):nrow(dfr),])
  
}

myUtils$move.row <- function(dfr,row2move=NULL,after=2)
{
  
  row2 <- dfr[row2move,]
  dfr <- dfr[-row2move,]
  if(after<row2move)
   res <-  rbind(dfr[1:after,],row2,dfr[(after+1):nrow(dfr),])
  else
   res <-  rbind(dfr[1:(after-1),],row2,dfr[after:nrow(dfr),])
    
  res 
}

myUtils$cindex <- function(i,N)
{
  #return index of element i in circular array of N elements
  
  n <- i%%N
  if(n==0)
    n <- N
  n
}

# myUtils$do.cond <- function(d,names=as.character(groups(d)))
# {
#   getname <- function(x,n){
#     v=x[1,(names(x) %in% n)]
#     v
#   }
#   #	print(names)
#   res <- do(d,getname,n=names)
#   res
#   #	sapply(res,getvalue,)
#   
# }
# 
# 
# myUtils$do.val <- function(x,name,ind=NULL)
# {
#   if(is.null(ind)){
#     
#     r <- x[names(x) %in% name]
#   }else{
#     r<- x[ind]
#   }	
#   as.numeric(unlist(r))
# }
# 
# myUtils$do.fac <- function(x,name,ind=NULL)
# {
#   if(is.null(ind)){
#     
#     r <- x[names(x) %in% name]
#   }else{
#     r<- x[ind]
#   }	
#   r <- unlist(r)
#   names(r) <- NULL
#   r
# }


#myUtils$MER <- function()
#  source("/Users/joan1/R/MERpsychophysics.0/MERpsychophysics.r")

while("myUtils" %in% search())
  detach("myUtils")

attach(myUtils)

#require(Rcpp)
#cat("sourcing cpp functions ...")
#sourceCpp("~/R/develop/rcpp/pdetect.cpp")
#sourceCpp("~/R/develop/rcpp/findIntv.cpp")
#sourceCpp("~/R/develop/rcpp/findVthres.cpp")

#cat("done!")

#get only legend from ffplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
