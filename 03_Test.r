##------------------------------------------------------------------
## Loads all of the driver-specific routes into a single list and
## then saves the list to a file (<driver_id>_trajectories.Rdata)
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## <function> getDriverIds
##------------------------------------------------------------------
getDriverIds    <- function(d)
{
    ids <- sort(as.numeric(gsub("_trajectories.Rdata", "", x=dir(d))))
    idx <- 1:length(ids)
    return(data.frame(id=ids, index=idx))
}


##------------------------------------------------------------------
## <function> iterativeNormalization
##------------------------------------------------------------------
iterativeNormalization <- function(s)
{
    seqNorm <- s
    k       <- 10
    
    for (i in 1:k)
    {
        avgValue <- mean(seqNorm, na.rm=TRUE)
        seqNorm  <- seqNorm - avgValue
        seqNorm  <- seqNorm + (seqNorm < -pi)*(2*pi) + (seqNorm > pi)*(-2*pi)
     }
    return(seqNorm)
}

##------------------------------------------------------------------
## <function> vcrossp (vector cross product)
##------------------------------------------------------------------
vcrossp <- function( a, b ) {
    result <- matrix( NA, nrow( a ), 3 )
    result[,1] <- a[,2] * b[,3] - a[,3] * b[,2]
    result[,2] <- a[,3] * b[,1] - a[,1] * b[,3]
    result[,3] <- a[,1] * b[,2] - a[,2] * b[,1]
    result
}





##------------------------------------------------------------------
## Define directories
##------------------------------------------------------------------
proc.dir    <- "/Users/alexstephens/Development/kaggle/axa/data/proc"

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd(proc.dir)

##------------------------------------------------------------------
## Grab the driver ids
##------------------------------------------------------------------
driver.ids  <- getDriverIds(proc.dir)

## The trips are recordings of the car's position (in meters) every second and look like the following:
## A prius is ~ 4.5M in length


## load a trajectory file
load(paste0(proc.dir,"/",3471,"_trajectories.Rdata"))


eucDist <- function(x,y)
{
    return( sqrt( (x * x) + (y * y) ))
}

revPath <- function(p)
{
    return(data.frame(apply(p, 2, function(x) {x - x[length(x)]})))
}




## example trajectory
t       <- traj[[14]]
s       <- traj[[22]]


## radial path
t.r         <- eucDist(t$p$x, t$p$y)
s.r         <- eucDist(s$p$x, s$p$y)

t.r.diff    <- c(0, diff(t.r))
t.r.idle    <- ( abs(t.r.diff) < 0.5 )      ## assumes a resulotion limit

a <- cbind(t$p, t.r, t.r.diff, t.r.idle)


## isolate non-idle path
t.pm   <- t$p[!t.r.idle,]
t.pm2    <- t$p[!t.r.idle2,]p


## velocity data (when in motion)
t.vm    <- data.frame(vx=diff(t.pm$x), vy=diff(t.pm$y))

t.vmr   <- eucDist(t.vm$vx, t.vm$vy)




## probably a resolution limit to the position information











for (i in c(11,14,22)) {
    if (i == 11) {
        plot(traj[[i]]$p$x, traj[[i]]$p$y,, type="l", pch=20, col=i, xlim=c(-800,800),ylim=c(-800,800))
    } else {
        lines(traj[[i]]$p$x, traj[[i]]$p$y, type="l", pch=20, col=i)
    }
}





tmp <- traj[[2]]

vt          <- data.frame(tmp$v, vz=0)
vref        <- 0*vt
vref$vx     <- 1


vt.dot      <- apply(vt * vref, 1, sum)
vt.norm     <- apply(vt, 1, function(x){sqrt(x %*% x)})
vref.norm   <- apply(vref, 1, function(x){sqrt(x %*% x)})


v.ang       <- acos(vt.dot / (vt.norm * vref.norm))
v.sign      <- sign(vcrossp(vt, vref)[,3])

ang <- v.ang*v.sign

ang2 <- iterativeNormalization(ang)

plot(ang, type="l", ylim=c(-pi, pi))
lines(ang2, col="red")






## simple radial distance measure
r   <- sqrt((tmp$p$x)^2 + (tmp$p$y)^2)



n.traj  <- length(traj)

traj.nobs   <- unlist(lapply(traj, function(a){a$np}))
max.nobs    <- max(traj.nobs)

padTrajectories <- function(pvec, max.len)
{
    pven.len  <- nrow(pvec)
    vec       <- NA*vector(,length=max.len)
    
    return(vec[1:pven.len] <- sqrt( (pvec$x)^2 +  (pvec$y)^2 ))
}


dtwOmitNA <-function (x,y)
{
    a<-na.omit(x)
    b<-na.omit(y)
    return(dtw(a,b,distance.only=TRUE)$normalizedDistance)
}

## create a new entry in the registry with two aliases
pr_DB$set_entry(FUN = dtwOmitNA, names = c("dtwOmitNA"))




traj.r  <- lapply(traj, function(x) { sqrt( (x$p$x)^2 + (x$p$y)^2 ) })

r.mat   <- matrix(NA, nrow=200, ncol=max.nobs)

for (i in 1:200)
{
    r.mat[i,1:length(traj.r[[i]])] <- traj.r[[i]]
    
}

d<-dist(r.mat, method = "dtwOmitNA")



head(r.mat[1:100,1:100])

library(dtw)




