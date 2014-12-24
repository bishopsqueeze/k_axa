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
    return(data.frame(index=idx, ids=ids))
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
    
    if (is.null(nrow(a))) {
        result  <- c(NA, NA, NA)
        result[1] <- a[2] * b[3] - a[3] * b[2]
        result[2] <- a[3] * b[1] - a[1] * b[3]
        result[3] <- a[1] * b[2] - a[2] * b[1]
    } else {
        result  <- matrix( NA, nrow( a ), 3 )
        result[,1] <- a[,2] * b[,3] - a[,3] * b[,2]
        result[,2] <- a[,3] * b[,1] - a[,1] * b[,3]
        result[,3] <- a[,1] * b[,2] - a[,2] * b[,1]
    }
    return(result)
}


##------------------------------------------------------------------
## <function> eucDist
##------------------------------------------------------------------
eucDist <- function(x, y) {
        return(sqrt((x*x)+(y*y)))
}


##------------------------------------------------------------------
## <function> revPath
##------------------------------------------------------------------
revPath <- function(p)
{
    return(data.frame(apply(p, 2, function(x) {x - x[length(x)]})))
}














procTraj <- function(myTraj)
{
    ## get trajectory length
    p           <- myTraj$p
    n           <- nrow(p)
    
    ## compute the center of mass
    com.p       <- c(mean(p$x), mean(p$y))
    com.r       <- sqrt(com.p %*% com.p)
    com.theta   <- ifelse(com.p[1] < 0, pi + atan(com.p[2]/com.p[1]), atan(com.p[2]/com.p[1]))
    com.rot     <- rbind( c(cos(-com.theta),-sin(-com.theta)), c(sin(-com.theta),cos(-com.theta)))
    
    ## rotate the trajectory
    p.rot       <- t(com.rot %*% t(p))
    p.rot       <- data.frame(x=p.rot[,1], y=p.rot[,2])
    p.rot.com   <- c(mean(p.rot$x), mean(p.rot$y))
   
   
p.rot.sm    <- data.frame(x=ma(p.rot$x, order=3), y=ma(p.rot$y, order=3))


    ## translate (x,y) into polar coordinates (r,theta)
    r       <- eucDist(p.rot$x, p.rot$y)
    theta   <- c(0, atan(p.rot$y / p.rot$x)[2:nrow(p.rot)])     ## exclude origin

    ## compute radial differences
    r.diff  <- diff(r)
    
    ## compute time vectors
    t       <- 0:(n-1)
    dt      <- diff(t)
    ct      <- cumsum(dt)

    ## identify true stop points
    sp.bool <- (r.diff == 0)
    sp.idx  <- which(r.diff == 0)

    ## remove true stop points
    p.rot.ns    <- p.rot[-(sp.idx+1),]
    t.ns        <- c(0, ct[-sp.idx])
    dt.ns       <- diff(t.ns)

    ## reconstruct original-length trajectory via interpolation (and remove end stop points)
    t.max       <- max(t)
    t.max.ns    <- max(t.ns)
    t.interp    <- 0:min(t.max, t.max.ns)
    
    x.interp    <- approx(x=t.ns, y=p.rot.ns$x, xout=t.interp, method="linear")$y
    y.interp    <- approx(x=t.ns, y=p.rot.ns$y, xout=t.interp, method="linear")$y

    p.rot.int   <- data.frame(x=x.interp, y=y.interp)
    r.rot.int   <- eucDist(p.rot.int$x, p.rot.int$y)


    ## identify end-of-trajectory idling
    r.end       <- r.rot.int[length(r.rot.int)]
    r.idle.idx  <- sort(which(abs(r.rot.int - r.end) < 1), decreasing=TRUE)
    r.idle.diff <- -diff(r.idle.idx)
    r.idle.min  <- r.idle.idx[ which(r.idle.diff > 1)[1] ]  ## first instance of a skip


    ## identify end stall points
    #t.window    <- 5                ## time window (in sec)
    #r.window    <- 1                ## radial window (in meters)
    #len         <- length(t.interp)
    #
    #    ## step 1: identify neighbors within (r.window, t.window)
    #    tmp <- vector(,length=len)
    #    for (i in 1:len) {
    #        tmp[i] <- length(which((abs(r.rot.int - r.rot.int[i]) <= r.window) & (abs(t.interp - t.interp[i]) <= t.window)))
    #    }
    #
    #    ## step 2: smooth and identify the last instance with no stalls
    #    tmp.sm      <- ma(tmp, order=(2*t.window))
    #    tmp.sm.last <- max(which((tmp.sm < t.window) & !is.na(tmp.sm)))

    ## compute trajectories with end stalls truncated
    p.rot.int.tr     <- p.rot.int[1:(r.idle.min+1),]
    t.rot.int.tr     <- t.interp[1:(r.idle.min+1)]
    r.rot.int.tr     <- eucDist(p.rot.int.tr$x, p.rot.int.tr$y)
    theta.rot.int.tr <- c(0, atan(p.rot.int.tr$y / p.rot.int.tr$x)[2:nrow(p.rot.int.tr)])     ## exclude origin

    ## compute the angle/arc-length data
    vt          <- data.frame(  vx=na.omit(ma(diff(p.rot.int.tr$x), order=5)),
                                vy=na.omit(ma(diff(p.rot.int.tr$y), order=5)),
                                vz=0)
    vt.ref      <- 0*vt
    vt.ref$vx   <- 1
    
    vt.nrm      <- apply(vt, 1, function(x){sqrt(x %*% x)})
    vt.dot      <- apply(vt * vt.ref, 1, sum)
    vt.ref.nrm  <- apply(vt.ref, 1, function(x){sqrt(x %*% x)})
    
    vt.arc      <- vt.nrm / sum(vt.nrm)
    vt.ang      <- sign(vcrossp(vt, vt.ref)[,3]) * acos(vt.dot / (vt.nrm * vt.ref.nrm) )
    
    ## smooth the trajectory
    p.rot.int.tr.sm     <- data.frame(x=ma(p.rot.int.tr$x,order=3), y=ma(p.rot.int.tr$y, order=5))

    ## compute additional metrics
    r.max   <- max(r.rot.int.tr)
    t.dur   <- max(t.rot.int.tr)
    t.max   <- t.rot.int.tr[which(r.rot.int.tr == r.max)]
    
}


##------------------------------------------------------------------
## <function> revPath
##------------------------------------------------------------------
baseTrajParams <- function(myTraj)
{
    p   <- myTraj$p
    n   <- nrow(p)
    r   <- eucDist(p$x, p$y)
    v   <- data.frame(vx=diff(p$x), vy=diff(p$y))
    vr  <- eucDist(v$vx, v$vy)
    t   <- 1:n
    
    return(
        list(
            avg.r=mean(r),
            max.r=max(r),
            trip.r=r[n],
            avg.v=mean(vr),
            max.t=t[which(r == max(r))[1]],
            trip.t=t[n]
            ))
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



## Need to review trajectories for:
## - small distances travelled
## - large jumps 










## The trips are recordings of the car's position (in meters) every second and look like the following:
## A prius is ~ 4.5M in length


## load a trajectory file
load(paste0(proc.dir,"/",113,"_trajectories.Rdata"))



for (j in 1:200)
{
    tmp <- procTraj(traj[[j]])
    
    
}

a <- lapply(traj, baseTrajParams)
b <- do.call("rbind", lapply(a, rbind))

plot( unlist(lapply(a, function(x){x$max.r})) , unlist(lapply(a, function(x){x$trip.r}))  )
points( unlist(lapply(a, function(x){x$max.r})) , unlist(lapply(a, function(x){x$dur.t})) , col="red" )








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




