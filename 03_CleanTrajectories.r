##------------------------------------------------------------------
## Loads all of the driver-specific routes into a single list and
## then saves the list to a file (<driver_id>_trajectories.Rdata)
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(forecast)

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


##------------------------------------------------------------------
## <function> getVelocities
##------------------------------------------------------------------
getVelocities <- function(myTraj)
{
    return(eucDist(diff(myTraj$p$x),diff(myTraj$p$y)))
}

getLargeVelocities <- function(myTraj)
{
    ## velocities
    p       <- myTraj$p
    n       <- nrow(p)
    v       <- eucDist(diff(p$x), diff(p$y))

    return(which(v > 50))
}

##------------------------------------------------------------------
## <function> checkNeighbors
##------------------------------------------------------------------
checkNeighbors <- function(t,r, r.res=0.5, t.res=10)
{
    r.len   <- length(r)
    t.len   <- length(t)
    
    if (r.len == t.len) {
        
        nearNeighbors <- sapply(1:t.len, function(x){ length(which( (abs(t - t[x])<=t.res) & (abs(r - r[x])<=r.res))) })
        
    }
    
}


##------------------------------------------------------------------
## <function> cleanTraj
##------------------------------------------------------------------
cleanTraj <- function(myTraj)
{
    ## extract the trajectory
    p   <- myTraj$p
    n   <- nrow(p)
    r   <- eucDist(p$x, p$y)
    
    ## take the radial difference and smooth the results
    k       <- 5
    dr      <- diff(r)
    dr.s    <- as.vector(na.omit(ma(dr, order=k)))
    dr.mask <- c(rep(FALSE, as.integer(k/2)), (abs(dr.s) > 0.5), rep(FALSE, as.integer(k/2)))
    
    
    
    
    
    
    
    ## skip no-movement trajectories
    if (sum(na.omit((dr.s < 0.5))) == length(na.omit(dr.s))) {

        p.new   <- p.hat
        tx.out  <- NA
        ty.out  <- NA
        p.fl    <- "raw"

    } else {
        
        ## trim beginning and ending sequences with minimal changes
        ## (here assuming a 0.5 m resolution) ... otherwise the
        ## numerical noise will cause the timeseries cleans to fail
        dr.idx  <- which(dr.s > 0.5)
        dr.min  <- min(dr.idx)
        dr.max  <- max(dr.idx)
    
        t.hat   <- dr.min:dr.max
        p.hat   <- p[t.hat,]
        n.hat   <- nrow(p.hat)
        r.hat   <- eucDist(p.hat$x, p.hat$y)
        
        tx      <- ts(diff(p.hat$x), start=1, end=(n.hat-1))
        tx.out  <- tsoutliers(tx)
        tx.new  <- tsclean(tx, replace.missing=FALSE)
        
        ty      <- ts(diff(p.hat$y), start=1, end=(n.hat-1))
        ty.out  <- tsoutliers(ty)
        ty.new  <- tsclean(ty, replace.missing=FALSE)

        p.new   <- data.frame(x=c(0,cumsum(as.vector(tx.new))), y=c(0, cumsum(as.vector(ty.new))))
        p.fl    <- "clean"
     
    }
    return(list(p.orig=p, p.new=p.new, p.fl=p.fl, tx.out=tx.out, ty.out=ty.out))
}


##------------------------------------------------------------------
##
## Main
##
##------------------------------------------------------------------


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
driver.num  <- nrow(driver.ids)

clean.list  <- list()
clear.iter  <- 1

for (i in 1:1)
{
    ## isolate the driver
    tmp.driver <- driver.ids[i,2]
    
    ## load a raw trajectory file
    load(paste0(proc.dir,"/",tmp.driver,"_trajectories.Rdata"))


    for (j in 1:200)
    {
        clean.list[[j]] <- cleanTraj(traj[[j]])
    }

}











## junkyard

bad.idx <- which(v > 60)




## no jumps
if (length(bad.idx) == 0) {
    
    p.new   <- p
    
    ## single jump -- step function
} else if (length(bad.idx) == 1) {
    
    p.shk   <- p[(bad.idx+1),] - p[(bad.idx),]
    
    p.corr  <- data.frame(
    x=p[(bad.idx+1):n,1] - as.vector(unlist(rep(p.shk[1],(n-bad.idx)))),
    y=p[(bad.idx+1):n,2] - as.vector(unlist(rep(p.shk[2],(n-bad.idx))))
    )
    
    p.new   <- rbind(p[(1:bad.idx),],p.corr)
    
    
} else {
    
    ## is it a contiguous block
    if (sum((diff(bad.idx) == 1)) == (length(bad.idx)-1)) {
        
        p.orig  <- data.frame(t=1:n, x=p$x, y=p$y)
        p.orig  <- p.orig[-bad.idx,]
        p.new   <- data.frame(x=approx(x=p.orig$t, y=p.orig$x, xout=1:n, method="linear")$y, y=approx(x=p.orig$t, y=p.orig$y, xout=1:n, method="linear")$y)
        
    } else {
        p.new   <- 0*p
    }
    
    
}

return(bad.idx)






