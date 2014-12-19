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
## Define directories
##------------------------------------------------------------------
drivers.dir <- "/Users/alexstephens/Development/kaggle/axa/data/drivers"
proc.dir    <- "/Users/alexstephens/Development/kaggle/axa/data/proc"

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd(drivers.dir)

##------------------------------------------------------------------
## Grab the driver ids
##------------------------------------------------------------------
drivers.ls  <- as.character(sort(as.numeric(dir())))
drivers.num <- length(drivers.ls)

##------------------------------------------------------------------
## Loop over each driver; read data and save to a list
##------------------------------------------------------------------
for (i in 1:drivers.num) {
    
    ## grab the .Rdata route files and sort
    tmp.driver  <- drivers.ls[i]
    rdata.files <- dir(paste0(drivers.dir,"/",tmp.driver),pattern=".Rdata")
    
    routes.files    <- paste0(as.character(sort(as.numeric(gsub(".Rdata","",x=rdata.files)))),".Rdata")
    routes.num      <- length(routes.files)
    
    ## define the output list
    traj            <- vector(mode="list", routes.num)
    
    ##------------------------------------------------------------------
    ## Loop over all routes; compute basic trajectory parameters
    ##------------------------------------------------------------------
    for (j in 1:routes.num) {
        
        tmp.file    <- routes.files[j]
        tmp.route   <- gsub(".Rdata","",tmp.file)
        
        ##------------------------------------------------------------------
        ## read the trajectory file
        ##------------------------------------------------------------------
        load(paste0(tmp.driver,"/",tmp.file))
        
        ##------------------------------------------------------------------
        ## compute base trajectory parameters
        ##------------------------------------------------------------------
        traj[[j]]$driver_id <- tmp.driver
        traj[[j]]$route_id  <- tmp.route
        traj[[j]]$p         <- p
        traj[[j]]$np        <- nrow(traj[[j]]$p)
        traj[[j]]$v         <- data.frame(vx=diff(p$x), vy=diff(p$y))
        traj[[j]]$a         <- data.frame(ax=diff(diff(p$x)), ay=diff(diff(p$y)))
        traj[[j]]$cm_p      <- apply(traj[[j]]$p, 2, mean)
        traj[[j]]$cm_v      <- apply(traj[[j]]$v, 2, mean)
        traj[[j]]$cm_a      <- apply(traj[[j]]$a, 2, mean)
        traj[[j]]$al_p      <- sqrt(apply((traj[[j]]$p*traj[[j]]$p),1,sum))
        traj[[j]]$al_v      <- sqrt(apply((traj[[j]]$v*traj[[j]]$v),1,sum))
        traj[[j]]$al_a      <- sqrt(apply((traj[[j]]$a*traj[[j]]$a),1,sum))

        ##------------------------------------------------------------------
        ## remove loaded trajectories
        ##------------------------------------------------------------------
        rm(p, v)
    }
    ##------------------------------------------------------------------
    ## save results and remove the list
    ##------------------------------------------------------------------
    save(traj, file=paste0(proc.dir,"/",tmp.driver,"_trajectories.Rdata") )
    rm(traj)
}

