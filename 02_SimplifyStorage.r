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
drivers     <- as.character(sort(as.numeric(dir())))
drivers.num <- length(drivers)

##------------------------------------------------------------------
## Loop over each driver; read data and save to a list
##------------------------------------------------------------------
for (i in 1:drivers.num) {
    
    ## grab the .Rdata route files and sort
    tmp.driver  <- drivers[i]
    tmp.routes  <- paste0(sort(as.numeric(gsub(".csv","",dir(paste0(drivers.dir,"/",tmp.driver),pattern=".csv")))),".csv")
    tmp.num     <- length(tmp.routes)
    
    ## define the output list
    traj            <- vector(mode="list", tmp.num)
    
    ##------------------------------------------------------------------
    ## Loop over all routes; compute basic trajectory parameters
    ##------------------------------------------------------------------
    for (j in 1:tmp.num) {
        
        tmp.filename    <- tmp.routes[j]
        tmp.route       <- gsub(".csv","",tmp.filename)
        
        ##------------------------------------------------------------------
        ## read the trajectory file
        ##------------------------------------------------------------------
        p <- read.csv(file=paste0(tmp.driver,"/",tmp.filename), header=TRUE)
        
        ##------------------------------------------------------------------
        ## compute base trajectory parameters
        ##------------------------------------------------------------------
        traj[[j]]$driver_id <- tmp.driver
        traj[[j]]$route_id  <- tmp.route
        traj[[j]]$filename  <- tmp.filename
        traj[[j]]$p         <- p
        traj[[j]]$np        <- nrow(traj[[j]]$p)

        ##------------------------------------------------------------------
        ## remove loaded trajectories
        ##------------------------------------------------------------------
        rm(p)
    }
    ##------------------------------------------------------------------
    ## save results and remove the list
    ##------------------------------------------------------------------
    save(traj, file=paste0(proc.dir,"/",tmp.driver,"_trajectories.Rdata") )
    rm(traj)
}

