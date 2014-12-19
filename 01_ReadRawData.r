##------------------------------------------------------------------
## <>
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
figs.dir    <- "/Users/alexstephens/Development/kaggle/axa/figs"
summ.dir    <- "/Users/alexstephens/Development/kaggle/axa/summary"

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd(drivers.dir)

##------------------------------------------------------------------
## Grab the drivers list
##------------------------------------------------------------------
drivers.ls  <- as.character(sort(as.numeric(dir())))
drivers.num <- length(drivers.ls)

##------------------------------------------------------------------
## Loop over each driver and load data into .Rdata files
##------------------------------------------------------------------
for (i in 1:drivers.num) {
    
    routes.ls   <- paste0(as.character(sort(as.numeric(gsub(".csv","",x=dir(paste0(drivers.dir,"/",drivers.ls[i]),pattern=".csv"))))),".csv")
    routes.num  <- length(routes.ls)
    routes.data <- matrix(NA, nrow=routes.num, ncol=6)
    colnames(routes.data) <- c("i","num","minx","maxx","miny","maxy")
    
    ##------------------------------------------------------------------
    ## loop over all routes
    ##------------------------------------------------------------------
    for (j in 1:routes.num) {
        
        ##------------------------------------------------------------------
        ## read the trajectory & compute the velocity
        ##------------------------------------------------------------------
        p   <- read.csv(paste0(drivers.ls[i],"/",routes.ls[j]), header=TRUE)
        v   <- data.frame(vx=diff(p$x),vy=diff(p$y))
        
        ##------------------------------------------------------------------
        ## save the trajectory & velocity as an Rdata file
        ##------------------------------------------------------------------
        save(p, v, file=paste0(drivers.ls[i],"/",gsub(".csv",".Rdata", routes.ls[j])))
        
        ##------------------------------------------------------------------
        ## accumulate summary data
        ##------------------------------------------------------------------
        routes.data[j,] <- c(j, nrow(p), min(p$x), max(p$x), min(p$y), max(p$y))
    }
    save(routes.data, file=paste0(summ.dir,"/",i,"_summary.Rdata") )
    
    
    ##------------------------------------------------------------------
    ## plot the trajectories
    ##------------------------------------------------------------------
    pdf(file=paste0(figs.dir,"/",i,"_summary.pdf"))
    for (k in 1:routes.num) {
        load(paste0(drivers.ls[i],"/",gsub(".csv",".Rdata", routes.ls[k])))
        if (k == 1) {
            plot(p$x,p$y, type="l", col=k,
                xlim=c(min(routes.data[,"minx"]), max(routes.data[,"maxx"])),
                ylim=c(min(routes.data[,"miny"]), max(routes.data[,"maxy"])),
                main=(paste0("driver = ",i)))
        } else {
            lines(p$x,p$y, type="l", col=k)
        }
    }
    dev.off()
    
}

