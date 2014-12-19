##


data.dir    <- "/Users/alexstephens/Development/kaggle/axa/data/drivers"
setwd(data.dir)


##
drivers.list <- as.character(sort(as.numeric(dir())))

files.list  <- dir(drivers.list[1])

tmp <- read.csv(file=paste0(drivers.list[1],"/",files.list[2]), header=TRUE)

## in driver 1, similar (rotated) routes
#> tmp <- read.csv(file=paste0(drivers.list[1],"/",files.list[11]), header=TRUE); plot(tmp$x, tmp$y, type="l")
#> tmp <- read.csv(file=paste0(drivers.list[1],"/",files.list[199]), header=TRUE); plot(tmp$x, tmp$y, type="l")


tmp <- read.csv(file=paste0(drivers.list[1],"/",files.list[199]), header=TRUE)
tmp2 <- read.csv(file=paste0(drivers.list[1],"/",files.list[11]), header=TRUE)

tmp$vx <- c(0, diff(tmp$x))
tmp$vy <- c(0, diff(tmp$y))
tmp$ax <- c(0, 0, diff(diff(tmp$x)))
tmp$ay <- c(0, 0, diff(diff(tmp$y)))


tmp2$vx <- c(0, diff(tmp2$x))
tmp2$vy <- c(0, diff(tmp2$y))
tmp2$ax <- c(0, 0, diff(diff(tmp2$x)))
tmp2$ay <- c(0, 0, diff(diff(tmp2$y)))

