# main script to run other
rm(list=ls()); a=getSrcDirectory(function(x) {x}) ; setwd(a)

# only first time
# library(devtools) 
# devtools::install_github("rchlumsk/RavenR")

# Load the RavenR library from the console and view its contents with the following commands:
library(RavenR)
ls("package:RavenR") 
install.packages('httr')
library(httr)
git_path = 'https://github.com/calvarezgarreton/RavenR/tree/developer/adapted_fn'
req <- GET(git_path)
stop_for_status(req)
filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
grep("Matteo/literature/", filelist, value = TRUE, fixed = TRUE)
# [1] "Matteo/literature/Subsetting.pdf"     
# [2] "Matteo/literature/datatable-intro.pdf"

source('https://raw.github.com/TonyLadson/BaseflowSeparation_LyneHollick/master/BFI.R')

# path to data
# read in hydrograph sample csv data from RavenR package
ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
ff <- system.file("extdata","run1_ForcingFunctions.csv",package="RavenR") 
ff <- "/Users/calvarez/Dropbox/RavenR/inst/extdata/run1_ForcingFunctions.csv" # replace with your own file

# The read functions have been modified in order to avoid problemas with time zone differences.
# 06 octiber 2020 - C. Alvarez
fn_path  = "/Users/calvarez/Dropbox/RavenR/RavenR_adapted"
files.sources = paste0(fn_path,'/',list.files(fn_path))
sapply(files.sources, source)

ff_data <- forcings.read.tz(ff, tz="GMT")
head(ff_data$forcings[,1:6])
dev.new()
forcings.plot(ff_data$forcings)


ff <- system.file("extdata","run1_Hydrographs.csv",package="RavenR") 
# ff <- "mydirectory/Hydrographs.csv" # replace with your own file 
# hy <- hyd.read(ff, tz="GMT")
hy <- hyd.read.tz(ff, tz="GMT")
head(hy$hyd)

flow36 <- hyd.extract("Sub36",hy) 
precip <- hyd.extract("precip",hy)$sim

dev.new()
plot(lubridate::date(flow36$sim),flow36$sim,col='red',type='l') 
lines(lubridate::date(flow36$obs),flow36$obs,col='black')

dev.new()
hyd.plot(sim=flow36$sim, obs=flow36$obs, precip=precip)

dev.new()
flow.spaghetti(flow36$sim)
flowdurcurve.plot(flow36$sim)
cum.plot.flow(flow36$sim, obs=flow36$obs)
annual.volume(flow36$sim, flow36$obs)
monthly.vbias(flow36$sim, obs=flow36$obs)
hyd.dyGraphs(hy, basins="Sub36")

# Raw sample data
shpfilename <- system.file("extdata","Nith_shapefile_sample.shp",package="RavenR")
# Custom Output data from Raven for Nith basin
cust.data <- custom.read.tz(system.file("extdata","run1_PRECIP_Daily_Average_BySubbasin.csv", package="RavenR"),tz="GMT")
subIDcol <- 'subID' # attriute in shapefile with subbasin IDs 
plot.date <- "2003-03-30" # date for which to plot custom data
# function call
SBMap.plot(shpfilename,subIDcol,plot.date,cust.data)


leg.title <- 'Legend \nPrecip. (mm)' 
colour1 <- "white"
colour2 <- "cyan"
num.classes <- 8
plot.title <- 'Daily Average Precipitation (mm/d)'
# create an updated plot
SBMap.plot(shpfilename,subIDcol,plot.date,cust.data,plot.title=plot.title, colour1 = colour1, colour2=colour2,leg.title = leg.title, num.classes=num.classes)


# cust.data <- custom.read.tz(system.file("extdata","run1_PRECIP_Daily_Maximum_BySubbasin.csv", package="RavenR"),tz="GMT")
# leg.title <- 'Legend \nDaily Max Precip (mm)' 
# olour1 <- "white"
# colour2 <- "blue"
# num.classes <- 5
# plot.title <- 'Daily Max Precip. (mm)' 
# plot.daterange <- '2003-05-01/2003-06-30'
# gif.filename='Nith_precip_May2003_June2003.gif' 
# gif.speed <- 0.5
# cleanup <- FALSE
# SBMap.animate(shpfilename,subIDcol,plot.daterange,cust.data,plot.title=plot.title, colour1 = colour1, colour2=colour2,
#               leg.title = leg.title, normalize=T, num.classes=num.classes, gif.filename=gif.filename,
#               gif.speed=gif.speed, cleanup=cleanup
# )