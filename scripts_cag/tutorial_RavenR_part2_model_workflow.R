rm(list=ls());
library(RavenR)

# check tz of reading functions
# trace("forcings.read", edit=TRUE)
# trace("custom.read",edit=TRUE)
# trace("watershed.read",edit=TRUE)
# this should go at the beginning of the function: tz = "GMT"
# this should in all as.POSIXct functions: tz = tz

# Load the RavenR sample data
# ===================================================== 
source("https://raw.githubusercontent.com/calvarezgarreton/RavenR/developer/adapted_fn/adapted_functions.R")

indir <- "/Users/calvarez/Dropbox/GitHub/RavenR/Tutorials/RavenTutorialFiles/Nith/"
outdir <- "/Users/calvarez/Dropbox/GitHub/RavenR/Tutorials/RavenTutorialFiles/Nith/output/"
fileprefix <- "Nith"
if (dir.exists(outdir)==FALSE) {dir.create(outdir)}
setwd(outdir)

# RUN RAVEN
# =====================================================
# writes complete command prompt command
# > Raven.exe [filename] -o [outputdir]
RavenCMD <-paste(indir,"Raven_osx.exe ",indir,fileprefix," -o ",outdir,sep="") 
system(RavenCMD) # this runs raven from the command prompt

ff <- "/Users/calvarez/Dropbox/GitHub/RavenR/Tutorials/RavenTutorialFiles/Nith/output/run1_ForcingFunctions.csv" # replace with your own file
ff_data <- forcings.read.tz(ff, tz="GMT")

# pdf("forcings.pdf") # create a pdf file to direct plot to 
dev.new()
forcings.plot(ff_data$forcings)
# dev.off() #finishes writing plot to .pdf file


data(watershed.data)
mywshd <- watershed.data$watershed.storage
mywshd <- watershed.read.tz("run1_WatershedStorage.csv", tz="GMT")$watershed.storage 
# png("snowpack.png") # create a png file to direct plot to
dev.new()
plot(mywshd$snow)
# dev.off() #finishes writing plot to .png file
