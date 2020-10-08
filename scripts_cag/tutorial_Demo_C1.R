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
indir  <- "/Applications/RavenR/Tutorials/CalibrationDemos/Demo_C1/model_cag/"
outdir <- "/Applications/RavenR/Tutorials/CalibrationDemos/Demo_C1/model_cag/output/"
fileprefix <- "Irondequoit"
if (dir.exists(outdir)==FALSE) {dir.create(outdir)}
setwd(outdir)

# RUN RAVEN
# =====================================================
# writes complete command prompt command
# > Raven.exe [filename] -o [outputdir]
RavenCMD <-paste(indir,"Raven_v3_MacOS.exe ",indir,fileprefix," -o ",outdir,sep="") 
system(RavenCMD) # this runs raven from the command prompt

ff <- paste0(outdir,fileprefix,"_ForcingFunctions.csv") # replace with your own file
ff_data <- forcings.read(ff)

# pdf("forcings.pdf") # create a pdf file to direct plot to 
dev.new()
forcings.plot(ff_data$forcings)
# dev.off() #finishes writing plot to .pdf file


data(watershed.data)
mywshd <- watershed.data$watershed.storage
mywshd <- RavenR::watershed.read(paste0(outdir,fileprefix,"_WatershedStorage.csv"))$watershed.storage 
# png("snowpack.png") # create a png file to direct plot to
dev.new()
plot(mywshd$snow)
# dev.off() #finishes writing plot to .png file
