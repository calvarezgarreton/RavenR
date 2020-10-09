#' Read in Raven WatershedStorage file
#'
#' watershed.read is used to read in the WatershedStorage.csv file produced by
#' the modelling Framework Raven.
#'
#' This function expects a full file path to the WatershedStorage.csv file,
#' then reads in the file using read.csv. The main advantage of this functon is
#' renaming the columns to nicer names and extracting the units into something
#' much easier to read.
#'
#' This function is also built to support the wshd.animate function, which uses
#' the object created here for creating an animation of the watershed storage
#' containers.
#'
#' ff is the full file path of the WatershedStorage.csv file. If the file is
#' located in the current working directory, then simply the name of the file
#' is sufficient.
#'
#' @param ff full file path to the WatershedStorage.csv file
#' @return \item{watershed.storage}{data frame from the file with standardized
#' names} \item{units}{vector corresponding to units of each column}
#' @seealso \code{\link{hyd.read}} for reading in the Hydrographs.csv file
#' \code{\link{watershedmeb.read}} for reading in the
#' WatershedMassEnergyBalance.csv file
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven read.csv watershed storage
#' @examples
#'
#' # locate in RavenR Watershed storage file
#' ff <- system.file("extdata","run1_WatershedStorage.csv", package="RavenR")
#'
#' # create full file path and read in file
#' mywshd <- watershed.read(ff)
#'
#' # check data
#' head(mywshd$watershed.storage)
#'
#' @export watershed.read.tz
watershed.read.tz <- function(ff=NA,tzone="") {
  
  if (missing(ff)) {
    stop("Requires the full file path to the WatershedStorage.csv file")
  }
  
  # test reading and get format, number of columns
  watersheds <- read.csv(ff,header=T,nrows=5)
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(watersheds)-3))
  
  # re-read with specified colClasses
  watersheds <- read.csv(ff,header=T,colClasses = classes,na.strings=c("---",'NA','1.#INF'))
  
  # careful in date-time formats; excel can screw it up if csv is saved over. This works for
  # un untouched Raven output file
  date.time <- as.POSIXct(paste(watersheds$date,watersheds$hour), format="%Y-%m-%d %H:%M:%S",tz=tzone)
  # head(date.time)
  cols <- colnames(watersheds)
  
  # temporary fix while precip column leaves no space between precip and units
  if ("precip.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "precip.mm.day."),"precip..mm.day.")
  }
  if ("rainfall.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "rainfall.mm.day."),"rainfall..mm.day.")
  }
  if ("snowfall.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "snowfall.mm.day."),"snowfall..mm.day.")
  }
  if ("rain.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "rain.mm.day."),"rain..mm.day.")
  }
  if ("snowfall.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "snow.mm.day."),"snow..mm.day.")
  }
  
  # change all "..." to ".." in cols
  newcols <- gsub("\\.\\.\\.","\\.\\.",cols)
  
  # setup units
  units <- matrix(data=NA,nrow=length(cols))
  
  # split the col names into units
  for (i in 4:length(cols)) {
    mysplit <- unlist(strsplit(newcols[i],"\\.\\."))
    
    if (length(mysplit) == 2) {
      units[i] = mysplit[2]
      newcols[i] = mysplit[1]
    }
    #       else if (length(mysplit) == 3) {
    #       units[i] = mysplit[3]
    #       newcols[i] = sprintf("%s_obs",mysplit[1])
    #     }
  }
  
  # add the date time object, replace time date hour bits
  watersheds <- watersheds[,4:ncol(watersheds)]
  newcols <- newcols[4:length(newcols)]
  units <- units[4:nrow(units)]
  watersheds <- xts(order.by=date.time,x=watersheds)
  
  # assign new column names
  colnames(watersheds) <- newcols
  
  # manual correct for units
  # remove trailing "." in unit labels
  #   for (i in 1:length(units)) {
  #     if (substr(units[i], nchar(units[i]), nchar(units[i])) == ".") {
  #       units[i] = substr(units[i],1,nchar(units[i])-1)
  #     }
  #   }
  # temporary correction
  units <- replace(units,which(units == "m3.s."),"m3/s")
  units <- replace(units,which(units == "mm.day."),"mm/day")
  
  return(list("watershed.storage" = watersheds, "units" = units))
}


#' Read Raven Custom Output files
#'
#' custom.read is used to read any Raven custom output file
#'
#' custom.read parses the filename and predicts the file format accordingly, so
#' it is important to use the unmodified file names for this function. The use
#' (or not) of a runname is accounted for.
#'
#' The returned object is a time series object (xts format), which can be used
#' to easily plot the time series data. The otpions of the custom output are
#' included in the rav.obj attributes.
#'
#' @param ff full file path to the custom output file
#' @param no.runname boolean for whether a runName is supplied, important for
#' parsing the filename
#' @return \item{custom.out}{data frame with the custom output data stored as xts
#' object}
#' @seealso \code{\link{customoutput.plot}} for plotting custom output
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven read custom output
#' @examples
#'
#'# find sample rvh file for Nith subwatershed
#'ff <- system.file("extdata","run1_SNOW_Daily_Average_ByHRU.csv", package="RavenR")
#'
#'# extract and plot custom data
#'mycustomdata <- custom.read(ff)
#'plot(mycustomdata[,5],main='Daily Average SNOW - HRU 5')
#'
#' @export custom.read.tz
custom.read.tz <- function(ff=NA, no.runname=F,tzone="") {
  
  if (missing(ff)) {
    stop("Requires the full file path to the Raven custom output file")
  }
  
  if(!file.exists(ff)){ stop("specified file name/path does not exist") }
  
  fname <- unlist(strsplit(unlist(strsplit(ff,".csv"))[1],"/"))[length(unlist(strsplit(ff,"/")))]
  namelist <- unlist(strsplit(fname,"_"))
  
  # JRC: to fix: currently won't be able to handle long variable name with no runname prefix unless
  #              no.runname is specified (i.e., it won't figure it out on its own)
  
  # determine properties from file name
  if (length(namelist) >= 5) {
    if (no.runname==F){
      runname <- namelist[1]
      vv <- paste(namelist[2:(length(namelist)-3)],collapse="_")
    } else {
      runname <-''
      vv <- paste(namelist[1:(length(namelist)-3)],collapse="_")
    }
    time.type <- namelist[length(namelist)-2]
    stat.type <- namelist[length(namelist)-1]
    space.type <- namelist[length(namelist)]
  } else {
    runname=''
    vv <- namelist[1]
    time.type <- namelist[2]
    stat.type <- namelist[3]
    space.type <- namelist[4]
  }
  
  cust.data    <- read.csv(ff,header=F,skip=2,stringsAsFactors=F);
  cust.data    <- cust.data[,1:(ncol(cust.data)-1)] # remove NA column
  cust.headers <- read.csv(ff,header=F,nrows=2,skip=0,stringsAsFactors = F);
  cust.headers <- cust.headers[,1:(ncol(cust.headers)-1)]
  
  ## time property handling
  # obtain time object, trim columns
  # need to update to handle hourly and yearly data *********
  if (time.type == "Continuous" | time.type == "Daily") {
    tt <- as.POSIXct(paste(cust.data[,2]), format="%Y-%m-%d",tz=tzone)
    # date.time <- as.POSIXct(paste(hydrographs$date,hydrographs$hour), format="%Y-%m-%d %H:%M:%S")
    cust.data <- cust.data[,-(1:2)]
    cust.headers <- cust.headers[1,-(1:2)]
    
  } else if (time.type == "Monthly") {
    tt <- as.POSIXct(paste0(cust.data[,2],rep("-1",nrow(cust.data)) ), format="%Y-%m-%d",tz=tzone)
    # tt <- as.yearmon(as.character(cust.data[,2]), "%Y-%m")
    cust.data <- cust.data[,-(1:2)]
    cust.headers <- cust.headers[1,-(1:2)]
    
  } else if (time.type == "Yearly") {
    tt <- as.POSIXct(paste0(cust.data[,2],rep("-12",nrow(cust.data)),rep("-1",nrow(cust.data))), format="%Y-%m-%d",tz=tzone)
    cust.data <- cust.data[,-(1:2)]
    cust.headers <- cust.headers[1,-(1:2)]
    
  } else if (time.type == "WYearly") {
    # graps the first year in the wyear column, attaches month=12 and day=1 to it
    #   adds that y-m-d as the timestamp in the time series
    # can adjust to the second year or any month/day desired
    # to change the year in the split, change the [[1]] to [[2]] in the line below
    yrs <- t(sapply(cust.data[,2],FUN=function(x) strsplit(x,"-")[[1]],USE.NAMES=F)) # split into matrix
    tt <- as.POSIXct(paste0(yrs[,1],rep("-12",nrow(cust.data)),rep("-1",nrow(cust.data))), format="%Y-%m-%d",tz=tzone)
    cust.data <- cust.data[,-(1:2)]
    cust.headers <- cust.headers[1,-(1:2)]
  }
  
  if (NCOL(cust.data) != 1) {
    colnames(cust.data) <- as.matrix(cust.headers)
  }
  
  # create xts custom data object
  dd <- xts(cust.data, order.by=tt)
  
  # adds attributes to xts custom data object
  # replace with xtsAttribute ??
  attr(dd,'runname')<-runname
  attr(dd,'datatype')<-vv
  attr(dd,'time.agg')<-time.type
  attr(dd,'stat.agg')<-stat.type
  attr(dd,'space.agg')<-space.type
  
  return("custom.out"=dd)
}

#' Modified by Camila Alvarez to incorporate the tz as function argument. 08 October 2020
#' Read in Raven ForcingFunctions file
#'
#' forcings.read is used to read in the ForcingFunctions.csv file produced by
#' the modelling Framework Raven.
#'
#' This function expects a full file path to the ForcingFunctions.csv file,
#' then reads in the file using read.csv. The main advantage of this functon is
#' renaming the columns to nicer names and extracting the units into something
#' much easier to read.
#'
#' ff is the full file path of the ForcingFunctions.csv file. If the file is
#' located in the current working directory, then simply the name of the file
#' is sufficient.
#'
#' @param ff full file path to the ForcingFunctions.csv file
#' @return \item{wstor}{data frame from the file with standardized names}
#' \item{units}{vector corresponding to units of each res column}
#' @seealso \code{\link{hyd.read}} for reading in the Hydrographs.csv file
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven read.csv forcing functions
#' @examples
#'
#'# read in sample forcings data
#'ff <- system.file("extdata","run1_ForcingFunctions.csv", package="RavenR")
#'myforcings <- forcings.read(ff)
#'
#' # check data
#' head(myforcings$forcings)
#'
#' @export forcings.read.tz
forcings.read.tz <- function(ff=NA,tzone="") {
  
  if (missing(ff)) {
    stop("Requires the full file path to the ForcingFunctions.csv file")
  }
  
  # test reading and get format, number of columns
  watersheds <- read.csv(ff,header=T,nrows=5)
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(watersheds)-3))
  
  # re-read with specified colClasses
  watersheds <- read.csv(ff,header=T,colClasses = classes,na.strings=c("---",'NA','1.#INF'))
  
  # careful in date-time formats; excel can screw it up if csv is saved over. This works for
  # un untouched Raven output file
  date.time <- as.POSIXct(paste(watersheds$date,watersheds$hour), format="%Y-%m-%d %H:%M:%S",tz=tzone)
  # head(date.time)
  cols <- colnames(watersheds)
  
  # temporary fix while precip column leaves no space between precip and units
  if ("precip.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "precip.mm.day."),"precip..mm.day.")
  }
  if ("rainfall.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "rainfall.mm.day."),"rainfall..mm.day.")
  }
  if ("snowfall.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "snowfall.mm.day."),"snowfall..mm.day.")
  }
  if ("rain.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "rain.mm.day."),"rain..mm.day.")
  }
  if ("snowfall.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "snow.mm.day."),"snow..mm.day.")
  }
  
  # change all "..." to ".." in cols
  newcols <- gsub("\\.\\.\\.","\\.\\.",cols)
  
  # setup units
  units <- matrix(data=NA,nrow=length(cols))
  
  # split the col names into units
  for (i in 4:length(cols)) {
    mysplit <- unlist(strsplit(newcols[i],"\\.\\."))
    
    if (length(mysplit) == 2) {
      units[i] = mysplit[2]
      newcols[i] = mysplit[1]
    }
    #       else if (length(mysplit) == 3) {
    #       units[i] = mysplit[3]
    #       newcols[i] = sprintf("%s_obs",mysplit[1])
    #     }
  }
  
  # add the date time object, replace time date hour bits
  watersheds <- watersheds[,4:ncol(watersheds)]
  newcols <- newcols[4:length(newcols)]
  units <- units[4:nrow(units)]
  watersheds <- xts(order.by=date.time,x=watersheds)
  
  # assign new column names
  colnames(watersheds) <- newcols
  
  # manual correct for units
  # remove trailing "." in unit labels
  #   for (i in 1:length(units)) {
  #     if (substr(units[i], nchar(units[i]), nchar(units[i])) == ".") {
  #       units[i] = substr(units[i],1,nchar(units[i])-1)
  #     }
  #   }
  # temporary correction
  units <- replace(units,which(units == "m3.s."),"m3/s")
  units <- replace(units,which(units == "mm.day."),"mm/day")
  
  return(list("forcings" = watersheds, "units" = units))
}


#' Read in Raven Hydrograph file
#'
#' hyd.read is used to read in the Hydrographs.csv file produced by the
#' modelling Framework Raven.
#'
#' This function expects a full file path to the Hydrographs.csv file, then
#' reads in the file using read.csv. The main advantage of this function is
#' renaming the columns to nicer names and extracting the units into something
#' much easier to read.
#'
#' This function is also built to support the hyd.extract function, which uses
#' the object created here for extracting by reference to the columns named
#' here, for example sub24.
#'
#' ff is the full file path of the Hydrographs.csv file. If the file is located
#' in the current working directory, then simply the name of the file is
#' sufficient.
#'
#' tzone is a string indicating the timezone of the supplied Hydrographs file. The
#' timezone provided is coded into the resulting hyd data frame using the as.POSIXct
#' function. If no timezone is provided, this is left as an empty string, and is
#' determined by the function as the current time zone.
#'
#' @param ff full file path to the Hydrographs.csv file
#' @param tzone string indicating the timezone of the data in ff
#' @return \item{hyd}{data frame from the file with standardized names}
#' @seealso \code{\link{hyd.extract}} for extraction tools related to the
#' hyd.read output file
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven read.csv hydrograph
#' @examples
#'
#' # read in hydrograph sample csv data from RavenR package
#' ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
#'
#' # read in Raven Hydrographs file, store into myhyd
#' myhyd <- hyd.read(ff)
#'
#' # view contents
#' head(myhyd$hyd)
#' myhyd$units
#'
#' @export hyd.read.tz
hyd.read.tz <- function(ff=NA,tzone="") {
  
  if (missing(ff)) {
    stop("Requires the full file path to the Hydrographs.csv file.")
  }
  
  #read hydrograph output
  hydrographs <- read.csv(ff,header=T,nrows=5)
  # careful in date-time formats; excel can screw it up if csv is saved over. This works for
  # an untouched Raven output file
  
  # assumed colClasses structure - mostly numeric except date and hour columns
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(hydrographs)-3))
  
  # re-read with specified colClasses
  hydrographs <- read.csv(ff,header=T,colClasses = classes,na.strings=c("---",'NA'))
  
  # need to fix the hourly model
  date.time <- as.POSIXct(paste(hydrographs$date,hydrographs$hour), format="%Y-%m-%d %H:%M:%S",tz=tzone)
  
  if (length(which(is.na(date.time)))>0){
    print("hyd.read: Error in mapping day/time to POSIXct. Must be timezone without daylight savings")
    return()
  }
  cols <- colnames(hydrographs)
  
  
  # temporary fix while precip column leaves no space between precip and units
  if ("precip.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "precip.mm.day."),"precip..mm.day.")
  }
  
  # change all "..." to ".." in cols
  newcols <- gsub("\\.\\.\\.","\\.\\.",cols)
  
  # setup units and obs_flag
  units <- matrix(data=NA,nrow=length(cols))
  obs_flag <- matrix(data=NA,nrow=length(cols))
  
  # find index where precip column starts
  pcol <- grep("precip*",cols)
  Ncol <- length(cols)
  
  # split the col names into units, observed flags
  for (i in pcol:Ncol) {
    mysplit <- unlist(strsplit(newcols[i],"\\.\\."))
    
    if (length(mysplit) == 2) {
      units[i] = mysplit[2]
      obs_flag[i] = F
      newcols[i] = mysplit[1]
    } else if (length(mysplit) >= 3) {
      if (mysplit[2] == "observed") {
        units[i] = mysplit[3]
        obs_flag[i] = T
        newcols[i] = sprintf("%s_obs",mysplit[1])
      } else if (mysplit[2] == "inflow") {
        units[i] = mysplit[3]
        obs_flag[i] = F
        newcols[i] = sprintf("%s_resinflow",mysplit[1])
      } else if (mysplit[3] == "inflow") {
        units[i] = mysplit[4]
        obs_flag[i] = F
        newcols[i] = sprintf("%s_resinflow",mysplit[1])
      }
    }
  }
  
  # add the date time object, replace time date hour bits
  hydrographs <- hydrographs[,pcol:Ncol]
  newcols <- newcols[pcol:Ncol]
  units <- units[pcol:Ncol]
  obs_flag <- obs_flag[pcol:Ncol]
  hydrographs <- xts(order.by=date.time,x=hydrographs)
  
  # assign new column names
  colnames(hydrographs) <- newcols
  
  # manual correct for units
  # remove trailing "." in unit labels
  # for (i in 1:length(units)) {
  #   if (substr(units[i], nchar(units[i]), nchar(units[i])) == ".") {
  #     units[i] = substr(units[i],1,nchar(units[i])-1)
  #   }
  # }
  
  # temporary correction
  units <- replace(units,which(units == "m3.s."),"m3/s")
  units <- replace(units,which(units == "mm.day."),"mm/day")
  
  return(list("hyd" = hydrographs, "units" = units, "obs.flag" = obs_flag))
}


#' Read in Raven ReservoirStages file
#'
#' res.read is used to read in the ReservoirStages.csv file produced by the
#' modelling Framework Raven.
#'
#' This function expects a full file path to the ReservoirStages.csv file, then
#' reads in the file using read.csv. The main advantage of this function is
#' renaming the columns to nicer names and extracting the units into something
#' much easier to read.
#'
#' This function is also built to support the res.extract function, which uses
#' the object created here for extracting by reference to the columns named
#' here, for example sub24.
#'
#' ff is the full file path of the ReservoirStages.csv file. If the file is
#' located in the current working directory, then simply the name of the file
#' is sufficient.
#'
#' @param ff full file path to the ReservoirStages.csv file
#' @return \item{res}{data frame from the file with standardized names}
#' @seealso \code{\link{res.extract}} for extraction tools related to the
#' res.read output file
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven read.csv reservoir
#' @examples
#' # warning: example not run, sample example for associated files only
#' \dontrun{
#' # create full file path
#' dir <- "C:/temp/model/output.csv")
#' ff <- paste0(dir,"/","run4_ReservoirStages.csv")
#'
#' # read in the Reservoir file
#' myres <- res.read(ff)
#'
#' # view contents
#' head(myres$res)
#' res$units
#' }
#'
#' @export res.read.tz
res.read.tz <- function(ff=NA,tzone="") {
  
  if (missing(ff)) {
    stop("Requires the full file path to the ReservoirStages.csv file.")
  }
  
  #read reservoir output
  reservoirs <- read.csv(ff,header=T,nrows=5)
  # careful in date-time formats; excel can screw it up if csv is saved over. This works for
  # an untouched Raven output file
  
  # assumed colClasses structure - mostly numeric except date and hour columns
  classes <- c(c('numeric','character','character'),rep('numeric',ncol(reservoirs)-3))
  
  # re-read with specified colClasses
  reservoirs <- read.csv(ff,header=T,colClasses = classes,na.strings=c("---",'NA'))
  
  
  # need to fix the hourly model
  date.time <- as.POSIXct(paste(reservoirs$date,reservoirs$hour), format="%Y-%m-%d %H:%M:%S",tz=tzone)
  # head(date.time)
  cols <- colnames(reservoirs)
  
  # temporary fix while precip column leaves no space between precip and units
  if ("precip.mm.day." %in% cols) {
    cols <- replace(cols,which(cols == "precip.mm.day."),"precip..mm.day.")
  }
  
  # change all "..." to ".." in cols
  newcols <- gsub("\\.\\.\\.","\\.\\.",cols)
  
  # setup units and obs_flag
  units <- matrix(data=NA,nrow=length(cols))
  obs_flag <- matrix(data=NA,nrow=length(cols))
  
  # find index where precip column starts
  pcol <- grep("precip*",cols)
  Ncol <- length(cols)
  
  # split the col names into units, observed flags
  for (i in pcol:Ncol) {
    mysplit <- unlist(strsplit(newcols[i],"\\.\\."))
    
    if (length(mysplit) == 2) {
      units[i] = mysplit[2]
      obs_flag[i] = F
      newcols[i] = mysplit[1]
    } else if (length(mysplit) == 3) {
      if (mysplit[2] == "observed") {
        units[i] = mysplit[3]
        obs_flag[i] = T
        newcols[i] = sprintf("%s_obs",mysplit[1])
      } else if (mysplit[2] == "res.inflow") {
        units[i] = mysplit[3]
        obs_flag[i] = F
        newcols[i] = sprintf("%s_inflow",mysplit[1])
      }
    }
  }
  
  # add the date time object, replace time date hour bits
  reservoirs <- reservoirs[,pcol:Ncol]
  newcols <- newcols[pcol:Ncol]
  units <- units[pcol:Ncol]
  obs_flag <- obs_flag[pcol:Ncol]
  reservoirs <- xts(order.by=date.time,x=reservoirs)
  
  # assign new column names
  colnames(reservoirs) <- newcols
  
  # manual correct for units
  # remove trailing "." in unit labels
  # for (i in 1:length(units)) {
  #   if (substr(units[i], nchar(units[i]), nchar(units[i])) == ".") {
  #     units[i] = substr(units[i],1,nchar(units[i])-1)
  #   }
  # }
  
  # temporary correction
  units <- replace(units,which(units == "m3.s."),"m3/s")
  units <- replace(units,which(units == "mm.day."),"mm/day")
  
  return(list("res" = reservoirs, "units" = units, "obs.flag" = obs_flag))
}
