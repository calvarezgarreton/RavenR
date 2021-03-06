% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forcings.plot.R
\name{forcings.plot}
\alias{forcings.plot}
\title{Plots summary of watershed forcing functions}
\usage{
forcings.plot(forcings, prd = NULL)
}
\arguments{
\item{forcings}{forcings attribute from forcings.read function}

\item{prd}{(optional) time period over which the plots are generated}
}
\description{
forcings.plot generates a set of 5 plots (precip,temperature,PET,radiation,
and potential melt), which summarize the watershed-averaged forcings.
}
\details{
This function creates multiple plots from a ForcingFunctions.csv file
structure generating using RavenR's forcings.read function
}
\examples{

# read in sample forcings data
data("forcing.data")
fdata <- forcing.data$forcings

# plot forcings data
forcings.plot(fdata)

plot subset of forcing data for 2002-2003 water year
prd = "2002-10-01/2003-09-30"
forcings.plot(fdata,prd)

}
\seealso{
\code{\link{forcings.read}} for the function used to read in the
forcings function data

See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
Craig's research page} for software downloads, including the
\href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
}
\keyword{Raven}
\keyword{diagnostics}
\keyword{forcing}
\keyword{plot}
