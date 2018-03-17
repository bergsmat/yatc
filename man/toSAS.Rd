\name{toSAS.datetime}
\alias{toSAS.datetime}
\alias{toSAS.date}
\alias{toSAS.time}
\title{Convert Timepoint to SAS Format}
\description{Convert timepoint objects to SAS format for writing XPT files}
\usage{
\method{toSAS}{datetime}(x, format="", format.info=NULL)
\method{toSAS}{date}(x, format="", format.info=NULL)
\method{toSAS}{time}(x, format="", format.info=NULL)
}
\arguments{
  \item{x}{subclass of timepoint}
  \item{format}{SAS format name}
  \item{format.info}{Table of SAS format information}
}
\details{\pkg{SASxport} defines \code{toSAS} and calls it on each column
when writing XPT files. The \code{datetime} method returns the integer number of seconds
since the start of 1960-01-01. The \code{date} method returns the integer number of days
since 1960-01-01.The \code{time} method returns the number of seconds
since midnight.
}
\value{numeric}
\author{Tim Bergsma}
\seealso{	\itemize{
			\item \code{\link{timepoint}}
	}
}
\examples{
if(require(SASxport)) toSAS(as.datetime('1960-01-01T00:00')) # 0
if(require(SASxport)) toSAS(as.date('1960-01-02')) # 1
if(require(SASxport)) toSAS(as.time('00:01')) # 60

}
\keyword{manip}

