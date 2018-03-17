\name{timepoint}
\alias{timepoint}
\alias{as.time}
\alias{as.time.character}
\alias{as.time.numeric}
\alias{as.time.time}
\alias{as.date.character}
\alias{as.date}
\alias{as.date.numeric}
\alias{as.date.date}
\alias{as.date.sasdate}
\alias{as.datetime}
\alias{as.datetime.character}
\alias{as.datetime.numeric}
\alias{as.datetime.date}
\alias{as.datetime.datetime}
\alias{format.time}
\alias{format.date}
\alias{format.datetime}
\alias{xtfrm.timepoint}
\alias{as.date.Date}
\alias{as.date.dates}
\alias{as.datetime.POSIXct}
\alias{as.datetime.POSIXlt}
\alias{as.datetime.chron}
\alias{as.time.times}
\alias{unique.timepoint}
\alias{Summary.timepoint}
\alias{datetime}
\alias{date}
\alias{time}
\alias{timeline}
\alias{+.timeline}
\alias{-.timeline}

\title{Temporal Classes with Selective Defaults}
\description{
  \code{timepoint} is an abstract superclass of \code{time}, \code{date},
  and \code{datetime}.  These latter are convenience classes that store timepoint
  information as seconds since the start of 1970-01-01.  They rely on \code{POSIXlt} and
  \code{POSIXct}, giving full access to the format constructs of \code{\link{strftime}}.
  However, the concepts of \sQuote{origin} and \sQuote{timezone} are deconstructed (fixed to
  1970-01-01 and GMT).  Default formats are suitably chosen for inputs
  (\code{as.character} methods) and outputs (\code{format} methods) and may be overridden.
  By default, \code{format} will append a \sQuote{+} symbol to timepoints with dangling seconds
  (not multiples of 60): seconds are not displayed by default but still operate
  (perhaps dangerously) in comparisons.
}
\usage{
as.time(x, ...)
\method{as.time}{character}(x, format = '\%H:\%M',...)
\method{as.time}{numeric}(x,...)
\method{as.time}{time}(x, ...)
\method{as.time}{times}(x, ...)
as.date(x, ...)
\method{as.date}{character}(x, format = '\%Y-\%m-\%d',...)
\method{as.date}{numeric}(x,...)
\method{as.date}{Date}(x,...)
\method{as.date}{date}(x,...)
\method{as.date}{dates}(x,...)
as.datetime(x, ...)
\method{as.datetime}{character}(x, format = '\%Y-\%m-\%dT\%H:\%M',...)
\method{as.datetime}{numeric}(x,...)
\method{as.datetime}{date}(x, y = 0,...)
\method{as.datetime}{datetime}(x, ...)
\method{as.datetime}{POSIXct}(x, ...)
\method{as.datetime}{POSIXlt}(x, ...)
\method{as.datetime}{chron}(x, ...)
\method{format}{time}(x, format = '\%H:\%M', mark=TRUE,...)
\method{format}{date}(x, format = '\%Y-\%m-\%d', mark=TRUE,...)
\method{format}{datetime}(x, format = '\%Y-\%m-\%dT\%H:\%M', mark=TRUE,...)
\method{unique}{timepoint}(x, incomparables=FALSE,...)
\method{Summary}{timepoint}(..., na.rm=FALSE)
\method{xtfrm}{timepoint}(x,...)
}

\arguments{
  \item{x}{character time as per format, numeric seconds since 1970-01-01, or timepoint subclass}
  \item{\dots}{other arguments, usually ignored}
  \item{y}{optional time for constructing datetime from date: anything coercible with \code{as.second()}}
  \item{format}{character, as per \code{\link{strftime}}}
  \item{mark}{boolean: mark times with dangling seconds using \sQuote{+}}
  \item{incomparables}{passed to \code{unique}}
  \item{na.rm}{passed to \code{Summary}}
 }
\details{
  Creating a timepoint object with these methods ultimately calls one of the
  \code{.numeric} methods, each of which round their first argument to zero
  places.  This means that all comparisons are based on whole numbers, and
  therefore not subject to rounding errors.

  Seconds that are not multiples of 60 can be stored in time and datetime
  objects, but will not be displayed by default (see above).  date can only
  store numbers of seconds that correspond to midnight.  To add time, explicitly
  create an datetime object using \code{as.datetime.date}.

  The timepoint classes are all subclasses of numeric, so numeric operations are generally available.

  The timepoint classes support \code{NA}, \code{Inf}, \code{-Inf}, \code{as.data.frame}, \code{seq}, subset, element
  selection, element assignment, and interconversion.


  The timepoint classes are also subclasses \code{timeline}, which exists to support addition and subtraction of durations and timepoints.  See examples.
  \itemize{
  	  \item You cannot add two timepoints.
  	  \item You cannot subtract a timepoint from a non-timepoint.
  	  \item For the difference of two timepoints, the arguments and result are coerced with \code{as.second}.
  	  \item When one argument is a timepoint, the other is coerced using \code{as.second}, and the result is the timepoint class.
  	  \item For two durations, the second value is coerced to the class of the first, with a message, if necessary.
  	  \item If only one argument is a duration, the other is coerced to that class.
  }
}
\value{
  \item{format}{character}
  \item{as.time}{object with class \code{c('time','timepoint','numeric')}}
  \item{as.date}{object with class \code{c('date','timepoint','numeric')}}
  \item{as.datetime}{object with class \code{c('datetime','timepoint','numeric')}}
}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{duration}}
		\item \code{\link{c.timeline}}
	}
}
\examples{
#numeric to timepoint
as.time(0)
# 00:00
as.time(1)
# 00:00+
as.time(-1)
# 23:59+
as.time(60)
# 00:01
as.time(-60)
# 23:59
as.time(86400)
# 00:00
as.time(86460)
# 00:01
as.date(0)
# 1970-01-01
as.date(1)
# 1970-01-01
as.date(-1)
# 1969-12-31
as.date(-86400)
# 1969-12-31
as.date(-86401)
# 1969-12-30
as.datetime(0)
# 1970-01-01T00:00
as.datetime(60)
# 1970-01-01T00:01
as.datetime(61)
# 1970-01-01T00:01+
as.datetime(-1)
# 1969-12-31T23:59+

#character to timepoint
as.time('00:00')
# 00:00
as.time('23:59')
# 23:59
as.time('23:59:00')
# 23:59
as.time('23:59:01')
# 23:59
as.time('23:59:01',format='\%H:\%M:\%S')
# 23:59+
as.time('24:00')
# 00:00
as.date('1970-01-02')
# 1970-01-02
as.date('01/02/1970',format='\%m/\%d/\%Y')
# 1970-01-02
as.datetime('01/02/1970 12:30',format='\%m/\%d/\%Y \%H:\%M')
# 1970-01-02 12:30
as.datetime('01/02/1970 12:30:15',format='\%m/\%d/\%Y \%H:\%M:\%S')
# 1970-01-02 12:30+

#timepoint to numeric
as.numeric(as.time(0))
# 0
as.numeric(as.time(1))
# 1
as.numeric(as.time(-1))
# 86399
as.numeric(as.time(60))
# 60
as.numeric(as.time(-60))
# 86340
as.numeric(as.time(86400))
# 0
as.numeric(as.time(86460))
# 60
as.numeric(as.date(0))
# 0
as.numeric(as.date(1))
# 0
as.numeric(as.date(-1))
# -86400
as.numeric(as.date(-86400))
# -86400
as.numeric(as.date(-86401))
# -172800
as.numeric(as.datetime(0))
# 0
as.numeric(as.datetime(60))
# 60
as.numeric(as.datetime(61))
# 61
as.numeric(as.datetime(-1))
# -1
as.numeric(as.time('00:00'))
# 0
as.numeric(as.time('23:59'))
# 86340
as.numeric(as.time('23:59:00'))
# 86340
as.numeric(as.time('23:59:01'))
# 86340
as.numeric(as.time('23:59:01',format='\%H:\%M:\%S'))
# 86341
as.numeric(as.time('24:00'))
# 0
as.numeric(as.date('1970-01-02'))
# 86400
as.numeric(as.date('01/02/1970',format='\%m/\%d/\%Y'))
# 86400
as.numeric(as.datetime('01/02/1970 12:30',format='\%m/\%d/\%Y \%H:\%M'))
# 131400
as.numeric(as.datetime('01/02/1970 12:30:15',format='\%m/\%d/\%Y \%H:\%M:\%S'))
# 131415

#timepoint to character
as.character(as.time(0))
# '00:00'
as.character(as.date(0))
# '1970-01-01'
as.character(as.datetime(0))
# '1970-01-01T00:00'

#non-default printout
format(as.time(30000),format='\%H')
# '08'
format(as.date('1970-01-01'),format='\%d\%b\%y')
# '01Jan70'
format(as.datetime('1970-01-02T23:30'),format='[\%d/\%m/\%y \%H:\%M:\%S]')
# '[02/01/70 23:30:00]'
format(as.time(1))
# '00:00+'
format(as.time(1),mark=FALSE)
# '00:00'

#sequence
seq(from=as.time('8:00'),to=as.time('12:00'))
# 08:00 09:00 10:00 11:00 12:00
seq(from=as.date('2008-01-01'),to=as.date('2008-01-04'))
# 2008-01-01 2008-01-02 2008-01-03 2008-01-04
seq(from=as.datetime('2008-01-01T12:00'),to=as.datetime('2008-01-04T12:30'))
# 2008-01-01 12:00 2008-01-02 12:00 2008-01-03 12:00 2008-01-04 12:00

#interconversion
as.time(as.date('2008-10-14'))
# 00:00
as.time(as.datetime('2008-10-14T08:00'))
# 08:00
as.date(as.time('23:59'))
# 1970-01-01
as.date(as.datetime('2008-10-14T08:00'))
# 2008-10-14
as.datetime(as.time(6000000))
# 1970-01-01T10:40
as.datetime(as.date('2008-10-14'))
# 2008-10-14 00:00
as.datetime(as.date('2008-10-14'),y=as.time('00:30'))
# 2008-10-14 00:30

#intercoversion from other systems
as.date(as.Date('1970-01-01'))
# 1970-01-01
as.datetime(as.POSIXct('1970-01-01',tz='GMT'))
# 1970-01-01T00:00
as.datetime(as.POSIXlt('1970-01-01',tz='GMT'))
# 1970-01-01T00:00
if(require(chron)) as.time(times('12:30:00'))
# 12:30
as.date(dates('01/01/1970'))
# 1970-01-01
if(require(chron))as.datetime(chron(dates='01/01/1970',times='12:30:00'))
# 1970-01-01T12:30
as.date.sasdate(0)
# 1960-01-01
as.time(as.numeric(NA))
# <NA>

#infinity
as.time(Inf)
# Inf
as.date(Inf)
# Inf
as.datetime(Inf)
# Inf
as.time(-Inf)
# -Inf
as.datetime(Inf) + (Inf)
# Inf
as.datetime(Inf) + (-Inf)
# <NA>

#comparison
as.time('08:00') < as.time('09:00')
# TRUE
as.date('1970-01-01') > as.date('2008-01-01')
# FALSE
as.datetime('1970-01-01 08:00') > as.date('1970-01-01')
# TRUE

#summary
max(as.date(c('1970-01-01','1980-01-01','1975-01-01')))
# 1980-01-01

#operations
as.datetime(0) + as.second(60)
# 1970-01-01T00:01
as.second(60) + as.datetime(0)
# 1970-01-01T00:01
try(as.datetime(60) + as.datetime(60))
# Error in `+.timeline`(as.datetime(60), as.datetime(60)) :
#   addition is undefined for two timepoints
as.datetime(0) + 60
# 1970-01-01 00:01
60 + as.datetime(0)
# 1970-01-01T00:01
as.minute(1) + as.datetime(0)
# 1970-01-01T00:01
as.datetime(0) - as.second(60)
# 1969-12-31T23:59
as.datetime(0) - 60
# 1969-12-31T23:59
as.datetime(60) - as.datetime(0)
# 60
try(as.second(60) - as.datetime(60))
# Error in `-.timeline`(as.second(60), as.datetime(60)) :
#   subtracting a timepoint from non-timepoint is undefined
try(60 - as.datetime(60))
# Error in `-.timeline`(as.second(60), as.datetime(60)) :
#   subtracting a timepoint from non-timepoint is undefined
as.second(10) * 6
# 60
as.datetime(0) * 2 # meaningless, but not prevented
# 1970-01-01T00:00

#unary operations
-as.time(1)
# 23:59+
+as.time(1)
# 00:00+

#sorting
sort(unique(as.time(c(180,120,60))))
# 00:01 00:02 00:03
}
\keyword{manip}
