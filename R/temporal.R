as.numeric.chartime <- function(x,format,...)as.numeric(unclass(as.POSIXct(strptime(x,format),tz='GMT')))
as.chartime <- function(x,...)UseMethod('as.chartime')
as.chartime.numeric <- function(x,format,mark=TRUE,...){
	y <- strftime(as.POSIXct(as.numeric(x),tz='GMT',origin='1970-01-01'),format=format,tz='GMT')
	y[is.infinite(x)] <- x[is.infinite(x)]
	z <- rep('',length(y))
	if(mark){
		s <- !is.na(x) & is.finite(x) & x%%60!=0
		z[s] <- '+'
	}
	y <- paste0(y,z)
	y[is.na(x)] <- NA
	y
}
as.time <- function(x,...)UseMethod('as.time')
as.time.numeric <- function(x,...){
	x <- round(x)
	x[is.finite(x)] <- x[is.finite(x)]%%(60*60*24)
	structure(x, class = c('time','timepoint','timeline','numeric'))
}
as.time.character <- function(x,format='%H:%M',...)as.time(as.numeric.chartime(x,format))

as.date <- function(x,...)UseMethod('as.date')
as.date.numeric <- function(x,...){
	x <- as.numeric(x)
	x <- round(x)
	f <- is.finite(x)
	x[f] <- x[f] - x[f]%%(60*60*24)
	structure(x, class = c('date','timepoint','timeline','numeric'))
}
as.date.character <- function(x,format='%Y-%m-%d',...)as.date(as.numeric.chartime(x,format))
as.date.sasdate <-function(x,...)as.date(as.Date(x, origin="1960-01-01",...))
as.datetime <- function(x,...)UseMethod('as.datetime')
as.datetime.numeric <- function(x,...){
	x <- round(x)
	structure(x, class = c('datetime','timepoint','timeline','numeric'))
}
as.datetime.character <- function(x,format='%Y-%m-%dT%H:%M',...)as.datetime(as.numeric.chartime(x,format))
as.datetime.date <- function(x,y=0,...)as.datetime(as.numeric(x)+as.numeric(as.second(y)))
format.time <- function(x,format='%H:%M',mark=TRUE,...)as.chartime(x,format,mark)
format.date <- function(x,format='%Y-%m-%d',mark=TRUE,...)as.chartime(x,format,mark)
format.datetime <- function(x,format='%Y-%m-%dT%H:%M',mark=TRUE,...)as.chartime(x,format,mark)
as.character.timepoint <- function(x,...)format(x,...)
print.timepoint <-function(x,...){
	print(format(x,...),quote=FALSE)
	invisible(x)
}
c.timeline <- function (..., recursive = FALSE){
	args <- list(...)
	oldclass <- class(args[[1]])
	structure(c(unlist(lapply(args, unclass))), class = oldclass)
}
seq.timeline <- function (from, to, by, length.out, along.with, ...){
  if(missing(from))stop('seq.timeline requires "from"')
  #defaults for interval can be set, if neither specified nor implied
  specified <- !missing(by)
  implied <- !missing(to) & (!missing(length.out) | !missing(along.with))
  if (!specified & !implied){
    if (inherits(from, "time")) by = 60 * 60
    if (inherits(from, "date")) by = 60 * 60 * 24
    if (inherits(from, "datetime")) by = 60 * 60 * 24
  }
  if(!missing(to)){
    stopifnot(identical(class(from),class(to)))
    to <- as.numeric(to)
  }
  theClass <- class(from)
  from <- as.numeric(from)
  #if(missing(length.out))length.out=NULL
  #if(missing(along.with))along.with=NULL
  args <- list(from=from)
  if(!missing(to))args <- c(args,list(to=to))
  if(!missing(by))args <- c(args,list(by=by))
  if(!missing(length.out))args <- c(args,list(length.out=length.out))
  if(!missing(along.with))args <- c(args,list(along.with=along.with))
  args=c(args,list(...))
  x <- do.call(seq,args)
  class(x) <- theClass
  x
}
as.time.time <- function(x,...)x
as.date.date <- function(x,...)x
as.datetime.datetime <- function(x,...)x
rep.timeline <- function (x, ...) structure(rep(as.numeric(x),...),class=class(x))


`[.timeline` <- function (x, ..., drop = TRUE)structure(NextMethod(.Generic), class = oldClass(x))
`[[.timeline` <- function (x, ..., drop = TRUE)structure(NextMethod(.Generic), class = oldClass(x))

`[<-.timepoint` <- function (x, ..., value){
    if (!(length(value)))return(x)
    if(all(is.na(value)))value <- as.numeric(value)
    if(inherits(x,'time'))value <- as.time(value)
    if(inherits(x,'date'))value <- as.date(value)
    if(inherits(x,'datetime'))value <- as.datetime(value)
    cl <- oldClass(x)
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    x
}
xtfrm.timepoint <- function(x,...)as.numeric(x)
as.date.Date <- function(x,...)as.date(round(as.numeric(x))*86400)
as.datetime.POSIXct <- function(x,...)as.datetime(round(as.numeric(x)))
as.datetime.POSIXlt <- function(x,...)as.datetime(as.POSIXct(x))
as.time.times <- function(x,...)as.time(as.numeric(x)*86400)
as.date.dates <- function(x,...)as.date(as.numeric(x)*86400)
as.datetime.chron <- function(x,...)as.datetime(as.numeric(x)*86400)
unique.timepoint <- function(x, incomparables=FALSE,...)unique.numeric_version(x,incomparables,...)
Summary.timepoint <- function (..., na.rm=FALSE)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok)
        stop(.Generic, " not defined for timepoint objects")
    val <- NextMethod(.Generic)
    class(val) <- oldClass(list(...)[[1L]])
    val
}
toSAS.datetime <- function(x, format="", format.info=NULL){
	diff <- as.date('1970-01-01') - as.date('1960-01-01')
	x <- as.second(x)
	x <- x + diff
	attr(x, "SASformat") <- format

	x
}
toSAS.date <- function(x, format="", format.info=NULL){
	diff <- as.day(as.date('1970-01-01') - as.date('1960-01-01'))
	x <- as.day(as.second(x))
	x <- x + diff
	attr(x, "SASformat") <- format

	x
}
toSAS.time <- function(x, format="", format.info=NULL){
	x <- as.numeric(x)
	attr(x, "SASformat") <- format
	x
}
#as.vector.timepoint <- function (x, mode = "any"){
#    if (mode == "any") x
#    else as.vector(unclass(x), mode)
#}
#aperm.timepoint <- aperm.table

