## Next few functions modified from GSEABase package AAA.R and utils.R
## http://www.bioconductor.org/packages/bioc/html/GSEABase.html

## Add names to character vector x.  Elements of x without names get
## a name matching the element.
.nameAll <- function(x) {
  if (!is.character(x))
    stop("argument 'x' must be a character vector")
  if (length(names(x)))
    names(x) <- ifelse(nchar(names(x)) == 0, x, names(x))
  else
    names(x) <- x
  x
}

## Add getter methods for the given slots.  The slots can have names,
## in which case the name because the method name.  The parameter objfunc
## allows you to specify a function that will return the proper object given
## the original.  For instance, if you want a getter for a class that actually
## gets something from one of the slots in the class, then you can return that slot
## in an objfunc and the getter will be applied to that rather than the original
## class instance.
.getters <- function(klass, slots, where=topenv(parent.frame()), objfunc=NULL, ...) {
  slots <- .nameAll(slots)
  if(is.null(objfunc))
    objfunc <- function(object) object
  
  for (i in seq_along(slots)) {
    eval(substitute({
      if (!isGeneric(GENERIC))
        setGeneric(GENERIC,
                   function(object) standardGeneric(GENERIC),
                   where=WHERE)

      setMethod(GENERIC,
                signature=signature(object=CLASS),
                function(object) slot(OBJFUNC(object), SLOT),
                where=WHERE)
    }, list(CLASS = klass,
            GENERIC = names(slots)[[i]],
            SLOT = slots[[i]],
            OBJFUNC = objfunc,
            WHERE = where)))
  }
}



.setters <- function(klass, slots, where=topenv(parent.frame()), ...) {
  slots <- .nameAll(slots)
  for (i in seq(along=slots)) {
    eval(substitute({
      if (!isGeneric(SETTER))
        setGeneric(SETTER, function(object, value)
                   standardGeneric(SETTER),
                   where = WHERE)
      if (getSlots(CLASS)[[SLOT]] == "ScalarCharacter")
        setReplaceMethod(GENERIC,
                         signature=signature(
                           object=CLASS,
                           value="character"),
                         function(object, value) {
                           slot(object, SLOT) <- mkScalar(value)
                           validObject(object)
                           object
                         },
                         where = WHERE)
      else
        setReplaceMethod(GENERIC,
                         signature=signature(
                           object=CLASS,
                           value=getSlots(CLASS)[[SLOT]]),
                         function(object, value) {
                           slot(object, SLOT) <- value
                           validObject(object)
                           object
                         },
                         where = WHERE)
    }, list(CLASS=klass,
            GENERIC=names(slots)[[i]],
            SETTER=paste(names(slots)[[i]], "<-", sep=""),
            SLOT=slots[[i]],
            WHERE=where)))
  }
}

## These function have been added to make R sane. They're by bmuller.

## For each thing in obj, fun FUN with it as the argument.  If obj
## is a list, then FUN will be passed the name and the value of thing.
## If it is a list and there are no names, then NA will be passed as
## the name.  To exit from the foreach loop, return FALSE.
foreach <- function(obj, FUN, ...) {
  FUN <- match.fun(FUN)
  func = switch(class(obj),
    list=function() {
      if(length(formals(FUN)) < 2)
        stop("The FUN argument to foreach must take two arguments when obj is a list")
      names <- names(obj)
      if(length(names) == 0)
        names <- doTimes(length(obj), NA)
      for(i in seq_along(obj)) {
        r <- FUN(names[i], obj[[i]], ...)
        if(!is.null(r) && !is.na(r) && r == FALSE)
          break
      }
    },
    function() {
      for(i in seq_along(obj)) {
        r <- FUN(obj[i], ...)
        if(!is.null(r) && !is.na(r) && r == FALSE)        
          break
      }
    })
  func()
}

## Much like Ruby's <int>.times() method, except returns vector of
## results.  It can be used like:
## doTimes(10, NA)
## doTimes(10, function() NA )
## doTimes(10, function(index) print(index))
doTimes <- function(count, f) {
  # if f isn't a function, make one that returns f
  if(!is.function(f))
    FUN <- function() f
  else
    FUN <- f
  result <- c()
  if(length(formals(FUN)) == 1)
    for(i in 1:count)
      result <- append(result, FUN(i))
  else
    for(i in 1:count)
      result <- append(result, FUN())
  return(result)
}

## This function accepts some number of character arguments that are all
## names of variables within the current environment.  It then sets
## those variables in the parent's parent's parent environment
## (configurable by the frame parameter).  You can pass a frame
## value of "sapply" in the case of a function within an sapply
## > x <- 1
## > lapply(c(1,2,3), function(y) { x <- 1; closure("x") })
## > print(x)
## 4
##
## > x <- 1
## > sapply(c(1,2,3), function(y) { x <- 1; closure("x", frame="sapply") })
## > print(x)
## 4
closure <- function(..., frame=3) {
  parent <- parent.frame()
  frame <- ifelse(frame=="sapply", 4, frame)
  grandparent <- sys.frame(sys.nframe()-frame)
  for(name in c(...))
    assign(name, get(name, envir=parent), envir=grandparent)
}
