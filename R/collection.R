
setClass("collection", representation( env="environment" ))

collection <- function(size=0, default=NA) {
  col <- new("collection", env = new.env(hash=TRUE, parent=emptyenv() ))
  if(size > 0)
    for(index in 1:size)
      collection.set(col, index, default)
  return(col)
}

collection.get <- function(col, key, drop=TRUE) {
  key <- valid.key(col, key)
  varname <- paste('vars', key, sep=".")
  return(get(varname, envir=col@env))
}

collection.set <- function(col, key, value, append=FALSE) {
  key <- valid.key(col, key, append)
  varname <- paste('vars', key, sep=".")  
  assign(varname, value, envir=col@env)
  return(col)
}

valid.key <- function(col, key, append=FALSE) {
  if(!is.numeric(key))
    stop("Cannot use non-numeric object as index")
  key <- as.integer(key)
  if(is.na(key))
    stop("Cannot coerce index object into an integer")

  if(key == 0)
    stop(paste("Index", key, "is out of bounds (0 is not a valid index)"))
  else if(append && key > length(col)+1)
    stop(paste("Index", key, "is out of bounds"))
  else if(!append && key > length(col))
    stop(paste("Index", key, "is out of bounds"))
  return(key)
}

setMethod("[", signature(x="collection", i="ANY", j="missing", drop="missing"),
          function(x, i, j, ..., drop) { return(collection.get(x, i)) }
          )

setReplaceMethod("[", signature(x="collection", i="ANY", j="missing", value="ANY"),
                 function(x, i, ..., value) {
                   if(missing(i)) {
                     index <- as.integer(length(x) + 1)
                     return(collection.set(x, index, value, TRUE))
                   }
                   return(collection.set(x, i, value))
                 }
                 )

setMethod("length", "collection", function(x) length(x@env))

setMethod("show", "collection", function(object) {
  cat("A 'collection' with a size of", length(object))
})

setGeneric("clear", function(x) standardGeneric("clear"))
setMethod("clear", "collection", function(x) {
  rm(list=paste("vars", 1:length(x), sep="."), envir=x@env)
})

is.collection <- function(x) is( x, "collection" )

collection.map <- function(f) {

}

collection.filter <- function(f) {

}

collection.reduce <- function(f, initial=0) {

}

setGeneric("values", function(x, ...) standardGeneric("values"))
setMethod("values", "collection",
          function(x, ...) {
                    sapply(1:length(x), function(i) { collection.get(x,i) }, ... )
                  }
          ) 

