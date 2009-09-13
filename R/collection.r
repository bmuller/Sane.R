
setClass("collection", representation( env="environment" ))

collection <- function(size=0, default=NA) {
  col <- new("collection", env = new.env(hash=TRUE, parent=emptyenv() ))
  if(size > 0)
    for(index in 1:size)
      collection.set(col, index, default)
  return(col)
}

collection.get <- function(col, key, drop=TRUE) {
  varname <- paste('vars.', key)
  return(get(varname, envir=col@env))
}

collection.set <- function(col, key, value) {
  varname <- paste('vars.', key)  
  assign(varname, value, envir=col@env)
  return(col)
}

valid.key <- function(col, key) {
  if(!is.numeric(key))
    stop("Cannot use non-numeric object as index")
  key <- as.integer(key)
  if(is.na(key))
    stop("Cannot coerce index object into an integer")
  if(key > length(col))
    stop("Index out of bounds")
  return(key)
}

setMethod("[", signature(x="collection", i="ANY", j="missing", drop="missing"),
          function(x, i, j, ..., drop) { return(collection.get(x, i)) }
          )

setReplaceMethod("[", signature(x="collection", i="ANY", j="missing", value="ANY"),
                 function(x, i, ..., value) {
                   if(missing(i)) {
                     index <- as.integer(length(x) + 1)
                     return(collection.set(x, index, value))
                   }
                   i <- valid.key(x, i)
                   return(collection.set(x, i, value))
                 }
                 )

setMethod("length", "collection", function(x) length(x@env))

setMethod("show", "collection", function(object) {
  cat("A 'collection' with a size of", length(object))
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
