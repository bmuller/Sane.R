
setClass("collection", representation( env="environment" ))

collection <- function() {
  col <- new("collection", env = new.env(hash=TRUE, parent=emptyenv() ))
  return(col)
}

collection.get <- function(col, key, drop=TRUE) {
  varname <- paste('vars.', key)
  return(get(varname, envir=col@env))
}

collection.set <- function(col, key, value, drop=TRUE) {
  varname <- paste('vars.', key)
  assign(varname, value, envir=col@env)
  return(invisible(NULL))
}
  
setMethod("[", signature(x="collection", i="ANY", j="missing", drop="missing"),
          function(x, i, j, ..., drop) { return(collection.get(x, i)) }
          )

setReplaceMethod("[", signature(x="collection", i="NULL", j="missing", value="ANY"),
          function(x, i, ..., value) {
            if(missing(i))
              return(collection.set(x, 0, value)) 
            return(collection.set(x, i, value)) }
          )

setMethod("length", "collection", function(x) length(x@env))


is.collection <- function(x) is( x, "collection" )


x <- collection()
x[] <- c(1,2,3)
