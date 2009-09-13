\name{hash-class}
\docType{class}
\alias{hash-class}

\title{Class "hash" }
\description{ 

  Implements a S4 hash class in R similar to hashes / associatesd arrays / 
  dictionaries in languages.  As much as possible these constructs have 
  been given an R type interface.
	
  Hash construction is flexible and takes several syntaxes.   

  Accessors are defined for \code{\$}, \code{[} and \code{[[}.
    
}

\section{Slots}{
  \describe{ 
     \item{\code{env}:}{ 
       Object of class \code{"environment"}.  This is the hashed environment 
       used for key-value storage. }
  }
}

\section{Extends}{
  None by design.
}


\section{Methods}{
  \describe{
    \item{[<-}{\code{signature(x = "hash", i = "ANY", j = "missing")}: ... }
    \item{[}{\code{signature(x = "hash", i = "ANY", j = "missing", drop = "missing")}
: ... }
    \item{[[<-}{\code{signature(x = "hash", i = "ANY", j = "missing")}: ... }
    \item{[[}{\code{signature(x = "hash", i = "ANY", j = "missing")}: ... }
    \item{\$<-}{\code{signature(x = "hash")}: ... }
    \item{\$}{\code{signature(x = "hash")}: ... }
    \item{clear}{\code{signature(x = "hash")}: ... }
    \item{del}{\code{signature(x = "ANY", hash = "hash")}: ... }
    \item{has.key}{\code{signature(key = "ANY", hash = "hash")}: ... }
    \item{keys}{\code{signature(hash = "hash")}: ... }
    \item{length}{\code{signature(x = "hash")}: ... }
    \item{values}{\code{signature(x = "hash")}: ... }
  }
}

\references{ 

	http://en.wikipedia.org/wiki/Hash_table

	http://en.wikipedia.org/wiki/Associative_array

}
\author{ Christopher Brown }

\note{ 

  For lists of any appreciable length hashes provide much quicker 
  element access than named vectors.

  Objects of class \code{hash} do not release memory with a call to 
  \code{rm}.  \code{clear} must be called before \code{rm} to properly
  release the memory.

}

\seealso{
	See also as \code{\link{environment}}. 
}

\examples{
  showClass("hash")
}

\keyword{classes}