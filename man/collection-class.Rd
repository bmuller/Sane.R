\name{collection-class}
\docType{class}
\alias{collection-class}

\title{Class "collection" }
\description{ 

  Implements a S4 collection class in R that is a heterogeneous, dynamic array that is
  agnostic in terms of element types and grows as needed.

  It is, at it's core, an associative array that uses integers as the key values.
	
  Accessors are defined for \code{[}.
    
}

\section{Slots}{
  \describe{ 
     \item{\code{env}:}{ 
       Object of class \code{"environment"}.  This is the environment 
       used for value storage. }
  }
}

\section{Extends}{
  None by design.
}


\section{Methods}{
  \describe{
    \item{[<-}{\code{signature(x=''collection'', i=''ANY'', j=''missing'', value=''ANY'')}: ... }
    \item{[}{\code{signature(x=''collection'', i=''ANY'', j=''missing'', drop=''missing'')}
: ... }
    \item{length}{\code{signature(x = "collection")}: ... }
    \item{values}{\code{signature(x = "collection")}: ... }
    \item{clear}{\code{signature(x = "collection")}: ... }
  }
}

\references{ 

	http://en.wikipedia.org/wiki/Associative_array

}
\author{ Brian Muller }

\note{ 

  Objects of class \code{collection} do not release memory with a call to 
  \code{rm}.  \code{clear} must be called before \code{rm} to properly
  release the memory.

}

\seealso{
	See also as \code{\link{environment}}. 
}

\examples{
  showClass("collection")
}

\keyword{classes}