\name{collection}

\alias{collection}
\alias{is.collection}

\title{ Heterogeneous array data structure for the R language }

\description{

  Preferred constructor for the \code{\link{collection-class}}.
  
}

\usage{
  collection(size=0, default=NA)

  is.collection(x)

}

\arguments{

  \item{x}{ A collection object. }

  \item{size}{ 
    The initial default size of the collection.
  }

  \item{default}{
    Default value for collection elements if size is greater than 0.
  }
}

\details{

  \code{collection} returns a collection object.  A collection is an array
  that is agnostic in terms of the elements within it.  For instance, a 
  collection may contain an array as the first element, a matrix as the 
  second, and a function as the third.  Collection elements can be 
  retrieved and set by integer index.
  
  Values are restricted only to valid R objects.

  Collections may be accessed via the standard R accessor \code{[}.  See 
  \code{\link{collection-accessors}} for details.
 
  \code{is.collection} returns a boolean value indicating if the argument is
  a collection object.

}

\value{
  For \code{collection}, an object of class collection.   
}

\author{ Brian Muller }
\seealso{ \code{\link{collection-accessors}} }

\examples{

  col <- collection()
  
  # Append some objects to an empty collection
  col[] <- c(1,2,3)
  col[] <- c(4,5,6,7,8)
  col[] <- function(x) { print(x) }

  # Reference those by index
  col[1]
  col[2]
  col[3]("hello")

  length(col)

  col <- collection(10, 0)
  # You can set specific elements as well
  col[10] <- "hello"

}

\keyword{ data }
\keyword{ manip }
