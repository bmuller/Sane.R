\name{collection-accessors}

\alias{collection-accessors}

\alias{[,collection,ANY,missing,missing-method}
\alias{[<-,collection,ANY,missing,ANY-method}
\alias{[<-,collection,missing,missing,ANY-method}

\title{Accessor methods for the collection class.}

\description{
  R style accesors for the \code{\link{collection-class}}.
}


\details{
  These are the collection accessor methods. They closely follow an R style.
}

\value{
  
  [ returns a collection element

}

\author{ Brian Muller }

\seealso{ \code{\link{collection}} }

\examples{

  col <- collection()
  
  # Append some objects to an empty collection
  col[] <- c(1,2,3)
  col[] <- c(4,5,6,7,8)
  col[] <- function(x) { print(x) }

  # Reference those by index
  col[1]
  col[2]
  col[3](``hello'')

  length(col)

  col <- collection(10, 0)
  # You can set specific elements as well
  col[10] <- ``hello''

}

\keyword{ methods }
\keyword{ data }
\keyword{ manip }
