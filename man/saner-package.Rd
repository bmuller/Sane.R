\name{saner-package}
\alias{saner-package}
\docType{package}

\title{
    Sane.R
}

\description{

    A group of functions and classes for making R act more like a 
    general purpose programming language.

}

\details{
  \tabular{ll}{
    Package: \tab Sane.R\cr
    Type: \tab Package\cr
    Version: \tab 0.0\cr
    Date: \tab 2009-09-01\cr
    License: \tab GPL 3+\cr
    LazyLoad: \tab yes\cr
    Depends: \tab methods\cr
  }
}

\author{
Brian Muller

Maintainer: Brian Muller <bmuller at butterfat dot net>
}
\references{

}

\seealso{
  See also \code{\link{collection} }, \code{\link{collection-accessors}} and 
  \code{\link{environment} }
}

\examples{


  col <- collection()
  
  col[] <- c(1,2,3)
  col[] <- c(4,5,6,7,8)
  col[] <- function(x) { print(x) }

  col[1]
  col[2]
  col[3]("hello")

  length(col)

  col <- collection(10, 0)
  col[10] <- "hello"

}


\keyword{ package }
