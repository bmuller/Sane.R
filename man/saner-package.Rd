\name{saner-package}
\alias{saner-package}
\docType{package}

\title{
    Hash/associative array/dictionary data structure for the R language.
}

\description{

    This S4 class is designed to provide a hash-like data structure in a  
    native R style and provides the necessary methods for all general 
    methods for hash operations.

}

\details{
  \tabular{ll}{
    Package: \tab hash\cr
    Type: \tab Package\cr
    Version: \tab 0.40\cr
    Date: \tab 2009-09-04\cr
    License: \tab GPL 2+\cr
    LazyLoad: \tab yes\cr
    Depends: \tab methods\cr
  }
}

\author{
Christopher Brown

Maintainer: Christopher Brown <cbrown -at- opendatagroup -dot- com>
}
\references{
    http://www.mail-archive.com/r-help@r-project.org/msg37637.html

    http://www.mail-archive.com/r-help@r-project.org/msg37650.html

    http://tolstoy.newcastle.edu.au/R/help/05/12/index.html\#18192
}

\seealso{
  See also \code{\link{hash} }, \code{\link{hash-accessors}} and 
  \code{\link{environment} }
}

\examples{

  h <- hash( keys=letters, values=1:26 )
  h <- hash( letters, 1:26 )

  h$a # 1

  h$foo <- "bar"
  h[ "foo" ] 
  h[[ "foo" ]]

  clear(h)
  rm(h)

}


\keyword{ package }
