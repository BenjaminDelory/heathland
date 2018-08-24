\name{import_constraints}
\alias{import_constraints}
\title{Import Scenario Constraints}
\description{This function creates a matrix containing the constraints needed for the computation of management scenarios.}
\usage{
import_constraints(path=NULL)
}
\arguments{
  \item{path}{A character string specifying the path to the folder containing the text file containing the constraints.}
}
\details{
To run \code{import_constraints} efficiently, the constraints must be stored in a text file (\dQuote{constraints.txt}). By default (\code{path=NULL}), the function imports the constraints stored in the \dQuote{extdata} folder of the package.
}
\value{Returns a matrix. The matrix is a \code{constraints} object containing the amount of time (in years) that one has to wait to perform a new management practice (column) when another management (row) was already performed at a site.
}
\author{Benjamin M. Delory
}
\examples{
constraints<-import_constraints()
}