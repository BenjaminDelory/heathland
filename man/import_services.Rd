\name{import_services}
\alias{import_services}
\title{Import Ecosystem Service Data}
\description{This function creates a list with the data needed for the computation of ecosystem services (groundwater recharge, groundwater quality, carbon removal, cost, and appreciation) for each management practice.}
\usage{
import_services(path=NULL)
}
\arguments{
  \item{path}{A character string specifying the path to the folder containing the text files containing ecosystem service data.}
}
\details{
To run \code{import_services} efficiently, data must be stored in text files. One text file should contain the data associated with one ecosystem service only. By default, the following naming convention is used for the text files: \dQuote{waterrecharge.txt}, \dQuote{waterquality.txt}, \dQuote{carbon.txt}, \dQuote{costs.txt}, and \dQuote{appreciation.txt}. By default (\code{path=NULL}), the function imports the data stored in the \dQuote{extdata} folder of the package.
}
\value{Returns a list. The list is a \code{services} object. Each element of the list contains the data associated with one ecosystem service.
}
\author{Benjamin M. Delory
}
\examples{
data<-import_services()
}