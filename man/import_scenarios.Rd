\name{import_scenarios}
\alias{import_scenarios}
\title{Import Management Scenarios}
\description{This function creates a \code{big.matrix} object containing the management scenarios stored in a text file.}
\usage{
import_scenarios(path)
}
\arguments{
  \item{path}{A character string specifying the path to the text file.}
}
\details{
This function uses the \code{\link{read.big.matrix}} function of the \code{bigmemory} package to create a \code{big.matrix} object. Ideally, the text file containing management scenarios should be created with the \code{scenarios} function of this package (\code{filename} argument).
}
\value{Returns a \code{big.matrix} object. Each line is a management scenario. The following convention is used: 0 for no management, 1 for grazing, 2 for mowing, 3 for burning, 4 for choppering, and 5 for sodcutting.
}
\references{
}
\author{Benjamin M. Delory
}
\seealso{\code{\link{read.big.matrix}}, \code{\link{getwd}}.}
\examples{
path<-system.file("extdata", package="heathland")
sc<-import_scenarios(paste(path, "scenarios.txt", sep="/"))
}