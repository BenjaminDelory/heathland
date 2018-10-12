\name{scenarios}
\alias{scenarios}
\title{Compute Management Scenarios}
\description{This function computes all possible management scenarios based on management frequency distributions and existing constraints.}
\usage{
scenarios(management=c("none", grazing", "mowing", "burning", "choppering"), 
          constraints=import_constraints(), n=100, filename=NULL)
}
\arguments{
  \item{management}{A character vector listing the allowed management practices.}
  \item{constraints}{A \code{constraints} object created by \code{\link{import_constraints}}.}
  \item{n}{The number of years to simulate.}
  \item{filename}{The name of the text file to export the results (with the extension: .txt).}
}
\details{
If filename is not \code{NULL}, the function will export a text file containing the computed scenarios. The text file will be stored in the working directory. This function uses the \code{\link{write.big.matrix}} function of the \code{bigmemory} package to create the text file.

For each possible management frequency distribution, the function computes one random scenario that simultaneously meets the frequency of each management and the constraints. 
}
\value{Returns a \code{big.matrix} object. Each line is a management scenario. The following convention is used: 0 for no management, 1 for grazing, 2 for mowing, 3 for burning, and 4 for choppering.
}
\author{Benjamin M. Delory
}
\seealso{\code{\link{import_constraints}}, \code{\link{write.big.matrix}}, \code{\link{getwd}}.}
\examples{
#Compute management scenarios
#Number of simulated years: 50
#Do not export the results as a text file
sc<-scenarios1(management=c("none", grazing", "mowing", "burning"), n=50)
}