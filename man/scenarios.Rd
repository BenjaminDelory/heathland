\name{scenarios}
\alias{scenarios}
\title{Compute Management Scenarios}
\description{This function computes all possible management scenarios based on existing constraints.}
\usage{
scenarios(management=c("grazing", "mowing", "burning", "choppering", "sodcutting"), 
          constraints=import_constraints(), n=20, show.progress=TRUE, filename=NULL)
}
\arguments{
  \item{management}{A character vector listing the allowed management practices.}
  \item{constraints}{A \code{constraints} object created by \code{\link{import_constraints}}.}
  \item{n}{The number of years to simulate.}
  \item{show.progress}{A logical value. If TRUE, a progress bar is shown.}
  \item{filename}{The name of the text file to export the results (with the extension: .txt).}
}
\details{
If filename is not \code{NULL}, the function will export a text file containing the computed scenarios. The text file will be stored in the working directory. This function uses the \code{\link{write.big.matrix}} function of the \code{bigmemory} package to create the text file.
}
\value{Returns a \code{big.matrix} object. Each line is a management scenario. The following convention is used: 0 for no management, 1 for grazing, 2 for mowing, 3 for burning, 4 for choppering, and 5 for sodcutting.
}
\author{Benjamin M. Delory
}
\seealso{\code{\link{import_constraints}}, \code{\link{write.big.matrix}}, \code{\link{getwd}}.}
\examples{
#Compute all management scenarios combining light management practices
#Number of simulated years: 20
#Do not export the results as a text file
sc<-scenarios(management=c("grazing", "mowing", "burning"), n=20)
}