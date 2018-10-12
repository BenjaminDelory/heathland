\name{nuts}
\alias{nuts}
\title{Compute the Nutrient Balance of Management Scenarios}
\description{This function computes the cumulative balance of one essential nutrient for many possible management scenarios.}
\usage{
nuts(scenarios, data=import_nutrients(), constraints=import_constraints(), 
     sqa=list(grazing=1, mowing=5, burning=5, choppering=7),
     nutrient="N", init=0)
}
\arguments{
  \item{scenarios}{A \code{big.matrix} object created by \code{\link{scenarios}} or \code{\link{import_scenarios}}.}
  \item{data}{A \code{nutrients} object created by \code{\link{import_nutrients}}.}
  \item{constraints}{A \code{constraints} object created by \code{\link{import_constraints}}.}
  \item{sqa}{A list giving the time period until leaching rates reach status quo ante (years) for each management.}
  \item{nutrient}{The nutrient to focus on. Could be either \code{\dQuote{N}}, \code{\dQuote{P}}, \code{\dQuote{K}}, \code{\dQuote{Mg}}, or \code{\dQuote{Ca}}.}
  \item{init}{The nutrient balance at the start of the simulation. Default to zero.}
}
\details{
The following convention is used: 0 for no management, 1 for grazing, 2 for mowing, 3 for burning, and 4 for choppering.
}
\value{Returns a \code{big.matrix} object containing the cumulative nutrient balance for each scenario (row) each year (column).
}
\author{Benjamin M. Delory
}
\seealso{\code{\link{import_nutrients}}, \code{\link{import_constraints}}, \code{\link{scenarios}}, \code{\link{import_scenarios}}.}
\examples{
#Create management scenarios
sc<-scenarios(management=c("none", grazing", "mowing", "burning", "choppering"), n=50)

#Compute the N balance for each scenario
balance<-nuts(sc)

#Plot results for the last year
boxplot(balance[,ncol(balance)], ylab="Cumulative N balance (kg/ha)", las=1)
abline(h=0, lty=2)
}