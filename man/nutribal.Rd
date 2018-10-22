\name{nutribal}
\alias{nutribal}
\title{Compute the Nutrient Balance of a Management Scenario}
\description{This function computes the cumulative balance of one essential nutrient for one management scenario.}
\usage{
nutribal(x, data=import_nutrients(), 
         sqa=list(grazing=1, mowing=5, burning=5, choppering=7), 
         nutrient="N", init=0)
}
\arguments{
  \item{x}{A management scenario (see details).}
  \item{data}{A \code{nutrients} object created by \code{\link{import_nutrients}}.}
  \item{sqa}{A list giving the time period until leaching rates reach status quo ante (years) for each management.}
  \item{nutrient}{The nutrient to focus on. Could be either \code{\dQuote{N}}, \code{\dQuote{P}}, \code{\dQuote{K}}, \code{\dQuote{Mg}}, or \code{\dQuote{Ca}}.}
  \item{init}{The nutrient balance at the start of the simulation. Default to zero.}
}
\details{
A management scenario should be given as a numeric vector. The following convention is used: 0 for no management, 1 for grazing, 2 for mowing, 3 for burning, and 4 for choppering. The vector should have a length equal to the number of simulated years (1 management per year).
}
\value{Returns a numeric vector reporting the cumulative nutrient balance (kg/ha).
}
\author{Benjamin M. Delory
}
\seealso{\code{\link{import_nutrients}}.}
\examples{
#Compute the N balance of a heathland that is grazed each year for 30 years
balance<-nutribal(rep(1, 30))
plot(c(0:30), balance, type="l", lwd=2, ylab="Cumulative N balance (kg/ha)",
xlab="Time (years)", bty="l", las=1, col="red")
abline(h=0, lty=2)
}