\name{nutribal}
\alias{nutribal}
\title{Compute the Nutrient Balance of a Management Scenario}
\description{This function computes the balance of one essential nutrient for one management scenario.}
\usage{
nutribal(x, data=import_nutrients(), sqa=list(grazing=1, mowing=5, burning=5, choppering=7),
        bioleft=list(mowing=5133.63, burning=1888.96), nutrient="N", Datm=NULL,
        gompertz=list(a=20148.38, b=8.48362, c=6.647557), sheep.pressure=0.5, 
        plot.biomass=FALSE, cumulative=TRUE, ...)
}
\arguments{
  \item{x}{A management scenario (see details).}
  \item{data}{A \code{nutrients} object created by \code{\link{import_nutrients}}.}
  \item{sqa}{A list giving the time period until leaching rates reach status quo ante (years) for each management.}
  \item{bioleft}{A list giving the standing plant biomass (in kg/ha) left after mowing and burning.}
  \item{nutrient}{The nutrient to focus on. Could be either \code{\dQuote{N}}, \code{\dQuote{P}}, \code{\dQuote{K}}, \code{\dQuote{Mg}}, or \code{\dQuote{Ca}}.}
  \item{Datm}{The atmospheric deposition for the target nutrient (in kg/ha/year).}
  \item{gompertz}{The parameters of the Gompertz model simulating plant biomass production over time.}
  \item{sheep.pressure}{Numeric value comprised between 0 and 1. A sheep pressure of 0 means that no sheep is present. A sheep pressure of 1 means that the sheep density equals 1.1 sheep/ha when a grazing management occurs.}
  \item{plot.biomass}{Logical value. Should the evolution of the standing plant biomass be plotted? TRUE is yes, FALSE is no.}
  \item{cumulative}{A logical value. Should the cumulative nutrient balance be calculated? TRUE is yes, FALSE is no.}
}
\details{
A management scenario should be given as a numeric vector. The following convention is used: 0 for no management, 1 for grazing, 2 for mowing, 3 for burning, and 4 for choppering. The vector should have a length equal to the number of simulated years (1 management per year) and must start with choppering.
}
\value{Returns a numeric vector reporting the (cumulative) nutrient balance (kg/ha).
}
\author{Benjamin M. Delory
}
\seealso{\code{\link{import_nutrients}}.}
\examples{
#Compute the N balance
balance<-nutribal(c(4,0,0,0,0,0,0,0,3,0,0))
plot(c(0:10), balance, type="l", lwd=2, ylab="Cumulative N balance (kg/ha)",
xlab="Time (years)", bty="l", las=1, col="red")
abline(h=0, lty=2)
}