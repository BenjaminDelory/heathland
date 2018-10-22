\name{ecoser}
\alias{ecoser}
\title{Compute Ecosystem Services of a Management Scenario}
\description{This function computes the cumulative balance of one ecosystem service (groundwater recharge, groundwater quality, carbon removal, cost, and appreciation) for one management scenario.}
\usage{
ecoser(x, data=import_services(), 
       sqa=list(grazing=1, mowing=5, burning=5, choppering=7), 
       service="waterrecharge", init=0)
}
\arguments{
  \item{x}{A management scenario (see details).}
  \item{data}{A \code{services} object created by \code{\link{import_services}}.}
  \item{sqa}{A list giving the time period until leaching rates reach status quo ante (years) for each management.}
  \item{service}{The ecosystem service to focus on. Could be either \code{\dQuote{waterrecharge}}, \code{\dQuote{waterquality}}, \code{\dQuote{carbon}}, \code{\dQuote{cost}}, \code{\dQuote{appreciation}}.}
  \item{init}{The water recharge/quality balance at the start of the simulation. Default to zero.}
}
\details{
A management scenario should be given as a numeric vector. The following convention is used: 0 for no management, 1 for grazing, 2 for mowing, 3 for burning, and 4 for choppering. The vector should have a length equal to the number of simulated years (1 management per year).

For the groundwater recharge, the groundwater quality, and the appreciation, we assume that there is no difference between control and managed plots for the grazing scenario.
}
\value{Returns a numeric vector reporting the cumulative values of a given ecosystem service. Values are given in mm/year for groundwater recharge, kg N/ha for groundwater quality, kg/ha for carbon removal, and euros/ha for cost. Appreciation is adimensional.
}
\references{
}
\author{Benjamin M. Delory
}
\seealso{\code{\link{import_services}}.}
\examples{
#Compute the groundwater recharge of a heathland that is grazed each year for 30 years
balance<-ecoser(rep(1, 30))
plot(c(0:30), balance, type="l", lwd=2, ylab="Groundwater recharge (mm/year)",
xlab="Time (years)", bty="l", las=1, col="red")
abline(h=0, lty=2)
}