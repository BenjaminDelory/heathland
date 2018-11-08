\name{nuts}
\alias{nuts}
\title{Compute the Nutrient Balance and Ecosystem Services of Management Scenarios}
\description{This function computes the cumulative balance of one essential nutrient (N, P, K, Mg, or Ca) or one ecosystem service (groundwater recharge, groundwater quality, carbon removal, cost, or appreciation) for many possible management scenarios.}
\usage{
nuts(scenarios, data=import_nutrients(), 
     sqa=list(grazing=1, mowing=5, burning=5, choppering=7),
     nutrient="N", service="waterrecharge", init=0)
}
\arguments{
  \item{scenarios}{A \code{big.matrix} object created by \code{\link{scenarios}} or \code{\link{import_scenarios}}.}
  \item{data}{A \code{nutrients} object created by \code{\link{import_nutrients}} or a \code{services} object created by \code{\link{import_services}}.}
  \item{sqa}{A list giving the time period until leaching rates reach status quo ante (years) for each management.}
  \item{nutrient}{The nutrient to focus on. Only used if data is a nutrients object. Could be either \code{\dQuote{N}}, \code{\dQuote{P}}, \code{\dQuote{K}}, \code{\dQuote{Mg}}, or \code{\dQuote{Ca}}.}
  \item{service}{The ecosystem service to focus on. Only used if data is a services object. Could be either \code{\dQuote{waterrecharge}}, \code{\dQuote{waterquality}}, \code{\dQuote{carbon}}, \code{\dQuote{cost}}, or \code{\dQuote{appreciation}}.}
  \item{init}{The nutrient/groundwater recharge/groundwater quality balance at the start of the simulation. Default to zero.}
}
\details{
The following convention is used: 0 for no management, 1 for grazing, 2 for mowing, 3 for burning, and 4 for choppering.
}
\value{Returns a \code{big.matrix} object containing the cumulative nutrient or ecosystem service balance for each scenario (row) each year (column). Values are given in kg/ha for the nutrient balance, mm/year for groundwater recharge, kg N/ha for groundwater quality, kg/ha for carbon removal, and euros/ha for cost. Appreciation is adimensional.
}
\author{Benjamin M. Delory
}
\seealso{\code{\link{import_nutrients}}, \code{\link{import_services}}, \code{\link{scenarios}}, \code{\link{import_scenarios}}.}
\examples{
#Create management scenarios
sc<-scenarios(management=c("none", "grazing", "mowing", "burning", "choppering"), n=50)

#Compute the N balance for each scenario
balance<-nuts(sc)

#Plot the results
boxplot(balance[,ncol(balance)], ylab="Cumulative N balance (kg/ha)", las=1)
abline(h=0, lty=2)
}