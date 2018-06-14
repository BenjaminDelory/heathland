\name{ecoser}
\alias{ecoser}
\title{Compute Ecosystem Services after Management}
\description{This function computes various ecosystem services in the years following the management of the experimental plots.}
\usage{
ecoser(data, management, freq=NULL, time=NULL, n)
}
\arguments{
  \item{data}{A services object created by \code{\link{import_services}}.}
  \item{management}{A character string specifying the management practice. Can be either \dQuote{grazing}, \dQuote{mowing}, \dQuote{burning}, \dQuote{choppering}, or \dQuote{sodcutting}.}
  \item{freq}{The time interval between two management practices (years).}
  \item{time}{The time period until leaching rates reach status quo ante (years).}
  \item{n}{The number of years to simulate.}
}
\details{
By default, we assume that there is no difference between control and managed plots for the grazing scenario.
}
\value{Returns a matrix. For the groundwater recharge, the returned matrix contains the cumulative increase in groundwater recharge relative to control plots.
}
\references{
}
\author{Benjamin M. Delory
}
\examples{
data<-import_services()

grazing<-ecoser(data, management="grazing", n=30)
mowing<-ecoser(data, management="mowing", freq=10, time=5, n=30)
burning<-ecoser(data, management="burning", freq=15, time=5, n=30)
choppering<-ecoser(data, management="choppering", freq=15, time=10, n=30)
sodcutting<-ecoser(data, management="sodcutting", freq=30, time=15, n=30)
}