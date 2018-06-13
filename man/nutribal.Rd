\name{nutribal}
\alias{nutribal}
\title{Compute Nutrient Balances after Management}
\description{This function computes the nutrient balances in the years following the management of the experimental plots.}
\usage{
nutribal(data, management, freq=NULL, time=NULL, n)
}
\arguments{
  \item{data}{A nutrient object created by \code{\link{importnutri}}.}
  \item{management}{A character string specifying the management practice. Can be either \dQuote{none} (no management), \dQuote{grazing}, \dQuote{mowing}, \dQuote{burning}, \dQuote{choppering}, or \dQuote{sodcutting}.}
  \item{freq}{The management frequency (years).}
  \item{time}{The time period until leaching rates reach status quo ante (years).}
  \item{n}{The number of years to simulate.}
}
\details{
By default, \code{freq=1} and \code{time=1} for grazing.
}
\value{Returns a matrix reporting the cumulative nutrient balances.
}
\references{
}
\author{Benjamin M. Delory
}
\examples{
data<-importnutri()

none<-nutribal(data, management="none", n=30)
grazing<-nutribal(data, management="grazing", n=30)
mowing<-nutribal(data, management="mowing", freq=10, time=5, n=30)
burning<-nutribal(data, management="burning", freq=15, time=5, n=30)
choppering<-nutribal(data, management="choppering", freq=15, time=10, n=30)
sodcutting<-nutribal(data, management="sodcutting", freq=30, time=15, n=30)
}