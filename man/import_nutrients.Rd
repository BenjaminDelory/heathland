\name{import_nutrients}
\alias{import_nutrients}
\title{Import Nutrient Data}
\description{This function creates a list with the data needed for the computation of nutrient balances for each management scenario.}
\usage{
import_nutrients(path=NULL)
}
\arguments{
  \item{path}{A character string specifying the path to the folder containing the text files containing nutrient data.}
}
\details{
To run \code{import_nutrients} efficiently, nutrient input and output data must be stored in text files. One text file should contain the nutrient data associated with one management practice only. By default, the following naming convention is used for the text files: \dQuote{grazing.txt},  \dQuote{mowing.txt}, \dQuote{burning.txt}, \dQuote{choppering.txt}, and \dQuote{sodcutting.txt}. By default (\code{path=NULL}), the function imports the data stored in the \dQuote{extdata} folder of the package.
}
\value{Returns a list. The list is a \code{nutrients} object. Each element of the list contains the nutrient data associated with one management practice. Data for five essential nutrients are available (N, P, K, Ca, Mg). Each element of the list contains the following rows:

\bold{Datm}: the annual atmospheric deposition (kg/ha/year).

\bold{Esheep}: the annual nutrient input via sheep excrement (kg/ha/year).

\bold{Lcontrol}: the annual amount of nutrient leached from the control plots (kg/ha/year).

\bold{R}: the nutrient loss due to the removal of biomass and soil (kg/ha).

\bold{Lfirst}: the amount of nutrient leached from the managed plots one year after the management took place (kg/ha/year).

\bold{Dash}: the nutrient deposition coming from ash production (kg/ha/year).
}
\author{Benjamin M. Delory
}
\examples{
data<-import_nutrients()
}