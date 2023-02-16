\name{dat.long}
\alias{dat.long.ped}
\alias{dat.long.mat}
\docType{data}
\title{dat.long.ped and dat.long.mat}
\description{
Each row of dat.long.mat contains one maternal immune biomarker measurement. \cr
Each row of dat.long.ped contains one pediatric immune biomarker measurement. \cr
dat.long.ped and dat.long.mat do not have the same columns since some columns are mothers-specific and some are infants-specific. To combine, use dplyr::bind_rows. \cr
Assay info is stored in three columns: assay, time, assay.value, which are described below. All other columns are defined in dat.wide.
}
\format{
  \describe{
    \item{\code{pair.id}}{a character vector}
    \item{\code{assay}}{a character}
    \item{\code{time}}{a character vector}
    \item{\code{assay.value}}{a numeric}
    \item{\code{assay.log10value}}{a numeric}
    \item{...}{}
  }
}
\details{
LLOQ: 400 for EIA, 12 for PCA, 13 for RSVA, and 8 for RSVB. \cr

}
\examples{
    aggregate(assay.value~assay+time, dat.long.ped, median)
    aggregate(assay.value~assay+time, dat.long.mat, median)
}

