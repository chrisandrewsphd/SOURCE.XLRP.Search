% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/negationprecautionhistory.R
\name{findstringboundaries}
\alias{findstringboundaries}
\title{Find sentence boundaries}
\usage{
findstringboundaries(dat_pn, varname_pn = "PROGRESS_NOTE", verbose = 0)
}
\arguments{
\item{dat_pn}{data.frame ENC_VISIT}

\item{varname_pn}{variable name for PROGRESS_NOTE}

\item{verbose}{Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.}
}
\value{
data.frame with locations of sentence boundaries
}
\description{
Find sentence boundaries
}