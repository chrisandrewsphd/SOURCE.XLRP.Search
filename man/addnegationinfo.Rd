% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/negationprecautionhistory.R
\name{addnegationinfo}
\alias{addnegationinfo}
\title{Add negation information to each instance of found keyword}
\usage{
addnegationinfo(
  dat_key,
  dat_neg,
  sent_bound,
  dat_pn,
  varname_pn = "PROGRESS_NOTE",
  varname_enc = "PAT_ENC_CSN_ID",
  verbose = 0
)
}
\arguments{
\item{dat_key}{data.frame containing instances of keywords}

\item{dat_neg}{data.frame containing instances of negations}

\item{sent_bound}{data.frame containing sentence boundaries}

\item{dat_pn}{data.frame ENC_VISIT containing progress notes}

\item{varname_pn}{name of PROGRESS_NOTE in dat_pn}

\item{varname_enc}{variable name of encounter variable}

\item{verbose}{Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.}
}
\value{
data.frame
}
\description{
Add negation information to each instance of found keyword
}
\examples{
# addnegationinfo()
}
