#' Title aggregate collection of keyword data.frame to patient level
#'
#' @param dat_enc_neg data.frame
#'
#' @return data.frame with variables for mrn, n_positive, n_negative, first date, last date, n dates
#' @export
#'
#' @examples
keyword2patient <- function(dat_enc_neg) {
  # for each person/date combination,
  # how many non-negated matches are there?
	posdate <- aggregate(
	  NEGATION_COUNT ~ PAT_MRN + CONTACT_DATE,
	  data = dat_enc_neg,
	  FUN=function(x) sum(x==0))
	names(posdate)[3] <- "n_positive"

	# need to convert CONTACT_DATE to a date
	posdate$CONTACT_DATE <- as.Date(posdate$CONTACT_DATE)

	# if there are any dates with positives:
	if (sum(posdate$n_positive>0)) {
	  # date of first positive mention
		firstposdate <- aggregate(
		  CONTACT_DATE ~ PAT_MRN,
		  data = posdate, subset = n_positive>0,
		  FUN = function(x) min(x))
		names(firstposdate)[2] <- "first_dt"

		# date of last positive mention
		lastposdate  <- aggregate(
		  CONTACT_DATE ~ PAT_MRN,
		  data = posdate, subset = n_positive>0,
		  FUN = function(x) max(x))
		names( lastposdate)[2] <- "last_dt"

		# number of dates with positive mention
		nposdate     <- aggregate(
		  CONTACT_DATE ~ PAT_MRN,
		  data = posdate, subset = n_positive>0,
		  FUN = function(x) length(x))
		names(nposdate)[2] <- "n_dt"

		# merge datasets
		firstlastposdate <- merge(
		  firstposdate, lastposdate, by="PAT_MRN", all=TRUE)
		firstlastnposdate <- merge(
		  firstlastposdate, nposdate, by="PAT_MRN", all=TRUE)
	} else { # No positive mentions, means data.frame with 0 rows
		firstlastnposdate <- structure(list(
		  PAT_MRN = character(0),
		  first_dt = structure(numeric(0), class = "Date"),
		  last_dt = structure(numeric(0), class = "Date"),
		  n_dt = numeric(0)), row.names = integer(0), class = "data.frame")
	}

	# count by person (not person/date)
	posmrn <- aggregate(
	  NEGATION_COUNT ~ PAT_MRN,
	  data = dat_enc_neg,
	  FUN = function(x) sum(x==0))
	names(posmrn)[2] <- "n_positive"

	negmrn <- aggregate(
	  NEGATION_COUNT ~ PAT_MRN,
	  data = dat_enc_neg,
	  FUN = function(x) sum(x> 0))
	names(negmrn)[2] <- "n_negative"

	posnegmrn <- merge(posmrn, negmrn, by = "PAT_MRN", all = TRUE)

	outdf <- merge(
	  firstlastnposdate,
	  posnegmrn,
	  by = "PAT_MRN", all = TRUE)
	outdf$n_dt[is.na(outdf$n_dt)] <- 0 # if missing, then count is 0

	return(invisible(outdf))
}

#' Title aggregate collection of keyword data.frame to patient level
#'
#' @param dat_enc_neg data.frame
#'
#' @return data.frame with variables for mrn token, encounter token, contact date, n_positive, n_negative
#' @export
#'
#' @examples
keyword2encounter <- function(dat_enc_neg) {
	# output includes mrn, csn, date, n_positive, n_negative

	pos <- aggregate(
	  NEGATION_COUNT ~ PAT_MRN + PAT_ENC_CSN_ID,
	  data = dat_enc_neg,
	  FUN = function(x) sum(x==0))
	names(pos)[3] <- "n_positive"

	neg <- aggregate(
	  NEGATION_COUNT ~ PAT_MRN + PAT_ENC_CSN_ID,
	  data = dat_enc_neg,
	  FUN = function(x) sum(x> 0))
	names(neg)[3] <- "n_negative"

	date <- aggregate(
	  CONTACT_DATE ~ PAT_MRN + PAT_ENC_CSN_ID,
	  data = dat_enc_neg,
	  FUN = function(x) min(x)) # should all be the same for a csn
	names(date)[3] <- "CONTACT_DATE"

	posneg <- merge(pos, neg, by = c("PAT_MRN", "PAT_ENC_CSN_ID"))
	posnegdate <- merge(date, posneg, by = c("PAT_MRN", "PAT_ENC_CSN_ID"))

	return(invisible(posnegdate))
}
