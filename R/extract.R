#' Extract matching text from progress notes
#'
#' @param pat regular expression for which to search
#' @param dat_pn data.table or data.frame containing clinic visit notes
#' @param varname_pn name of variable in dat_pn containing the progress note.  Default is "PROGRESS_NOTE".
#'
#' @return data.frame or data.table (invisibly) with seven columns containing all text matching pattern pat, the position of the matches within the progress note and the associated data for that progress note (encounter, mrn, and id tokens; date)
#' @export
#'
#' @examples extract("XLRP", dat_pn)
extract <- function(pat, dat_pn, varname_pn = "PROGRESS_NOTE") {

	# determine the locations of the matches within the notes
	loc <- gregexpr(pat, dat_pn[, varname_pn], ignore.case = TRUE)
	# extract the text of the matches
	# ext is a list (length = nrow(dat_pn))
	# each element is a character vector (length = number of matches)
	ext <- regmatches(dat_pn[, varname_pn], loc)

	# the total number of matches found (might be several per progress note)
	n <- sum(sapply(ext, length))

	# create storage vectors for
	# encounter token associated with progress note
	# position of pat in progress note
	# line (to differentiate multiple matches within a progress note)
	# mrn, id tokens associated with progress note
	# date associated with progress note
	# text that matches the pat
	csn <- pos <- line <- numeric(n)
	id <- mrn <- date <- found <- character(n)

	j <- 0 # j counts up to n
	for (i in seq(nrow(dat_pn))) { # i loops through records in dat_pn
		if (l <- length(ext[[i]])) { # if any matches in record i, then
		  # copy the l instances to the new vectors
			repl <- seq.int(j+1, j+l)
			csn  [repl] <- dat_pn$PAT_ENC_CSN_ID[i]
			line [repl] <- seq.int(l) # 1..l to differentiate
			id   [repl] <- dat_pn$PAT_ID[i]
			mrn  [repl] <- dat_pn$PAT_MRN[i]
			date [repl] <- dat_pn$CONTACT_DATE[i]
			found[repl] <- ext[[i]] # ext[[i]] is a vector of length l
			pos  [repl] <- loc[[i]] # loc[[i]] is a vector of length l
			j <- j + l # advance counter by number of matches in this record
		}
	}

	# create data.frame of results
	dat_ext <- data.frame(
	  PAT_ENC_CSN_ID = csn,
	  PAT_ID = id,
	  PAT_MRN = mrn,
	  CONTACT_DATE = date,
	  MATCHED_TEXT = found,
	  POSITION = pos,
	  LINE = line)

	# output result sorted by PAT_MRN and then CONTACT_DATE and then PAT_ENC_CSN_ID and then character position
	dat_ext <- dat_ext[
	  with(
	    dat_ext,
	    order(PAT_MRN, CONTACT_DATE, PAT_ENC_CSN_ID, POSITION)), ]

	return(invisible(dat_ext))
}

#system.time(dat_wo <- extract(pat="\\bw/o\\b", filebase=NULL))


#' Extract matching text from progress notes; multiple patterns allowed
#'
#' @param pats vector of regular expressions for which to search
#' @param dat_pn data.table or data.frame containing clinic visit notes
#' @param varname_pn name of variable in dat_pn containing the progress note.  Default is "PROGRESS_NOTE".
#'
#' @return data.frame or data.table (invisibly) with seven columns containing all text matching pattern pat, the position of the matches within the progress note and the associated data for that progress note (encounter, mrn, and id tokens; date)
#' @export
#'
#' @examples
#' system.time(dat_ext <- extracts(pats=c("XLRP", "x[ -]*linked[ -]*(RP|retinitis([ -]*pigmentosa)?)"), dat_pn))
#' table(dat_ext$MATCHED_TEXT)

extracts <- function(pats, dat_pn, varname_pn = "PROGRESS_NOTES") {
  # number of patterns
	npat <- length(pats)

	# call extract with each pattern
	dat_exts <- lapply(pats, function(pat) extract(pat, filebase=NULL))

	# combine to single data.frame
	dat_ext <- do.call(rbind, dat_exts)

	# re-sort
	# output result sorted by PAT_MRN and then CONTACT_DATE and then PAT_ENC_CSN_ID and then character position
	dat_ext <- dat_ext[
	  with(
	    dat_ext,
	    order(PAT_MRN, CONTACT_DATE, PAT_ENC_CSN_ID, POSITION)), ]

	return(invisible(dat_ext))
}

#' Extract matching text from progress notes; multiple patterns allowed and exclusion patterns allowed
#'
#' @param yespats vector of regular expressions for which to search
#' @param nopats vector of regular expressions for which to exclude
#' @param dat_pn data.table or data.frame containing clinic visit notes
#' @param varname_pn name of variable in dat_pn containing the progress note.  Default is "PROGRESS_NOTE".
#'
#' @return data.frame or data.table (invisibly) with seven columns containing all text matching pattern pat, the position of the matches within the progress note and the associated data for that progress note (encounter, mrn, and id tokens; date)
#' @export
#'
#' @examples
#'  extracts_with_exceptions(
#'    yespats = c("RP"),
#'    nopats = c("RP2"),
#'    dat_pn)
extracts_with_exceptions <- function(yespats, nopats, dat_pn, varname_pn = "PROGRESS_NOTES") {
	dat_yes <- extracts(yespats, dat_pn = dat_pn, varname_pn = varname_pn)
	dat_no  <- extracts(nopats , dat_pn = dat_pn, varname_pn = varname_pn)

	# vector of TRUE/FALSE
	# for each 'yes' match, is it included in 'no' match?
	# if so, it will be removed
	toexclude <- with(
	  dat_yes,
	  sprintf("%s_%s_%d", PAT_MRN, PAT_ENC_CSN_ID, POSITION)) %in%
		with(
		  dat_no ,
		  sprintf("%s_%s_%d", PAT_MRN, PAT_ENC_CSN_ID, POSITION))

	dat_ext <- dat_yes[!toexclude,]

	return(invisible(dat_ext))
}
