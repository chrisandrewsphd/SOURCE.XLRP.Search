#' Extract matching text from progress notes
#'
#' @param pat regular expression for which to search
#' @param dat_pn data.table or data.frame containing clinic visit notes
#' @param varname_pn name of variable in dat_pn containing the progress note.  Default is "PROGRESS_NOTE".
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#'
#' @return data.frame or data.table (invisibly) with seven columns containing all text matching pattern pat, the position of the matches within the progress note and the associated data for that progress note (encounter, mrn, and id tokens; date)
#' @export
#'
#' @examples
#' dat_pn <- data.frame(
#'   PAT_MRN = c("A", "B"),
#'   PAT_ID = c("ZA", "ZB"),
#'   CONTACT_DATE = c("2023-03-01", "2020-03-01"),
#'   PAT_ENC_CSN_ID = c("A1", "B1"),
#'   PROGRESS_NOTE = c("XLRP uh oh", "No XLRP. Good day."))
#' system.time(dat_ext <- extract(
#'   pat = "XLRP",
#'   dat_pn))
#' table(dat_ext$MATCHED_TEXT)
extract <- function(
    pat,
    dat_pn,
    varname_pn = "PROGRESS_NOTE",
    verbose = 0) {

  if (verbose > 0) cat("Pattern to search:", pat, "\n")

  stime1 <- system.time({
    # determine the locations of the matches within the notes
    loc <- gregexpr(pat, dat_pn[, varname_pn], ignore.case = TRUE)
    # extract the text of the matches
    # ext is a list (length = nrow(dat_pn))
    # each element is a character vector (length = number of matches)
    ext <- regmatches(dat_pn[, varname_pn], loc)
  })

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

	stime2 <- system.time({
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
	})

	stime <- stime1 + stime2

	if (verbose > 0) {
	  cat("How long to process:", stime[1:3], "\n")
	  cat(nrow(dat_ext), "rows created.\n")

	  if (verbose > 1) {
	    cat("Terms matched:\n")
	    print(table(dat_ext$MATCHED_TEXT))

	    if (verbose > 2) print(utils::head(dat_ext))
	  }
	}

	return(invisible(dat_ext))
}

#system.time(dat_wo <- extract(pat="\\bw/o\\b", filebase=NULL))


#' Extract matching text from progress notes; multiple patterns allowed
#'
#' @param pats vector of regular expressions for which to search
#' @param dat_pn data.table or data.frame containing clinic visit notes
#' @param varname_pn name of variable in dat_pn containing the progress note.  Default is "PROGRESS_NOTE".
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#'
#' @return data.frame or data.table (invisibly) with seven columns containing all text matching pattern pat, the position of the matches within the progress note and the associated data for that progress note (encounter, mrn, and id tokens; date)
#' @export
#'
#' @examples
#' dat_pn <- data.frame(
#'   PAT_MRN = c("A", "B"),
#'   PAT_ID = c("ZA", "ZB"),
#'   CONTACT_DATE = c("2023-03-01", "2020-03-01"),
#'   PAT_ENC_CSN_ID = c("A1", "B1"),
#'   PROGRESS_NOTE = c("XLRP uh oh", "No XLRP. Good day."))
#' system.time(dat_ext <- extracts(
#'   pats = c(
#'     "XLRP",
#'     "x[ -]*linked[ -]*(RP|retinitis([ -]*pigmentosa)?)"),
#'   dat_pn))
#' table(dat_ext$MATCHED_TEXT)

extracts <- function(
    pats,
    dat_pn,
    varname_pn = "PROGRESS_NOTE",
    verbose = 0) {
  # number of patterns
	npat <- length(pats)

	if (verbose > 0) {
	  cat(npat, " patterns to find.\n")
	  cat(pats, "\n")
	}

	stime <- system.time({
	  # call extract with each pattern
	  dat_exts <- lapply(
	    pats,
	    function(pat)
	      extract(
	        pat,
	        dat_pn = dat_pn,
	        varname_pn = varname_pn,
	        verbose = verbose))

	  # combine to single data.frame
	  dat_ext <- do.call(rbind, dat_exts)

	  # re-sort
	  # output result sorted by PAT_MRN and then CONTACT_DATE and then PAT_ENC_CSN_ID and then character position
	  dat_ext <- dat_ext[
	    with(
	      dat_ext,
	      order(PAT_MRN, CONTACT_DATE, PAT_ENC_CSN_ID, POSITION)), ]
	})

	if (verbose > 0) {
	  cat("How long to process:", stime[1:3], "\n")
	  cat(nrow(dat_ext), "rows created.\n")
	}

	return(invisible(dat_ext))
}

#' Extract matching text from progress notes; multiple patterns allowed and exclusion patterns allowed
#'
#' @param yespats vector of regular expressions for which to search
#' @param nopats vector of regular expressions for which to exclude
#' @param dat_pn data.table or data.frame containing clinic visit notes
#' @param varname_pn name of variable in dat_pn containing the progress note.  Default is "PROGRESS_NOTE".
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#'
#' @return data.frame or data.table (invisibly) with seven columns containing all text matching pattern pat, the position of the matches within the progress note and the associated data for that progress note (encounter, mrn, and id tokens; date)
#' @export
#'
#' @examples
#' dat_pn <- data.frame(
#'   PAT_MRN = c("A", "B"),
#'   PAT_ID = c("ZA", "ZB"),
#'   CONTACT_DATE = c("2023-03-01", "2020-03-01"),
#'   PAT_ENC_CSN_ID = c("A1", "B1"),
#'   PROGRESS_NOTE = c("XLRP uh oh", "No XLRP. Good day."))
#' extracts_with_exceptions(
#'    yespats = c("XLRP"),
#'    nopats = c("\\bRP2\\b"),
#'    dat_pn)
extracts_with_exceptions <- function(
    yespats,
    nopats,
    dat_pn,
    varname_pn = "PROGRESS_NOTE",
    verbose = 0) {

  stime1 <- system.time(
    dat_yes <- extracts(
      yespats,
      dat_pn = dat_pn,
      varname_pn = varname_pn,
      verbose = verbose)
	)
  stime2 <- system.time(
    dat_no  <- extracts(
      nopats ,
      dat_pn = dat_pn,
      varname_pn = varname_pn,
      verbose = verbose)
  )

	# vector of TRUE/FALSE
	# for each 'yes' match, is it included in 'no' match?
	# if so, it will be removed
	toexclude <- with(
	  dat_yes,
	  sprintf("%s_%s_%d", PAT_MRN, PAT_ENC_CSN_ID, POSITION)) %in%
		with(
		  dat_no ,
		  sprintf("%s_%s_%d", PAT_MRN, PAT_ENC_CSN_ID, POSITION))

	dat_ext <- dat_yes[!toexclude, ]

	return(invisible(dat_ext))
}
