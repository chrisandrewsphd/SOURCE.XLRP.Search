# utility functions to create
#   negation file
#   precaution file
#   history file
# utility functions to add relevant info to keyword data

# Chris Andrews
# 2021-03-01


#' find negation terms in progress notes
#'
#' @param dat_pn data.frame containing ENC_VISIT data
#' @param varname_pn name of PROGRESS_NOTE in dat_pn
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#'
#' @return data.frame of negation terms with information about location.
#'
#' @examples # findnegations()
findnegations <- function(
    dat_pn,
    varname_pn = "PROGRESS_NOTE",
    verbose = 0) {
	# it is faster to load previously found negations than
  # to find them a second time.
  # Be sure to store output.

  # Output:
	# each row corresponds to a negation instance.
	# Several rows may apply to the same csn.

  negation_terms <- c(
    "\\bnot?\\b", # The words "no" and "not"
    "n't\\b", # any word ending "n't"
    "without", # "without"
    "\\bw/o\\b") # complete word "w/o"

  stime <- system.time(
    dat_neg <- extracts(
      pats = negation_terms,
      dat_pn = dat_pn,
      varname_pn = varname_pn,
      verbose = verbose))

  if (verbose > 0) cat(stime[1:3], "\n")

	return(invisible(dat_neg))
}

#' find precautions terms in progress notes
#'
#' @param dat_pn data.frame containing ENC_VISIT data
#' @param varname_pn name of PROGRESS_NOTE in dat_pn
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#'
#' @return data.frame of precaution terms with information about location.
#'
#' @examples # findprecautions()
findprecautions <- function(
    dat_pn,
    varname_pn = "PROGRESS_NOTE",
    verbose = 0) {
  # it is faster to load previously found negations than
  # to find them a second time.
  # Be sure to store output.

  # Output:
  # each row corresponds to a negation instance.
  # Several rows may apply to the same csn.

  precaution_terms <- c(
    "\\bprecautions?\\b",
    "\\bdiscuss(ed)?\\b",
    "\\breview(ed)?\\b",
    "\\binstruct(ed)?\\b",
    "worr(y|ied)[[:space:]]+about",
    "\\bd/w\\b",
    "\\bddx\\b",
    "\\br/o\\b",
    "ruled?[[:space:]]+out",
    "\\badvise(d)?\\b",
    "\\bexplain(ed)?\\b",
    "\\beducate(d)?\\b",
    "\\bwarn(ed)?\\b",
    "\\bcounsel(ed)?\\b",
    "\\bcaution(ed)?\\b")

  stime <- system.time(
    dat_pc <- extracts(
      pats = precaution_terms,
      dat_pn = dat_pn,
      varname_pn = varname_pn,
      verbose = verbose))

  if (verbose > 0) cat(stime[1:3], "\n")

  return(invisible(dat_pc))
}


#' find history terms in progress notes
#'
#' @param dat_pn data.frame containing ENC_VISIT data
#' @param varname_pn name of PROGRESS_NOTE in dat_pn
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#'
#' @return data.frame of history terms with information about location.
#'
#' @examples # findhistory()
findhistory <- function(
    dat_pn,
    varname_pn = "PROGRESS_NOTE",
    verbose = 0) {
  # it is faster to load previously found negations than
  # to find them a second time.
  # Be sure to store output.

  # Output:
  # each row corresponds to a negation instance.
  # Several rows may apply to the same csn.

  history_terms <- "(\\bfam(ily)?[ -]*)?(\\b(hist(ory)?|hx)\\b([[:space:]-]*of)?|\\bh/o\\b)"

  stime <- system.time(
    dat_hx <- extracts(
      pats = history_terms,
      dat_pn = dat_pn,
      varname_pn = varname_pn,
      verbose = verbose))

  if (verbose > 0) cat(stime[1:3], "\n")

  return(invisible(dat_hx))
}


###########################################


#' Find sentence boundaries
#'
#' @param dat_pn data.frame Usually the output from loadprogressnote(). Generally the data stored in an ENC_VISIT file.
#' @param varname_pn variable name in dat_pn that contains the PROGRESS_NOTE. Default value is "PROGRESS_NOTE".
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#'
#' @return (invisible) list of length nrow(dat_pn). Each element is an integer matrix with two columns ('start', and 'end') containing locations of sentence boundaries (one row for each sentence).
#' @export
findstringboundaries <- function(
    dat_pn,
    varname_pn = "PROGRESS_NOTE",
    verbose = 0) {

  stime <- system.time(
    sent_bound <- stringi::stri_locate_all_boundaries(
      dat_pn[[varname_pn]],
      type = "sentence"))

  if (verbose > 0) cat("Boundary finding time", stime[1:3], "\n")

  if (verbose > 1) print(utils::head(sent_bound))

  return(invisible(sent_bound))
}

#' Add sentence information to each instance of found keyword
#'
#' @param dat_key data.frame containing instances of keywords
#' @param sent_bound data.frame containing sentence boundaries
#' @param dat_pn data.frame ENC_VISIT containing progress notes
#' @param varname_pn name of PROGRESS_NOTE in dat_pn
#' @param varname_enc variable name of encounter variable in dat_pn
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#'
#' @return data.frame. Same as dat_key but with additional columns
#' SENTENCE, SENTENCE_START, and SENTENCE_END.
#' @export
#'
#' @examples # addsentenceinfo(dat_key, sent_bound, dat_pn)
addsentenceinfo <- function(
    dat_key,
    sent_bound,
    dat_pn,
    varname_pn = "PROGRESS_NOTE",
    varname_enc = "PAT_ENC_CSN_ID",
    verbose = 0) {

  idx_key <- match(
    dat_key[["PAT_ENC_CSN_ID"]], # encounter variable named in extract() is "PAT_ENC_CSN_ID"
    dat_pn[[varname_enc]]) # encounter variable in dat_pn may be supplied by user

  # extra variables to add to dat_key
  dat_key[["SENTENCE_START"]] <- NA_integer_
  dat_key[["SENTENCE_END"]] <- NA_integer_
  dat_key[["SENTENCE"]] <- NA_character_

  for (i in seq_along(idx_key)) {
    # extract sentence from dat_pn$PROGRESS_NOTE containing key word
    these_sent <- stringi::stri_sub(
      dat_pn[[varname_pn]][idx_key[i]],
      from = sent_bound[[idx_key[i]]])
    which_sent <- findInterval(
      dat_key[["POSITION"]][i],
      sent_bound[[idx_key[i]]][, 1])
    dat_key[["SENTENCE"]][i] <- these_sent[which_sent]
    dat_key[["SENTENCE_START"]][i] <- sent_bound[[idx_key[i]]][which_sent, 1]
    dat_key[["SENTENCE_END"]][i]   <- sent_bound[[idx_key[i]]][which_sent, 2]
  }

  return(invisible(dat_key))
}


#' Add negation information to each instance of found keyword
#'
#' @param dat_key data.frame containing instances of keywords
#' @param dat_neg data.frame containing instances of negations
#' @param sent_bound data.frame containing sentence boundaries
#' @param dat_pn data.frame ENC_VISIT containing progress notes
#' @param varname_pn name of PROGRESS_NOTE in dat_pn
#' @param varname_enc variable name of encounter variable
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#'
#' @return data.frame
#'
#' @examples # addnegationinfo()
addnegationinfo <- function(
    dat_key,
    dat_neg,
    sent_bound,
    dat_pn,
    varname_pn = "PROGRESS_NOTE",
    varname_enc = "PAT_ENC_CSN_ID",
    verbose = 0) {

	idx_key <- match(dat_key[, varname_enc], dat_pn[, varname_enc])

	# extra variables to add to dat_key
	dat_key$NEGATION <- NA_character_
	dat_key$NEGATION_POSITION <- NA_integer_
	dat_key$NEGATION_COUNT <- NA_integer_
	dat_key$SENTENCE_START <- NA_integer_
	dat_key$SENTENCE_END <- NA_integer_
	dat_key$SENTENCE <- NA_character_

	for (i in seq_along(idx_key)) {
		# extract sentence from dat_pn$PROGRESS_NOTE containing key word
		these_sent <- stringi::stri_sub(
		  dat_pn[, varname_pn][idx_key[i]],
		  from = sent_bound[[idx_key[i]]])
		which_sent <- findInterval(
		  dat_key$POSITION[i],
		  sent_bound[[idx_key[i]]][, 1])
		dat_key$SENTENCE[i] <- these_sent[which_sent]
		dat_key$SENTENCE_START[i] <- sent_bound[[idx_key[i]]][which_sent, 1]
		dat_key$SENTENCE_END[i]   <- sent_bound[[idx_key[i]]][which_sent, 2]

		# extract negation from dat_neg
		# in same sentence, prior to key term.
		these_neg <- dat_neg[
		  which( (dat_neg$PAT_ENC_CSN_ID == dat_key$PAT_ENC_CSN_ID[i]) & # same encounter
		           (dat_key$SENTENCE_START[i] <= dat_neg$POSITION) & # after the start of this sentence
		           #(dat_neg$POSITION <= dat_key$SENTENCE_END[i])),] # before the end of this sentence
		           (dat_neg$POSITION < dat_key$POSITION[i])),] # before the key word

		dat_key$NEGATION_COUNT[i] <- nrow(these_neg)

		# if there is a negation, take the one closest to the key word
		if (nrow(these_neg) > 0) {
			maxpos <- which.max(these_neg$POSITION) # which negation is closest to the key word
			dat_key$NEGATION_POSITION[i] <- these_neg$POSITION[maxpos] # where is it located
			dat_key$NEGATION[i] <- these_neg$MATCHED_TEXT[maxpos] # what is the negation text
		}
	}

	return(invisible(dat_key))
}

#
#
# removeprecautions <- function(filebase, dir=".", insuffix = "_with_neg", outsuffix = "_not_precaution", writeprecautions=TRUE, overwrite=FALSE) {
# 	if (!require(data.table)) stop("Unable to load package 'data.table'.")
#
# 	if (writeprecautions) {
# 		outfile <- sprintf("%s/%s%s%s.csv", dir, filebase, insuffix, outsuffix)
# 		if (file.exists(outfile)) {
# 			if (overwrite) {
# 				warning(sprintf("%s exists and will be overwritten.", outfile))
# 			} else {
# 				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
# 			}
# 		}
# 	}
#
# 	# load previously found key words
# 	dat_key <- fread(file=sprintf("%s/%s%s.csv", dir, filebase, insuffix), colClasses=c(PAT_MRN="character"))
# 	idx_key <- match(dat_key$PAT_ENC_CSN_ID, dat_pn$PAT_ENC_CSN_ID)
#
# 	PRECAUTION <- rep(NA, length(idx_key))
#
# 	for (i in seq_along(idx_key)) {
# 		# if there is a precaution term in the same sentence as keyword, flag the row for removal
# 		PRECAUTION[i] <- any(
# 			(dat_pc$PAT_ENC_CSN_ID == dat_key$PAT_ENC_CSN_ID[i]) & # same encounter
# 				(dat_key$SENTENCE_START[i] <= dat_pc$POSITION) & # after the start of this sentence
# 				(dat_pc$POSITION <= dat_key$SENTENCE_END[i])) # before the end of this sentence
# 	}
#
# 	cat(sprintf("%d hits of %d eliminated due to precautionary term\n", sum(PRECAUTION), length(PRECAUTION)))
#
# 	dat_key_nop <- dat_key[ !PRECAUTION, ]
#
# 	if (writeprecautions) {
# 		if (require(data.table)) fwrite(dat_key_nop, file=outfile)
# 		else warning(sprintf("data.table not available. %s file not written.", outfile))
# 	}
#
# 	return(invisible(dat_key_nop))
# }
#
#
#
# removehistories <- function(filebase, dir=".", insuffix = "_with_neg_not_precaution", outsuffix = "_not_history", writehistories=TRUE, overwrite=FALSE) {
# 	if (!require(data.table)) stop("Unable to load package 'data.table'.")
#
# 	if (writehistories) {
# 		outfile <- sprintf("%s/%s%s%s.csv", dir, filebase, insuffix, outsuffix)
# 		if (file.exists(outfile)) {
# 			if (overwrite) {
# 				warning(sprintf("%s exists and will be overwritten.", outfile))
# 			} else {
# 				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
# 			}
# 		}
# 	}
#
# 	# load previously found key words
# 	dat_key <- fread(file=sprintf("%s/%s%s.csv", dir, filebase, insuffix), colClasses=c(PAT_MRN="character"))
# 	idx_key <- match(dat_key$PAT_ENC_CSN_ID, dat_pn$PAT_ENC_CSN_ID)
#
# 	HISTORY <- rep(NA, length(idx_key))
#
# 	for (i in seq_along(idx_key)) {
# 		# if there is a history term in the same sentence as keyword, flag the row for removal
# 		HISTORY[i] <- any(
# 			(dat_hx$PAT_ENC_CSN_ID == dat_key$PAT_ENC_CSN_ID[i]) & # same encounter
# 				(dat_key$SENTENCE_START[i] <= dat_hx$POSITION) & # after the start of this sentence
# 				(dat_hx$POSITION <= dat_key$SENTENCE_END[i])) # before the end of this sentence
# 	}
#
# 	cat(sprintf("%d hits of %d eliminated due to history term\n", sum(HISTORY), length(HISTORY)))
#
# 	dat_key_noh <- dat_key[ !HISTORY, ]
#
# 	if (writehistories) {
# 		if (require(data.table)) fwrite(dat_key_noh, file=outfile)
# 		else warning(sprintf("data.table not available. %s file not written.", outfile))
# 	}
#
# 	return(invisible(dat_key_noh))
# }
#
