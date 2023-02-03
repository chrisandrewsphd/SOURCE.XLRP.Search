# utility functions to create
#   negation file
#   precaution file
#   history file

# Chris Andrews
# 2021-03-01


findnegations <- function(dir = ".", verbose = 0, usefread = TRUE) {
	# load previously found negations if already computed
	# or construct it
  # (should also check if already dat_neg already loaded?)
  
	# Output:
	# each row corresponds to a key word instance.
	# Several rows may apply to the same csn.
	
  negfile <- sprintf("%s/%s.csv", dir, "negation")
	if (file.exists(negfile)) {
	  dat_neg <- if (isTRUE(usefread)) {
	    if (!require(data.table)) stop("could not load 'data.table'")
	    fread(file = negfile)
	  } else if (isFALSE(usefread)) {
	    read.csv(file = negfile)
	  } else stop("'usefread' must be TRUE or FALSE")
	  
		if (verbose > 0) cat(nrow(dat_neg), "negation rows read.\n")
	  
	} else { # else negation file needs to be created
	  
	  negation_terms <- c("\\bnot?\\b", "n't\\b", "without", "\\bw/o\\b")
	  howlong <- system.time(
	    dat_neg <- extracts(pats = negation_terms, filebase = "negation", outdir = dir))
	  
	  if (verbose > 0) {
	    cat("how long to process:\n")
	    print(howlong)
	    cat(nrow(dat_neg), "negation rows created.\n")
	    
	    
	    if (verbose > 1) print(table(dat_neg$MATCHED_TEXT))
	    
	  }
	}
  
  if (verbose > 1) print(head(dat_neg))
  
	return(invisible(dat_neg))
}


findprecautions <- function(dir = ".", verbose = 0, usefread = TRUE) {
  
  # use existing file if possible
  # (should also check if already dat_pc already loaded?)
  pcfile <- sprintf("%s/%s.csv", dir, "precaution")
	if (file.exists(pcfile)) {
	  dat_pc <- if (isTRUE(usefread)) {
	    if (!require(data.table)) stop("could not load 'data.table'")
		  fread(file = pcfile)
	  } else if (isFALSE(usefread)) {
	    read.csv(pcfile)
	  } else stop("'usefread' must be TRUE or FALSE")
	  
		if (verbose > 0) cat(nrow(dat_pc), "precaution rows read.\n")
		return(invisible(dat_pc))
	}
  
  # precautionary terms
  precaution_terms <- c("\\bprecautions?\\b", "\\bdiscuss(ed)?\\b", "\\breview(ed)?\\b", "\\binstruct(ed)?\\b", "worr(y|ied)[[:space:]]+about", "\\bd/w\\b", "\\bddx\\b", "\\br/o\\b", "ruled?[[:space:]]+out", "\\badvise(d)?\\b", "\\bexplain(ed)?\\b", "\\beducate(d)?\\b", "\\bwarn(ed)?\\b", "\\bcounsel(ed)?\\b", "\\bcaution(ed)?\\b")
	
	howlong <- system.time(dat_pc <- extracts(pats=precaution_terms, filebase="precaution", out=dir))
	
	if (verbose>0) {
		cat("how long to process:\n")
		print(howlong)
		
		print(summary(dat_pc$MATCHED_TEXT))
		
		cat(nrow(dat_pc), "precaution rows created.\n")
		print(head(dat_pc))
	}
	
	return(invisible(dat_pc))
}


# history terms # added 2018 09 07

findhistory <- function(dir=".", verbose=0, usefread = TRUE) {
  # use existing file if possible
  # (should also check if already dat_pc already loaded?)
  hxfile <- sprintf("%s/%s.csv", dir, "history")
  if (file.exists(hxfile)) {
    dat_hx <- if (isTRUE(usefread)) {
      if (!require(data.table)) stop("could not load 'data.table'")
      fread(file = hxfile)
    } else if (isFALSE(usefread)) {
      read.csv(hxfile)
    } else stop("'usefread' must be TRUE or FALSE")
    
    if (verbose > 0) cat(nrow(dat_hx), "history rows read.\n")
    return(invisible(dat_hx))
  }
  
	history_terms <- "(\\bfam(ily)?[ -]*)?(\\b(hist(ory)?|hx)\\b([[:space:]-]*of)?|\\bh/o\\b)"
	
	howlong <- system.time(dat_hx <- extract(
	  pat = history_terms, filebase = "history", out = dir))

	if (verbose>0) {
		cat("how long to process:\n")
		print(howlong)
		
		print(table(dat_hx$MATCHED_TEXT))
		
		cat(nrow(dat_hx), "history rows created.\n")
		print(head(dat_hx))
	}
	
	return(invisible(dat_hx))
}


###########################################


addnegationinfo <- function(
    filebase, dir = ".",
    usefread = TRUE,
    rewrite=TRUE, overwrite=FALSE,
    use_existing_sentence_boundaries_if_available = FALSE) {
  
  if (isTRUE(usefread)) {
    if (!require(data.table)) stop("Unable to load package 'data.table'.")
	}
  if (!require(stringi)) stop("Unable to load package 'stringi'.")
	
	if (isTRUE(rewrite)) {
		outfile <- sprintf("%s/%s_with_neg.csv", dir, filebase)
		if (file.exists(outfile)) {
			if (isTRUE(overwrite)) {
				warning(sprintf("%s exists and will be overwritten.", outfile))
			} else if (isFALSE(overwrite)) {
				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
			} else stop("'overwrite' must be TRUE or FALSE")
		}
	}
	
	if (!use_existing_sentence_boundaries_if_available || !exists("sent_bound")) {
		sent_bound <<- stri_locate_all_boundaries(dat_pn$PROGRESS_NOTE, type="sentence")
	}
	
	# load previously found key words
	dat_key <- fread(file=sprintf("%s/%s.csv", dir, filebase), colClasses=c(PAT_MRN="character"))
	idx_key <- match(dat_key$PAT_ENC_CSN_ID, dat_pn$PAT_ENC_CSN_ID)
	
	# extra variables to add to dat_key
	dat_key$NEGATION <- NA_character_
	dat_key$NEGATION_POSITION <- NA_integer_
	dat_key$NEGATION_COUNT <- NA_integer_
	dat_key$SENTENCE_START <- NA_integer_
	dat_key$SENTENCE_END <- NA_integer_
	dat_key$SENTENCE <- NA_character_
	
	for (i in seq_along(idx_key)) {
		# extract sentence from dat_pn$PROGRESS_NOTE containing key word
		these_sent <- stri_sub(dat_pn$PROGRESS_NOTE[idx_key[i]], from=sent_bound[[idx_key[i]]])
		which_sent <- findInterval(dat_key$POSITION[i], sent_bound[[idx_key[i]]][,1])
		dat_key$SENTENCE[i] <- these_sent[which_sent]
		dat_key$SENTENCE_START[i] <- sent_bound[[idx_key[i]]][which_sent,1]
		dat_key$SENTENCE_END[i]   <- sent_bound[[idx_key[i]]][which_sent,2]
		
		# extract negation from dat_neg
		# in same sentence, prior to key term.
		these_neg <- dat_neg[which( (dat_neg$PAT_ENC_CSN_ID == dat_key$PAT_ENC_CSN_ID[i]) & # same encounter
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
	
	if (rewrite) {
		if (require(data.table)) fwrite(dat_key, file=outfile)
		else warning(sprintf("data.table not available. %s file not written.", outfile))
	}
	
	return(invisible(dat_key))
}



removeprecautions <- function(filebase, dir=".", insuffix = "_with_neg", outsuffix = "_not_precaution", writeprecautions=TRUE, overwrite=FALSE) {
	if (!require(data.table)) stop("Unable to load package 'data.table'.")
	
	if (writeprecautions) {
		outfile <- sprintf("%s/%s%s%s.csv", dir, filebase, insuffix, outsuffix)
		if (file.exists(outfile)) {
			if (overwrite) {
				warning(sprintf("%s exists and will be overwritten.", outfile))
			} else {
				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
			}
		}
	}
	
	# load previously found key words
	dat_key <- fread(file=sprintf("%s/%s%s.csv", dir, filebase, insuffix), colClasses=c(PAT_MRN="character"))
	idx_key <- match(dat_key$PAT_ENC_CSN_ID, dat_pn$PAT_ENC_CSN_ID)
	
	PRECAUTION <- rep(NA, length(idx_key))
	
	for (i in seq_along(idx_key)) {
		# if there is a precaution term in the same sentence as keyword, flag the row for removal
		PRECAUTION[i] <- any(
			(dat_pc$PAT_ENC_CSN_ID == dat_key$PAT_ENC_CSN_ID[i]) & # same encounter
				(dat_key$SENTENCE_START[i] <= dat_pc$POSITION) & # after the start of this sentence
				(dat_pc$POSITION <= dat_key$SENTENCE_END[i])) # before the end of this sentence
	}
	
	cat(sprintf("%d hits of %d eliminated due to precautionary term\n", sum(PRECAUTION), length(PRECAUTION)))
	
	dat_key_nop <- dat_key[ !PRECAUTION, ]
	
	if (writeprecautions) {
		if (require(data.table)) fwrite(dat_key_nop, file=outfile)
		else warning(sprintf("data.table not available. %s file not written.", outfile))
	}
	
	return(invisible(dat_key_nop))
}	



removehistories <- function(filebase, dir=".", insuffix = "_with_neg_not_precaution", outsuffix = "_not_history", writehistories=TRUE, overwrite=FALSE) {
	if (!require(data.table)) stop("Unable to load package 'data.table'.")

	if (writehistories) {
		outfile <- sprintf("%s/%s%s%s.csv", dir, filebase, insuffix, outsuffix)
		if (file.exists(outfile)) {
			if (overwrite) {
				warning(sprintf("%s exists and will be overwritten.", outfile))
			} else {
				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
			}
		}
	}

	# load previously found key words
	dat_key <- fread(file=sprintf("%s/%s%s.csv", dir, filebase, insuffix), colClasses=c(PAT_MRN="character"))
	idx_key <- match(dat_key$PAT_ENC_CSN_ID, dat_pn$PAT_ENC_CSN_ID)
	
	HISTORY <- rep(NA, length(idx_key))
	
	for (i in seq_along(idx_key)) {
		# if there is a history term in the same sentence as keyword, flag the row for removal
		HISTORY[i] <- any(
			(dat_hx$PAT_ENC_CSN_ID == dat_key$PAT_ENC_CSN_ID[i]) & # same encounter
				(dat_key$SENTENCE_START[i] <= dat_hx$POSITION) & # after the start of this sentence
				(dat_hx$POSITION <= dat_key$SENTENCE_END[i])) # before the end of this sentence
	}
	
	cat(sprintf("%d hits of %d eliminated due to history term\n", sum(HISTORY), length(HISTORY)))
	
	dat_key_noh <- dat_key[ !HISTORY, ]
	
	if (writehistories) {
		if (require(data.table)) fwrite(dat_key_noh, file=outfile)
		else warning(sprintf("data.table not available. %s file not written.", outfile))
	}
	
	return(invisible(dat_key_noh))
}	

