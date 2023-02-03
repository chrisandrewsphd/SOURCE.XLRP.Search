# Search for key terms in encounter notes (aka progress notes)
##### this version does not track sentences or locations of hits ####
#
# Chris Andrews
# 2018 05 07
# 2018 09 07 moved from EMERSE to R code directory
#   made more modular

# 2022 09 02

# use "load progress notes.R" to load dat_pn
source("./R Code/load progress notes.R")


###################

extract <- function(pat, filebase=NULL, outdir=".", overwrite=FALSE) {
	# check if dat_pn exists.
	if (!exists("dat_pn")) stop("dat_pn does not exist.  Call loadprogressnotes.")
	
	# check whether output directory exists
	if (!is.null(outdir) && !dir.exists(outdir)) stop("Output directory does not exist.")
	
	# check whether output file already exists
	if (!is.null(filebase)) {
		outfile <- sprintf("%s/%s.csv", outdir, filebase)
		if (file.exists(outfile)) {
			if (overwrite) {
				warning(sprintf("%s exists and will be overwritten.", outfile))
			} else {
				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
			}
		}
	}
	
	# determine the locations of the matches within the notes
	loc <- gregexpr(pat, dat_pn$PROGRESS_NOTE, ignore.case = TRUE)
	# extract the text of the matches
	ext <- regmatches(dat_pn$PROGRESS_NOTE, loc)
	
	# the number of matches found (might be several per progress note)
	n <- sum(sapply(ext, length))
	# create storage vectors
	csn <- pos <- line <- numeric(n)
	id <- mrn <- date <- found <- character(n)
	
	j <- 0 # j counts from 1 to n
	for (i in seq(nrow(dat_pn))) { # i loops through records in dat
		if (l <- length(ext[[i]])) { # if any matches in record i, then
			repl <- seq.int(j+1, j+l) # copy the information to the new vectors
			csn  [repl] <- dat_pn$PAT_ENC_CSN_ID[i]
			line [repl] <- seq.int(l)
			id   [repl] <- dat_pn$PAT_ID[i]
			mrn  [repl] <- dat_pn$PAT_MRN[i]
			date [repl] <- dat_pn$CONTACT_DATE[i]
			found[repl] <- ext[[i]]
			pos  [repl] <- loc[[i]]
			j <- j + l # advance counter by number of matches in this record
		}
	}
	
	dat_ext <- data.frame(csn, id, mrn, date, found, pos, line)
	#names(dat_ext) <- c(names(dat), "POSITION", "LINE") # this way the MATCHED_TEXT is named PROGRESS_NOTE
	names(dat_ext) <- c("PAT_ENC_CSN_ID", "PAT_ID", "PAT_MRN", "CONTACT_DATE", "MATCHED_TEXT", "POSITION", "LINE")
	
	# output result sorted by PAT_MRN and then CONTACT_DATE and then PAT_ENC_CSN_ID and then character position
	dat_ext <- dat_ext[with(dat_ext, order(PAT_MRN, CONTACT_DATE, PAT_ENC_CSN_ID, POSITION)), ]
	
	if (!is.null(filebase) && !is.null(outdir)) {
		if (require(data.table)) fwrite(dat_ext, file=outfile)
		else warning(sprintf("data.table not available. %s file not written.", outfile))
	}

	return(invisible(dat_ext))
}

#system.time(dat_wo <- extract(pat="\\bw/o\\b", filebase=NULL))

# system.time(dat_on <- extract(pat="Optic Neuritis", filebase = "ON"))

# this version is the same but allows a vector of patterns.
extracts <- function(pats, filebase=NULL, outdir=".", overwrite=FALSE) {
	# check if output directory and output file already exist
	outfile <- sprintf("%s/%s.csv", outdir, filebase)
	
	# check whether output directory exists
	if (!is.null(outdir) && !dir.exists(outdir)) stop("Output directory does not exist.")
	
	# check whether output file already exists
	if (!is.null(filebase)) {
		outfile <- sprintf("%s/%s.csv", outdir, filebase)
		if (file.exists(outfile)) {
			if (overwrite) {
				warning(sprintf("%s exists and will be overwritten.", outfile))
			} else {
				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
			}
		}
	}
	
	npat <- length(pats)
	
	dat_exts <- lapply(pats, function(pat) extract(pat, filebase=NULL))

	dat_ext <- do.call(rbind, dat_exts)

	# re-sort
	# output result sorted by PAT_MRN and then CONTACT_DATE and then PAT_ENC_CSN_ID and then character position
	dat_ext <- dat_ext[with(dat_ext, order(PAT_MRN, CONTACT_DATE, PAT_ENC_CSN_ID, POSITION)), ]
	
	if (!is.null(filebase) && !is.null(outdir)) {
		if (require(data.table)) fwrite(dat_ext, file=outfile)
		else warning(sprintf("data.table not available. %s file not written.", outfile))
	}

	return(invisible(dat_ext))
}

extracts_with_exceptions <- function(yespats, nopats, filebase=NULL, outdir=".", overwrite=FALSE) {
	# check if output directory and output file already exist
	outfile <- sprintf("%s/%s.csv", outdir, filebase)
	
	# check whether output directory exists
	if (!is.null(outdir) && !dir.exists(outdir)) stop("Output directory does not exist.")
	
	# check whether output file already exists
	if (!is.null(filebase)) {
		outfile <- sprintf("%s/%s.csv", outdir, filebase)
		if (file.exists(outfile)) {
			if (overwrite) {
				warning(sprintf("%s exists and will be overwritten.", outfile))
			} else {
				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
			}
		}
	}

	dat_yes <- extracts(yespats, filebase=NULL)
	dat_no  <- extracts(nopats,  filebase=NULL)

	# vector of TRUE/FALSE
	toexclude <- with(dat_yes, sprintf("%s_%s_%d", PAT_MRN, PAT_ENC_CSN_ID, POSITION)) %in%
		with(dat_no , sprintf("%s_%s_%d", PAT_MRN, PAT_ENC_CSN_ID, POSITION))
	
	dat_ext <- dat_yes[!toexclude,]

	if (!is.null(filebase) && !is.null(outdir)) {
		if (require(data.table)) fwrite(dat_ext, file=outfile)
		else warning(sprintf("data.table not available. %s file not written.", outfile))
	}
	
	return(invisible(dat_ext))
}


# previous default location
# outdir <- "S:/Andrews Projects/Text Analytics/EMERSE Data/michart"
# current output location
# dir1 <- sprintf("%s/Andrews Projects/Text Analytics/Filtered Data/Intermediate Data/%s", datadrive, datadir)
dir1 <- "./Data"

dir.exists(dir1) # check if directory exists for output


#####################
# finding negations #
#####################
# load previously found negations if already computed

# each row corresponds to a key word instance.  Several rows may apply to the same csn.
if (file.exists(sprintf("%s/%s.csv", dir1, "negation"))) {
	dat_neg <- fread(file=sprintf("%s/%s.csv", dir1, "negation"), colClasses=c(PAT_MRN="character"))
	nrow(dat_neg)
} else {
	negation_terms <- c("\\bnot?\\b", "n't\\b", "without", "\\bw/o\\b")
	print(system.time(dat_neg <- extracts(pats=negation_terms, filebase="negation", outdir=dir1)))
	print(summary(dat_neg$MATCHED_TEXT)) # was summary(dat_neg$PROGRESS_NOTE)
	nrow(dat_neg)
}

class(dat_neg) # data.table (fread) or data.frame (read.csv)
head(dat_neg)
# idx_neg <- match(dat_neg$PAT_ENC_CSN_ID, dat_pn$PAT_ENC_CSN_ID)



# precautionary terms
if (file.exists(sprintf("%s/%s.csv", dir1, "precaution"))) {
	dat_pc <- fread(file=sprintf("%s/%s.csv", dir1, "precaution"), colClasses=c(PAT_MRN="character"))
	nrow(dat_pc)
} else {
	#precaution_terms <- c("\\bprecautions?\\b", "\\bdiscuss(ed)?\\b", "\\breview(ed)?\\b")
	precaution_terms <- c("\\bprecautions?\\b", "\\bdiscuss(ed)?\\b", "\\breview(ed)?\\b", "\\binstruct(ed)?\\b", "worr(y|ied) about", "\\bd/w\\b", "\\bddx\\b", "\\br/o\\b", "rule out", "\\badvise(d)?\\b", "\\bexplain(ed)?\\b", "\\beducate(d)?\\b", "\\bwarn(ed)?\\b", "\\bcounsel(ed)?\\b") # added 2018 06 11
	#precaution_terms <- c("\\b(precautions?|discuss(ed)?|review(ed)?|instruct(ed)?|d/w|ddx|r/o|advise(d)?|explain(ed)?|educate(d)?|warn(ed)?|counsel(ed)?)\\b", "(rule out|worr(y|ied) about)") # should check if this runs faster, also add * after spaces, also add "caution(ed)?"
	print(system.time(dat_pc <- extracts(pats=precaution_terms, filebase="precaution", out=dir1)))
	print(summary(dat_pc$MATCHED_TEXT))
	nrow(dat_pc)
}

class(dat_pc)
head(dat_pc)

# history terms # added 2018 09 07
if (file.exists(sprintf("%s/%s.csv", dir1, "history"))) {
	dat_hx <- fread(file=sprintf("%s/%s.csv", dir1, "history"), colClasses=c(PAT_MRN="character"))
	nrow(dat_hx)
} else {
	history_terms <- "(fam(ily)?[ -]*)?((hist(ory)?|hx)([ -]*of)?|\\bh/o\\b)" # should hist or hx be required to be complete word?
	print(system.time(dat_hx <- extract(pat=history_terms, filebase="history", out=dir1)))
	print(summary(dat_hx$MATCHED_TEXT))
	nrow(dat_hx)
}

class(dat_hx)
head(dat_hx)

# Extract keywords
system.time(dat_ext <- extract(pat="optic[ -]*neuritis", filebase="optic neuritis", out=dir1))



###################################
# ADD SENTENCE TO THE ABOVE FILES #
###################################

# sentence boundary locations
library(stringi)
# ~1 minute
# same length as dat_pn$PROGRESS_NOTE
# each list element is a 2-column matrix of positions: cbind(sentence start, sentence end)
system.time(sent_bound <- stri_locate_all_boundaries(dat_pn$PROGRESS_NOTE, type="sentence"))

addsentence <- function(filebase, dir=".", rewrite=TRUE, extrasentences=0, overwrite=FALSE) {
	if (!require(data.table)) stop("Unable to load package 'data.table'.")
	if (!require(stringi)) stop("Unable to load package 'stringi'.")
	
	if (rewrite) {
		outfile <- sprintf("%s/%s_with_sentence.csv", dir, filebase)
		if (file.exists(outfile)) {
			if (overwrite) {
				warning(sprintf("%s exists and will be overwritten.", outfile))
			} else {
				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
			}
		}
	}

	# make sure sentence boundaries are defined
	if (!exists("sent_bound")) {
		sent_bound <<- stri_locate_all_boundaries(dat_pn$PROGRESS_NOTE, type="sentence")
	}

	# load previously found key words
	dat_key <- fread(file=sprintf("%s/%s.csv", dir, filebase), colClasses=c(PAT_MRN="character"))
	idx_key <- match(dat_key$PAT_ENC_CSN_ID, dat_pn$PAT_ENC_CSN_ID)
	
	# extra variables to add to dat_key
	dat_key$SENTENCE_START <- NA_integer_
	dat_key$SENTENCE_END <- NA_integer_
	dat_key$SENTENCE <- NA_character_
	
	for (i in seq_along(idx_key)) {
		# extract sentence from dat_pn$PROGRESS_NOTE containing key word
		these_sent <- stri_sub(dat_pn$PROGRESS_NOTE[idx_key[i]], from=sent_bound[[idx_key[i]]])
		which_sent <- findInterval(dat_key$POSITION[i], sent_bound[[idx_key[i]]][,1])
		if (extrasentences==0) {
			dat_key$SENTENCE[i] <- these_sent[which_sent]
			dat_key$SENTENCE_START[i] <- sent_bound[[idx_key[i]]][which_sent,1]
			dat_key$SENTENCE_END[i]   <- sent_bound[[idx_key[i]]][which_sent,2]
		} else {
			last_sent <- min(which_sent + extrasentences, length(these_sent))
			dat_key$SENTENCE[i] <- paste(these_sent[which_sent:last_sent], collapse = "")
			dat_key$SENTENCE_START[i] <- sent_bound[[idx_key[i]]][which_sent,1]
			dat_key$SENTENCE_END[i]   <- sent_bound[[idx_key[i]]][last_sent,2]
		}
	}
	
	if (rewrite) {
		if (require(data.table)) fwrite(dat_key, file=outfile)
		else warning(sprintf("data.table not available. %s file not written.", outfile))
	}
	
	return(invisible(dat_key))
}


system.time(dat_ext <- addsentence(filebase="optic neuritis", rewrite=TRUE, dir=dir1))

###############################################
# ADD NEGATION INFORMATION TO THE ABOVE FILES #
###############################################


addnegationinfo <- function(filebase, dir=".", rewrite=TRUE, overwrite=FALSE) {
	if (!require(data.table)) stop("Unable to load package 'data.table'.")
	if (!require(stringi)) stop("Unable to load package 'stringi'.")

	if (rewrite) {
		outfile <- sprintf("%s/%s_with_neg.csv", dir, filebase)
		if (file.exists(outfile)) {
			if (overwrite) {
				warning(sprintf("%s exists and will be overwritten.", outfile))
			} else {
				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
			}
		}
	}
	
	if (!exists("sent_bound")) {
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


allkeyfiles <- "optic neuritis"

for (keyfile in allkeyfiles) {
	cat(sprintf("%s: %s\n", date(), keyfile))
	print(system.time(addnegationinfo(filebase=keyfile, dir=dir1)))
}


############
# remove hits where the key term is in the same sentence as a precautionary term


removeprecautions <- function(filebase, dir=".", writeprecautions=TRUE, overwrite=FALSE) {
	if (!require(data.table)) stop("Unable to load package 'data.table'.")

	if (writeprecautions) {
		outfile <- sprintf("%s/%s_with_neg_not_precaution.csv", dir, filebase)
		if (file.exists(outfile)) {
			if (overwrite) {
				warning(sprintf("%s exists and will be overwritten.", outfile))
			} else {
				stop(sprintf("%s exists and overwrite == FALSE.", outfile))
			}
		}
	}
	
	# load previously found key words
	dat_key <- fread(file=sprintf("%s/%s_with_neg.csv", dir, filebase), colClasses=c(PAT_MRN="character"))
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


precautionfiles <- c("optic neuritis")
# precautionfiles <- allkeyfiles

for (keyfile in precautionfiles) {
  cat(sprintf("%s: %s\n", date(), keyfile))
  print(system.time(removeprecautions(filebase=keyfile, writeprecautions = TRUE, dir=dir1)))
}

removeprecautionsandhistories <- function(filebase, dir=".", writeprecautions=TRUE, overwrite=FALSE) {
  if (!require(data.table)) stop("Unable to load package 'data.table'.")
  
  if (writeprecautions) {
    outfile <- sprintf("%s/%s_with_neg_not_precaution_history.csv", dir, filebase)
    if (file.exists(outfile)) {
      if (overwrite) {
        warning(sprintf("%s exists and will be overwritten.", outfile))
      } else {
        stop(sprintf("%s exists and overwrite == FALSE.", outfile))
      }
    }
  }
  
  # load previously found key words
  dat_key <- fread(file=sprintf("%s/%s_with_neg.csv", dir, filebase), colClasses=c(PAT_MRN="character"))
  idx_key <- match(dat_key$PAT_ENC_CSN_ID, dat_pn$PAT_ENC_CSN_ID)
  
  PRECAUTION <- rep(NA, length(idx_key))
  HISTORY <- rep(NA, length(idx_key))
  
  for (i in seq_along(idx_key)) {
    # if there is a precaution term in the same sentence as keyword, flag the row for removal
    PRECAUTION[i] <- any(
      (dat_pc$PAT_ENC_CSN_ID == dat_key$PAT_ENC_CSN_ID[i]) & # same encounter
        (dat_key$SENTENCE_START[i] <= dat_pc$POSITION) & # after the start of this sentence
        (dat_pc$POSITION <= dat_key$SENTENCE_END[i])) # before the end of this sentence
    # if there is a hisotry term in the same sentence as keyword, flag the row for removal
    HISTORY[i] <- any(
      (dat_hx$PAT_ENC_CSN_ID == dat_key$PAT_ENC_CSN_ID[i]) & # same encounter
        (dat_key$SENTENCE_START[i] <= dat_hx$POSITION) & # after the start of this sentence
        (dat_hx$POSITION <= dat_key$SENTENCE_END[i])) # before the end of this sentence
  }
  
  cat(sprintf("%d hits of %d eliminated due to precautionary term\n", sum(PRECAUTION), length(PRECAUTION)))
  cat(sprintf("%d hits of %d eliminated due to history term\n", sum(HISTORY), length(HISTORY)))
  
  dat_key_noph <- dat_key[ !PRECAUTION & !HISTORY, ]
  
  if (writeprecautions) {
    if (require(data.table)) fwrite(dat_key_noph, file=outfile)
    else warning(sprintf("data.table not available. %s file not written.", outfile))
  }
  
  return(invisible(dat_key_noph))
}	

precautionfiles <- c("optic neuritis")
# precautionfiles <- allkeyfiles

for (keyfile in precautionfiles) {
	cat(sprintf("%s: %s\n", date(), keyfile))
	print(system.time(removeprecautionsandhistories(filebase=keyfile, writeprecautions = TRUE, dir=dir1)))
}
