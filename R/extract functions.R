# extract, extracts, extracts_with_exceptions functions
# Chris Andrews
# 2021-03-01


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

