# Load text data to be searched from clinic visit (a.k.a. progress note)
#
# Chris Andrews
# 2018 05 14
# updated
# 2018 08 20
# 2018 09 06
# 2018 10 22: MRN read as character to preserve leading 0s.
# 2018 10 24: Updated to work with newer datamart that splits the visit file
# 2022 09 02: Read from scrubber files
# e.g., K:/lab/Scrubber/ToScrub/2021/SOURCE_um_oph_enc_visit.csv

loadprogressnotes <- function(
    datadir,
    subdir,
    datafile = "SOURCE_UM_OPH_ENC_VISIT",
    verbose = 0,
    usefread = TRUE,
    skip = 0,
    nrows = -1L) {
  
  if (isTRUE(usefread))
  	if (isFALSE(require("data.table", character.only=TRUE))) stop("Install package data.table.")

  filename <- sprintf("%s/%s/%s.csv", datadir, subdir, datafile)
  if (verbose > 1) cat(filename, "\n")
  
  timing <- system.time(
    dat <- if (usefread) {
      fread(filename, nrows = nrows, header = TRUE, sep = ",", skip=skip, colClasses = "character")
    } else {
      read.csv(filename, nrows = nrows, header = TRUE, sep = ",", skip=skip, colClasses = "character")
    })
  
	if (verbose > 1) {
		print(timing)
	}

	if (any(grepl("DUMMY", names(dat)))) {
		dat$DUMMY <- NULL
		if (verbose > 1) cat("Removed column 'DUMMY'.\n")
	}

	if (anyDuplicated(dat$PAT_ENC_CSN_ID)) {
		if (verbose > 1) cat("Non-unique encounter record ids.\n")
	} # ==0, unique encounter records

	if (verbose > 0) {
		print(class(dat))
		print(dim(dat))
		print(names(dat))
		print(str(dat))
		if (verbose > 1) print(head(dat))
	}

	return(invisible(dat))
}
# 
# dat_pn <- loadprogressnotes(
#   datadir = "K:/lab/Scrubber/ToScrub",
#   subdir = "2021",
#   verbose = 3,
#   usefread = FALSE,
#   nrows = -1L
# )
# 
# datf <- loadprogressnotes(
#   datadir = "K:/lab/Scrubber/ToScrub",
#   subdir = "2021",
#   verbose = 3,
#   usefread = TRUE,
#   nrows = -1L
# )

