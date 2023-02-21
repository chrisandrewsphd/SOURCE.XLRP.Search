#' Load Progress Notes: This function reads the ENC_VISIT file in preparation for free text search of the progress note.
#'
#' @param filename Name of encounter visit file.  At UM this is "SOURCE_UM_OPH_ENC_VISIT.csv".  You may wish to include entire path to the file.
#' @param varname_enc_id Name of the encounter id variable filename.  At UM this is "PAT_ENC_CSN_ID" (which is the default if nothing else is provided).
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#' @param usefread If TRUE (default), use data.table::fread. If FALSE, use utils::read.csv.
#' @param nrows Number of rows to read from file.  Default (-1) is to read all rows.  Consider positive values for testing.
#'
#' @return A data.table (fread == TRUE) or data.frame (fread == FALSE), invisibly.
#' @export
#'
#' @examples
#' ## Not Run
#' ## dat_pn <- loadprogressnotes("K:/lab/Scrubber/ToScrub/2021/SOURCE_UM_OPH_ENC_VISIT.csv")

loadprogressnotes <- function(
    filename,
    varname_enc_id = "PAT_ENC_CSN_ID",
    verbose = 0,
    usefread = TRUE,
    nrows = NULL) {

  # read all columns as character
  timing <- system.time(
    dat <- if (isTRUE(usefread)) {
      data.table::fread(
        filename,
        nrows = if (is.null(nrows)) Inf else nrows,
        header = TRUE,
        sep = ",",
        colClasses = "character")
    } else if(isFALSE(usefread)) {
      utils::read.csv(
        filename,
        nrows = if (is.null(nrows)) -1L else nrows,
        header = TRUE,
        sep = ",",
        colClasses = "character")
    } else {
      stop("'usefread' must be TRUE or FALSE.")
    })

	if (verbose > 1) cat("Read time: ", timing[1:3], "\n")

  # remove 'dummy' column if present
	if ("DUMMY" %in% names(dat)) {
		dat$DUMMY <- NULL
		if (verbose > 1) cat("Removed column 'DUMMY'.\n")
	}

  if (!(varname_enc_id %in% names(dat))) {
    if (verbose > 0) {
      cat(sprintf("%s is not a variable name in %s\nUniqueness check skipped.\n", varname_enc_id, filename))
    }
  } else {
    if (anyDuplicated(dat[[varname_enc_id]])) {
      if (verbose > 0) cat("Non-unique encounter record ids.\n")
    } # ==0, all distinct encounter records
  }

	if (verbose > 0) {
		print(class(dat))
		print(dim(dat))
		print(names(dat))
		print(utils::str(dat))
		if (verbose > 1) print(utils::head(dat))
	}

	return(invisible(dat))
}
