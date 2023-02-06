#' Load Progress Notes: This function reads the ENC_VISIT file in preparation for free text search of the progress note.
#'
#' @param filename Name of encounter visit file.  At UM this is "SOURCE_UM_OPH_ENC_VISIT.csv".  You may wish to include entire path to the file.
#' @param verbose Controls the amount of output to the console. The default, 0, prints nothing.  Higher values provide more detail.
#' @param usefread If TRUE (default), use data.table::fread. If FALSE, use utils::read.csv.
#' @param nrows Number of rows to read from file.  Default (-1) is to read all rows.  Consider positive values for testing.
#'
#' @return A data.table (fread == TRUE) or data.frame (fread == FALSE).
#' @export
#'
#' @examples
#' ## Not Run
#' ## dat_pn <- loadprogressnotes("K:/lab/Scrubber/ToScrub/2021/SOURCE_UM_OPH_ENC_VISIT.csv")

loadprogressnotes <- function(
    filename,
    verbose = 0,
    usefread = TRUE,
    nrows = -1L) {

  # read all columns as character
  timing <- system.time(
    dat <- if (isTRUE(usefread)) {
      data.table::fread(
        filename,
        nrows = nrows,
        header = TRUE,
        sep = ",",
        colClasses = "character")
    } else {
      utils::read.csv(
        filename,
        nrows = nrows,
        header = TRUE,
        sep = ",",
        colClasses = "character")
    })

	if (verbose > 1) print(timing)

  # remove 'dummy' column if present
	if (any(grepl("DUMMY", names(dat)))) {
		dat$DUMMY <- NULL
		if (verbose > 0) cat("Removed column 'DUMMY'.\n")
	}

	if (anyDuplicated(dat$PAT_ENC_CSN_ID)) {
		if (verbose > 1) cat("Non-unique encounter record ids.\n")
	} # ==0, unique encounter records

	if (verbose > 0) {
		print(class(dat))
		print(dim(dat))
		print(names(dat))
		print(utils::str(dat))
		if (verbose > 1) print(utils::head(dat))
	}

	return(invisible(dat))
}
