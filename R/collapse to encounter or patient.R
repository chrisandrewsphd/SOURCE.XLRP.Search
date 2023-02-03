# Summarize the text found
# from the keyword level
# to the encounter level
# to the patient level


datadrive <- "M:/EPIC-Ophthalmology" # Turbo
#datadir <- "2012 Aug 01 - 2018 Sep 30"
datadir <- "2012 Aug 01 - 2018 Dec 31"
pnedir <- sprintf("%s/DataMart/%s/derived/progress note extraction" , datadrive, datadir)



# was "encounter2mrn"
keyword2patient <- function(prefix="", filebase, datadir) {
	if (!require(readr)) stop("readr not available")
	
	suppressMessages(dat <- read_csv(sprintf("Filtered Data/Intermediate Data/%s/%s.csv", datadir, filebase), col_types = cols(PAT_MRN="c")))
	
	posdate <- aggregate(NEGATION_COUNT ~ PAT_MRN + CONTACT_DATE, data=dat, FUN=function(x) sum(x==0))
	names(posdate)[3] <- "n_positive"
	
	if (sum(posdate$n_positive>0)) {
		firstposdate <- aggregate(CONTACT_DATE ~ PAT_MRN, data=posdate, subset=n_positive>0, FUN=function(x) min(x))
		names(firstposdate)[2] <- "first_dt"
		lastposdate  <- aggregate(CONTACT_DATE ~ PAT_MRN, data=posdate, subset=n_positive>0, FUN=function(x) max(x))
		names( lastposdate)[2] <- "last_dt"
		nposdate     <- aggregate(CONTACT_DATE ~ PAT_MRN, data=posdate, subset=n_positive>0, FUN=function(x) length(x))
		names(nposdate)[2] <- "n_dt"
		
		firstlastposdate <- merge(firstposdate, lastposdate, by="PAT_MRN", all=TRUE)
		firstlastnposdate <- merge(firstlastposdate, nposdate, by="PAT_MRN", all=TRUE)
	} else {
		firstlastnposdate <- structure(list(PAT_MRN = character(0),
																				first_dt = structure(numeric(0), class = "Date"), 
																				last_dt = structure(numeric(0), class = "Date"),
																				n_dt = numeric(0)), row.names = integer(0), class = "data.frame")
	}
	
	posmrn <- aggregate(NEGATION_COUNT ~ PAT_MRN, data=dat, FUN=function(x) sum(x==0))
	names(posmrn)[2] <- "n_positive"
	negmrn <- aggregate(NEGATION_COUNT ~ PAT_MRN, data=dat, FUN=function(x) sum(x> 0))
	names(negmrn)[2] <- "n_negative"
	
	posnegmrn <- merge(posmrn, negmrn, by="PAT_MRN", all=TRUE)
	
	outdf <- merge(firstlastnposdate, posnegmrn, by="PAT_MRN", all=TRUE)
	outdf$n_dt[is.na(outdf$n_dt)] <- 0
	
	names(outdf)[2:6] <- paste0(prefix, names(outdf)[2:6])
	
	return(outdf)
}


keyword2encounter <- function(prefix="", filebase, datadir) {
	# output includes mrn, csn, date, n_positive, n_negative
	if (!require(readr)) stop("readr not available")

	suppressMessages(dat_kw <- read_csv(sprintf("Filtered Data/Intermediate Data/%s/%s.csv", datadir, filebase), col_types = cols(PAT_MRN="c")))
	
	pos <- aggregate(NEGATION_COUNT ~ PAT_MRN + PAT_ENC_CSN_ID, data=dat_kw, FUN=function(x) sum(x==0))
	names(pos)[3] <- "n_positive"
	neg <- aggregate(NEGATION_COUNT ~ PAT_MRN + PAT_ENC_CSN_ID, data=dat_kw, FUN=function(x) sum(x> 0))
	names(neg)[3] <- "n_negative"
	date <- aggregate(CONTACT_DATE ~ PAT_MRN + PAT_ENC_CSN_ID, data=dat_kw, FUN=function(x) min(x)) # should all be the same for a csn
	
	posneg <- merge(pos, neg, by=c("PAT_MRN", "PAT_ENC_CSN_ID"))
	posnegdate <- merge(date, posneg, by=c("PAT_MRN", "PAT_ENC_CSN_ID"))

	return(posnegdate)
}


# # examples
# 
# keyword <- "disc_heme"
# 
# df_encounter <- keyword2encounter(prefix="", filebase=sprintf("%s_with_neg_not_precaution", keyword), datadir=datadir)
# 
# write_csv(df_encounter, path=sprintf("%s/%s_encounter.csv", pnedir, keyword), na="")
# 
# df_patient <- keyword2patient  (prefix="", filebase=sprintf("%s_with_neg_not_precaution", "disc_heme"), datadir=datadir)
# 
# write_csv(df_patient, path=sprintf("%s/%s_patient.csv", pnedir, keyword), na="")
# 
# 
# #### for Moshiur
# keyword <- "pxf"
# 
# df_encounter <- keyword2encounter(prefix="", filebase=sprintf("%s_with_neg_not_precaution", keyword), datadir=datadir)
# 
# write_csv(df_encounter, path=sprintf("%s/%s_encounter.csv", pnedir, keyword), na="")
# 
# df_patient <- keyword2patient  (prefix="", filebase=sprintf("%s_with_neg_not_precaution", keyword), datadir=datadir)
# 
# write_csv(df_patient, path=sprintf("%s/%s_patient.csv", pnedir, keyword), na="")


# #### for Moshiur (2019 02 06)
# # has 2 versions
# keyword <- "mac_ed"
# 
# df_encounter <- keyword2encounter(prefix="", filebase=sprintf("%s_with_neg_not_precaution", keyword), datadir=datadir)
# write_csv(df_encounter, path=sprintf("%s/%s_encounter.csv", pnedir, keyword), na="")
# 
# df_patient <- keyword2patient  (prefix="", filebase=sprintf("%s_with_neg_not_precaution", keyword), datadir=datadir)
# write_csv(df_patient, path=sprintf("%s/%s_patient.csv", pnedir, keyword), na="")
# 
# df_encounter <- keyword2encounter(prefix="", filebase=sprintf("%s_outcome", keyword), datadir=datadir)
# write_csv(df_encounter, path=sprintf("%s/%s_outcome_encounter.csv", pnedir, keyword), na="")
# 
# df_patient <- keyword2patient  (prefix="", filebase=sprintf("%s_outcome", keyword), datadir=datadir)
# write_csv(df_patient, path=sprintf("%s/%s_outcome_patient.csv", pnedir, keyword), na="")


# 'allkeyfiles' defined previously (see 'extract terms from progress notes.R')
allkeyfiles <- c(allkeyfiles, "mac_ed_outcome")

for (keyfile in allkeyfiles) {
	cat(sprintf("%s: %s\n", date(), keyfile))
	df_encounter <- keyword2encounter(prefix="", filebase=sprintf("%s_with_neg_not_precaution", keyfile), datadir=datadir)
	write_csv(df_encounter, path=sprintf("%s/%s_encounter.csv", pnedir, keyfile), na="")
	df_patient <- keyword2patient  (prefix="", filebase=sprintf("%s_with_neg_not_precaution", keyfile), datadir=datadir)
	write_csv(df_patient, path=sprintf("%s/%s_patient.csv", pnedir, keyfile), na="")
	invisible(NULL)
}
