# Search for key terms in encounter notes (aka progress notes)

#
# Chris Andrews
# 2018 05 07
# 2018 09 07 moved from EMERSE to R code directory
#   made more modular
# 2021 03 01: focused on "x-linked" over time.


# this function is found in "load progress notes.R"
source("./R Code/load progress notes.R")

source("./R Code/extract functions.R")

source("./R Code/negation precaution history functions.R")

datadrive <- "J:/EPIC-Ophthalmology"

datadirs <-  c(
# # "2012 Aug 01- 2018 Apr 30" ,
# # "2012 Aug 01- 2018 Aug 31" ,
# # "2012 Aug 01- 2018 Feb 28" ,
# # "2012 Aug 01- 2018 Jun 30" ,
# # "2012 Aug 01- 2018 Mar 31" ,
# # "2012 Aug 01- 2018 May 31" ,
# # "2012 Aug 01 - 2017 Dec 31",
# # "2012 Aug 01 - 2018 Nov 30",
# # "2012 Aug 01 - 2018 Oct 31",
# # "2012 Aug 01 - 2018 Sep 30",
"2019 Apr 01 - 2019 Apr 30",
"2019 Aug 01 - 2019 Aug 31",
"2019 Dec 01 - Dec 31"     ,
"2019 Feb 01 - 2019 Feb 28",
"2019 Jan 01 - 2019 Jan 31",
"2019 Jul 01 - 2019 Jul 31",
"2019 Jun 01 - 2019 Jun 30",
"2019 Mar 01 - 2019 Mar 31",
"2019 May 01 - 2019 May 31",
"2019 Nov 01 - 2019 Nov 30",
"2019 Oct 01 - 2019 Oct 31",
"2019 Sep 01 - 2019 Sep 30",
"2020 Apr 01 - Apr 30"     ,
"2020 Aug 01 - Aug 31"     ,
"2020 Feb 01 - Feb 29"     ,
"2020 Jan 01 - Jan 31"     ,
"2020 Jul 01 - 2020 Jul 31",
"2020 Jun 01 - 2020 Jun 30",
"2020 Mar 01 - Mar 31"     ,
"2020 May 01 - May 31"     ,
"2020 Oct 01 - Nov 30"     ,
"2020 Sep 01 - Sep 30",
"2012 Aug 01 - 2018 Dec 31"
)

# datadir <- datadirs[23]

for (datadir in datadirs[1:22]) {
	
	cat("datadir =", datadir, "\n")
	
	dat_pn <- if (datadir == "2019 Aug 01 - 2019 Aug 31") {
		loadprogressnotes(datadrive, sprintf("%s/all", datadir), verbose=1)
	} else if (datadir == "2019 Dec 01 - Dec 31") {
		loadprogressnotes(datadrive, datadir, verbose=1, skip=12, usefread = FALSE)
	} else if (datadir == "2019 Jul 01 - 2019 Jul 31") {
		loadprogressnotes(datadrive, sprintf("%s/Temp", datadir), verbose=1)
	} else if (datadir == "2012 Aug 01 - 2018 Dec 31") {
		loadprogressnotes(datadrive, datadir, dataext=" 2018 Jan 01- 2018 Dec 31", verbose=1, splitformat = TRUE)
	} else {
		loadprogressnotes(datadrive, datadir, dataext=dataext, verbose=1)
	}
	
	dir1 <- sprintf("%s/Andrews Projects/Text Analytics/x-linked/%s", datadrive, datadir)
	
	
	if (!dir.exists(dir1)) { # check if directory exists for output
		cat("Creating", dir1, "\n")
		if (!dir.create(dir1, recursive = TRUE)) stop(sprintf("Failed to create %s", dir1))
	}
	
	
	
	system.time(dat_ext <- extracts(pats=c("XLRP", "achromatopsia", "x[ -]*linked[ -]*(RP|retinitis([ -]*pigmentosa)?)"), filebase="x-linked", out=dir1))
	# summary(dat_ext$MATCHED_TEXT)
	
	
	###############################################
	# ADD NEGATION INFORMATION TO THE ABOVE FILES #
	###############################################
	
	# find all negations, precaution terms, history references #
	
	dat_neg <- findnegations(dir1, 0)
	dat_pc <- findprecautions(dir1, 0)
	dat_hx <- findhistory(dir1, 0)
	
	# add negation information, remove precautions and histories
	addnegationinfo("x-linked", dir=dir1)
	removeprecautions("x-linked", dir=dir1)
	removehistories("x-linked", dir=dir1)
}






# Extract keywords
# XLRP, x-linked RP, x-linked retinitis, achromatopsia
# research
# system.time(dat_ext <- extracts(pats=c("X[ -]*L[ -]*R[ -]*P", "achromatopsia", "x[ -]*linked.{24}"), filebase=NULL))
# table(dat_ext$MATCHED_TEXT)
# dat_ext
# # mmm <- aregexec("\\bachromatopsia", dat_pn$PROGRESS_NOTE)
# mmm <- aregexec("achromatopsia", dat_pn$PROGRESS_NOTE)
# mmm <- regexec(".{5}chromato.{5}", dat_pn$PROGRESS_NOTE)
# ttt <- regmatches(dat_pn$PROGRESS_NOTE, mmm)
# ttt[sapply(ttt, length) > 0]
# # "dyschromatopsia" also?  No.
