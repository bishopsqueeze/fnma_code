##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## load libraries
##-----------------------------------------------------------------------
source("/Users/alexstephens/Development/fnma/code/99_Load_Libraries.r")

##-----------------------------------------------------------------------
## clean the cache
##-----------------------------------------------------------------------
rm(list=ls())

##-----------------------------------------------------------------------
## set the working directory
##-----------------------------------------------------------------------
setwd("/Users/alexstephens/Development/fnma/data/unemp")
unempDirectory <- "/Users/alexstephens/Development/fnma/data/unemp"
unemp.out      <- "UNEMP_AT_STATE.Rda"
##-----------------------------------------------------------------------
## Step 1:  Load the HPI data
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## read the raw data
##-----------------------------------------------------------------------
la.data     <- as.data.table(read.csv(file="la.data.3.AllStatesS", sep="", header=TRUE))   ## raw data (several series)
la.series   <- as.data.table(read.csv(file="la.series_small.csv", sep=",", header=TRUE))   ## map between rate series and state


##-----------------------------------------------------------------------
## clean series_id and define the key
##-----------------------------------------------------------------------
la.data[, series_id := as.character(series_id)]
setkey(la.data, series_id)

la.series[, c("series_id","state_cd") := list(as.character(series_id), as.character(state_code))]
la.series[, series_id := gsub(" ","",series_id,fixed=TRUE)]
setkey(la.series, series_id)

##-----------------------------------------------------------------------
## isolate unemployment rate series; append state code
##-----------------------------------------------------------------------
la.rate     <- la.data[which(series_id %in% la.series[,series_id]), ]
la.rate[la.series, state_cd := i.state_cd]
la.rate[, footnote_codes := NULL]
la.rate[, period := as.integer(gsub("M","", period))]
la.rate[, OBS_DTE := as.Date(paste0(la.rate$period,"/01/",la.rate$year), "%m/%d/%Y")]

##-----------------------------------------------------------------------
## create the final seried; define a key
##-----------------------------------------------------------------------
unemp = la.rate
unemp[ , c("series_id") := NULL]
unemp[ , Key := paste0(year,period,state_cd)]
setkey(unemp, Key)

##-----------------------------------------------------------------------
## save
##-----------------------------------------------------------------------
save(unemp, file=paste0(unempDirectory,"/",unemp.out))


