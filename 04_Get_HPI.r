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
setwd("/Users/alexstephens/Development/fnma/data/hpi")
hpiDirectory <- "/Users/alexstephens/Development/fnma/data/hpi"

##-----------------------------------------------------------------------
## get the base BAC default file list
##-----------------------------------------------------------------------
hpi.file    <- "HPI_AT_3zip.csv"
hpi.out     <- "HPI_AT_3zip.Rda"


##-----------------------------------------------------------------------
## Step 1:  Load the HPI data
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## read the raw data
##-----------------------------------------------------------------------
hpi.raw             <- read.csv(file=hpi.file, header=TRUE, colClasses=c("character", "numeric", "numeric", "numeric", "character"))[,1:5]
colnames(hpi.raw)   <- c("ZIP_3", "Year", "Qtr", "Index", "Index.Type")

##-----------------------------------------------------------------------
## create a date field; Order the results
##-----------------------------------------------------------------------
hpi                 <- as.data.table(hpi.raw)
hpi$HPI_DTE         <- as.Date(paste0(3*hpi$Qtr,"/01/",hpi$Year), "%m/%d/%Y")       ## origination date
hpi$Key             <- paste(hpi$ZIP_3, paste(hpi$Year, hpi$Qtr, sep=""), sep="")   ## key

## define keys
setorderv(hpi, c("ZIP_3", "HPI_DTE"))
setkey(hpi, "Key")

##-----------------------------------------------------------------------
## drop redundant variables and set the key
##-----------------------------------------------------------------------
hpi[,c("Index.Type") := NULL]



##-----------------------------------------------------------------------
## Step 2:  Determine where BAC data do not have ZIP_3 HPI data
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## Load the BAC acquisitions data;  Will use to map ZIP_3 to State
##-----------------------------------------------------------------------
bacDirectory <- "/Users/alexstephens/Development/fnma/data/bac"
load(paste0(bacDirectory,"/","Acquisitions_Data_BAC_All.Rda"))


## tabulate the two sets of zip3
bac.zip3    <- Acquisitions_Data.bac.all[,table(ZIP_3)]
hpi.zip3    <- hpi[,table(ZIP_3)]

## compare the two sets of ZIP_3 data
diff.bac2hpi.zip3   <- setdiff(Acquisitions_Data.bac.all[,unique(ZIP_3)],hpi[,unique(ZIP_3)])
#diff.hpi2bac.zip3   <- setdiff(hpi[,unique(ZIP_3)],Acquisitions_Data.bac.all[,unique(ZIP_3)]) ## All hpi in bac

## total number of mis-matches
bac.zip3.mismatch.cnt   <- bac.zip3[which(names(bac.zip3) %in% diff.bac2hpi.zip3)]
bac.zip3.mismatch.names <- names(bac.zip3)[which(names(bac.zip3) %in% diff.bac2hpi.zip3)]

message("mismatch rate [%] = ", sum(bac.zip3.mismatch.cnt) / nrow(Acquisitions_Data.bac.all)*100)

##-----------------------------------------------------------------------
## save
##-----------------------------------------------------------------------
save(hpi, bac.zip3.mismatch.names, bac.zip3, hpi.zip3, file=paste0(hpiDirectory,"/",hpi.out))


