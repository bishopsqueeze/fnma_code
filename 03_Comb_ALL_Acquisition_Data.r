##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## clean the cache
##-----------------------------------------------------------------------
#rm(list=ls())

##-----------------------------------------------------------------------
## load libraries
##-----------------------------------------------------------------------
source("/Users/alexstephens/Development/fnma/fnma_code/99_Load_Libraries.r")

##------------------------------------------------------------------
## Define the parallel flag
##------------------------------------------------------------------
DOPARALLEL  <- TRUE

##------------------------------------------------------------------
## Register the clusters
##------------------------------------------------------------------
if (DOPARALLEL) {
    library(doMC)
    registerDoMC(2)
}

##-----------------------------------------------------------------------
## set the working directory
##-----------------------------------------------------------------------
setwd("/Users/alexstephens/Development/fnma/data/proc")
outDirectory <- "/Users/alexstephens/Development/fnma/data/bac/03_Processed_ALL_Acquisition_Data"

##-----------------------------------------------------------------------
## get the acquisitions file list
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Acquisitions_Data.Rda$")
file.num    <- length(file.list)

##-----------------------------------------------------------------------
## loop over all files and collect the universe of sellers
##-----------------------------------------------------------------------
for (i in 1:file.num)
{
    tmp.file    <- file.list[i]
    tmp.hdr     <- substr(tmp.file, 1, 6)
    load(tmp.file)
    
    ## echo progress
    message("processing file :: ", tmp.file)

    ## after the first step, union files
    if (i == 1) {
        Acquisitions_Data.all <- Acquisitions_Data
    } else {
        Acquisitions_Data.all <- rbindlist( list(Acquisitions_Data.all, Acquisitions_Data), use.names=TRUE, fill=TRUE)
    }
    
    ## remove the loaded acquisitions file
    rm("Acquisitions_Data")
}


##-----------------------------------------------------------------------
## Define the data.table key (LOAN_ID) in the merged data
##-----------------------------------------------------------------------
setkey(Acquisitions_Data.all, LOAN_ID)

##-----------------------------------------------------------------------
## Save the merged data
##-----------------------------------------------------------------------
save(Acquisitions_Data.all, file=paste0(outDirectory,"/","Acquisitions_Data_All.Rda"))
