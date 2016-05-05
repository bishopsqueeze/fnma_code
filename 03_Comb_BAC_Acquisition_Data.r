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
setwd("/Users/alexstephens/Development/fnma/data/bac/02_Processed_Acquisition_Data")
outDirectory <- "/Users/alexstephens/Development/fnma/data/bac/03_Processed_BAC_Acquisition_Data"

##-----------------------------------------------------------------------
## get the acquisitions file list
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Acquisitions_Data_BAC.Rda$")
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
        Acquisitions_Data.bac.all <- Acquisitions_Data.bac
    } else {
        Acquisitions_Data.bac.all <- rbindlist( list(Acquisitions_Data.bac.all, Acquisitions_Data.bac), use.names=TRUE, fill=TRUE)
    }
    
    ## remove the loaded acquisitions file
    rm("Acquisitions_Data.bac")
}

##-----------------------------------------------------------------------
## Define the data.table key (LOAN_ID) in the merged data
##-----------------------------------------------------------------------
setkey(Acquisitions_Data.bac.all, LOAN_ID)

##-----------------------------------------------------------------------
## Save the merged data
##-----------------------------------------------------------------------
save(Acquisitions_Data.bac.all, file=paste0(outDirectory,"/","Acquisitions_Data_BAC_All.Rda"))

