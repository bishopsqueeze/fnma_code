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
bacDirectory <- "/Users/alexstephens/Development/fnma/data/bac"
outDirectory <- "/Users/alexstephens/Development/fnma/data/bac/02_Processed_Acquisition_Data"

##-----------------------------------------------------------------------
## define the bank to isolate
##-----------------------------------------------------------------------
bank_fullName   <- "BANK OF AMERICA, N.A."
bank_id         <- "BAC"

##-----------------------------------------------------------------------
## get the acquisitions file list
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## the reason for the filter on the file list ([1:48]) is because BAC stopped
## selling loans to FNMA in 2012.  So, there is no reason to process the
## raw files holding acquistion data dating from 2012 and onward
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Acquisitions_Data.Rda$")[1:48]
file.num    <- length(file.list)

##-----------------------------------------------------------------------
## loop over all files and collect the universe of sellers
##-----------------------------------------------------------------------
#for (i in 1:file.num) {
foreach (i=1:file.num) %dopar% {

    tmp.file    <- file.list[i]
    tmp.hdr     <- substr(tmp.file, 1, 6)
    tmp.out     <- paste0(tmp.hdr, "_Acquisitions_Data_BAC.Rda")
    load(tmp.file)
    
    ## echo progress
    message("processing file :: ", tmp.file)

    ## Extract the acquisition data
    Acquisitions_Data.bac <- Acquisitions_Data[Seller.Name == bank_fullName]
    
    ## Delete unnecessary Acquisition variables.
    Acquisitions_Data.bac[,c("Seller.Name","Product.Type"):=NULL]
    
    ##-----------------------------------------------------------------------
    ##  Define minimum Fico Score of the Borrower and Co-Borrower
    ##  Calculate House Price, and
    ##  Replace Missing OCLTV values with OLTV values where available
    ##-----------------------------------------------------------------------
    Acquisitions_Data.bac[ ,c("CSCORE_MN", "ORIG_VAL", "OCLTV"):= list(
                                    pmin(CSCORE_B, CSCORE_C, na.rm = TRUE),
                                    (ORIG_AMT/(OLTV/100)),
                                    ifelse(is.na(OCLTV), OLTV, OCLTV))]

    ## save the subsetted data
    save(Acquisitions_Data.bac, file=paste0(outDirectory,"/",tmp.out))
    
    ## clean the workspace
    rm("Acquisitions_Data","Acquisitions_Data.bac")
}

