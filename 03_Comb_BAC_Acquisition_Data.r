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
setwd("/Users/alexstephens/Development/fnma/data/bac")
bacDirectory <- "/Users/alexstephens/Development/fnma/data/bac"

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
    
    ## after the first step, union files
    if (i == 1) {
        Acquisitions_Data.bac.all <- Acquisitions_Data.bac
    } else {
        Acquisitions_Data.bac.all <- rbind(Acquisitions_Data.bac.all, Acquisitions_Data.bac)
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
save(Acquisitions_Data.bac.all, file=paste0(bacDirectory,"/","Acquisitions_Data_BAC_All.Rda"))


#########################################################################


##-----------------------------------------------------------------------
## Tabulate basic stats
##-----------------------------------------------------------------------

##load(paste0(bacDirectory,"/","Acquisitions_Data_BAC_All.Rda"))

## create a numeric origination year
Acquisitions_Data.bac.all$ORIG_YY <- as.integer(Acquisitions_Data.bac.all[,substr(ORIG_DTE,4,8)])

## compute yearly (count, UPB) total to cross-check against FNMA acquisition stats
ORIG_UPB_YY_MM  <- xtabs(ORIG_AMT ~ ORIG_YY , data=Acquisitions_Data.bac.all[,])/1e06
LOAN_CNT_YY     <- Acquisitions_Data.bac.all[,table(ORIG_YY)]

## tabule categorical variables by origination year
Acquisitions_Data.bac.all[,table(ORIG_CHN,ORIG_YY)]
Acquisitions_Data.bac.all[,table(Seller.Name,ORIG_YY)]
Acquisitions_Data.bac.all[,table(ORIG_TRM,ORIG_YY)]
Acquisitions_Data.bac.all[,table(NUM_BO,ORIG_YY)]
Acquisitions_Data.bac.all[,table(FTHB_FLG,ORIG_YY)]
Acquisitions_Data.bac.all[,table(PURPOSE,ORIG_YY)]
Acquisitions_Data.bac.all[,table(PROP_TYP,ORIG_YY)]
Acquisitions_Data.bac.all[,table(NUM_UNIT,ORIG_YY)]
Acquisitions_Data.bac.all[,table(OCC_STAT,ORIG_YY)]
Acquisitions_Data.bac.all[,table(STATE,ORIG_YY)]

## tabulate NA totals for quasi-numeric variables by origination year
Acquisitions_Data.bac.all[,table(is.na(ORIG_RT))]
Acquisitions_Data.bac.all[,table(is.na(ORIG_AMT))]
Acquisitions_Data.bac.all[,table(is.na(OLTV))]
Acquisitions_Data.bac.all[,table(is.na(DTI))]
Acquisitions_Data.bac.all[,table(is.na(CSCORE_B))]
Acquisitions_Data.bac.all[,table(is.na(CSCORE_C))]
Acquisitions_Data.bac.all[,table(NUM_BO, is.na(CSCORE_C))]
Acquisitions_Data.bac.all[,table(is.na(ZIP_3))]

