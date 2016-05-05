##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## clean the cache
##-----------------------------------------------------------------------
rm(list=ls())

##-----------------------------------------------------------------------
## set-up scripts to run
##-----------------------------------------------------------------------
script.dir  <- "/Users/alexstephens/Development/fnma/fnma_code/"

script.list <- c(   #"02_Get_BAC_Acquisition_Data.r",
                    #"03_Comb_BAC_Acquisition_Data.r",
                    #"04_Get_HPI.r",
                    #"05_Get_Unemp.r",
                    #"06_Get_BAC_Performance_Data.r",
                    #"07_Split_BAC_Performance_Data_By_Date.r",
                    #"08_Build_BAC_Performance_Data_Cohorts.r",
                    #"09_Split_BAC_Performance_Data_Cohorts.r",
                    #"20_Create_BAC_Performance_Data_Odad_NonOverlap_Picklists.r",
                    "21_Define_Odad_NonOverlap_Samples.r",
                    "22_Create_Odad_Qtr_Reference_Data.r"
                    )

script.num  <- length(script.list)

##-----------------------------------------------------------------------
## loop over each script; source it and then clean-up
##-----------------------------------------------------------------------
for (i in 1:script.num)
{
    ## process each script
    message("Running script ",i," of ",script.num," --> ", script.list[i])
    source(paste0(script.dir,script.list[i]))
    
    ## remove non-script objects from memory and
    clean.list  <- ls()
    rm(list=clean.list[-grep("script", clean.list)])
}
