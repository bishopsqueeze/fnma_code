##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## clean the cache
##-----------------------------------------------------------------------
rm(list=ls())

##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------

message("Running script :: 02_Get_BAC_Acquisition_Data.r")
source("/Users/alexstephens/Development/fnma/fnma_code/02_Get_BAC_Acquisition_Data.r")

#message("Running script :: 03_Comb_ALL_Acquisition_Data.r")
#source("/Users/alexstephens/Development/fnma/fnma_code/03_Comb_ALL_Acquisition_Data.r")

message("Running script :: 03_Comb_BAC_Acquisition_Data.r")
source("/Users/alexstephens/Development/fnma/fnma_code/03_Comb_BAC_Acquisition_Data.r")

message("Running script :: 04_Get_HPI.r")
source("/Users/alexstephens/Development/fnma/fnma_code/04_Get_HPI.r")

message("Running script :: 05_Get_Unemp.r")
source("/Users/alexstephens/Development/fnma/fnma_code/05_Get_Unemp.r")

message("Running script :: 06_Get_BAC_Performance_Data.r")
source("/Users/alexstephens/Development/fnma/fnma_code/06_Get_BAC_Performance_Data.r")
