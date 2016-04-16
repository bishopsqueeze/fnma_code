##-----------------------------------------------------------------------
## FNMA Raw Data Load Script
##-----------------------------------------------------------------------
## Note:
##  - Two of the files 2003Q2, 2003Q3 would error-out on the unzip due to
##    extra text at the head of the file.
##  - I could unzip the files manually and place the output A/P files into
##    the data/zips directory and then step through the loop at the end
##    of the script to process the files.
##
##-----------------------------------------------------------------------
## >  procRawData(zipDirectory, outDirectory, tmpFile, tmpHdr)
##  Archive:  2003Q2.zip
##  warning [2003Q2.zip]:  76 extra bytes at beginning or within zipfile
##  (attempting to process anyway)
##  error [2003Q2.zip]:  reported length of central directory is
##  -76 bytes too long (Atari STZip zipfile?  J.H.Holm ZIPSPLIT 1.1
##  zipfile?).  Compensating...
##  skipping: Performance_2003Q2.txt  need PK compat. v4.5 (can do v2.1)
##  replace Acquisition_2003Q2.txt? [y]es, [n]o, [A]ll, [N]one, [r]ename:  NULL
##  (assuming [N]one)
##
##  note:  didn't find end-of-central-dir signature at end of central dir.
##  (please check that you have transferred or created the zipfile in the
##  appropriate BINARY mode and that you have compiled UnZip properly)
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
## Load FNMA raw data attributes
##-----------------------------------------------------------------------
source("/Users/alexstephens/Development/fnma/code/99_FNMA_Raw_Data_Attributes.r")

##-----------------------------------------------------------------------
## <function> :: procRawData
##-----------------------------------------------------------------------
procRawData <- function(myZipDirectory, myOutDirectory, myZipFile, myHeader)
{
    ## Set the directory
    setwd(myZipDirectory)
    
    ## Unzip the raw data file
    myUnzipCmd  <- paste("/usr/bin/unzip", myZipFile, sep=" ")
    system(myUnzipCmd)

    ## Get a file/core count
    myNumFiles  <- length(list.files(myZipDirectory, pattern = glob2rx("*txt"), full.names=FALSE))
    myNumCores  <- (myNumFiles/2)

    ## Read Acquisition/Performance files
    Acquisitions    <- list.files(myZipDirectory, pattern = glob2rx("*Acquisition*txt"), full.names=FALSE)
    Performance     <- list.files(myZipDirectory, pattern = glob2rx("*Performance*txt"), full.names=FALSE)

    #Close Connections created as result of Running Foreach
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)

    ## Load the acquisitions data
    Acquisitions_Data <-
        foreach(k=1:myNumCores, .inorder=FALSE, .combine=rbind, .packages=c("data.table")) %do%
    {
        Data_A <- fread(Acquisitions[k], sep = "|", colClasses=Acquisition_ColClasses, showProgress=FALSE)
        setnames(Data_A, Acquisitions_Variables)
        setkey(Data_A, "LOAN_ID")
    }

    ## Load the acquisitions data
    Performance_Data <-
        foreach(k=1:myNumCores, .inorder=FALSE, .combine=rbind, .packages=c("data.table")) %do%
    {
        Data_P <- fread(Performance[k], sep = "|", colClasses=Performance_ColClasses, showProgress=FALSE)
        setnames(Data_P, Performance_Variables)
        setkey(Data_P, "LOAN_ID")
    }
    
    ## Save the loaded data as Rda files
    save(Acquisitions_Data, file=paste0(myOutDirectory,"/",myHeader,"_Acquisitions_Data.Rda"))
    save(Performance_Data, file=paste0(myOutDirectory,"/",myHeader,"_Performance_Data.Rda"))

    ## Remove unzipped files
    system(paste0("rm ",Acquisitions[1]))
    system(paste0("rm ",Performance[1]))

    ## Clean-up R cache
    rm(list= ls()[!(ls() %in% c('Acquisitions_Data', 'Performance_Data'))])
}


##-----------------------------------------------------------------------
## <main>
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## Define the raw zip'd file directory
##-----------------------------------------------------------------------
zipDirectory    <- "/Users/alexstephens/Development/fnma/data/zips"
outDirectory    <- "/Users/alexstephens/Development/fnma/data/proc"

##-----------------------------------------------------------------------
## Get a list of zip files
##-----------------------------------------------------------------------
zipFiles        <- list.files(zipDirectory, pattern = glob2rx("*zip"), full.names=FALSE) #full.names=TRUE)
numZipFiles     <- length(zipFiles)

##-----------------------------------------------------------------------
## Loop over all zip files and process into Rdata
##-----------------------------------------------------------------------
for (i in 1:numZipFiles)
{
    ## set the working direcotry
    setwd(zipDirectory)
    
    ## isolate file
    tmpFile <- zipFiles[i]
    tmpHdr  <- substr(tmpFile,1,6)
    
    ## process the raw data
    procRawData(zipDirectory, outDirectory, tmpFile, tmpHdr)
}





