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
## <function> :: csiPlot
##-----------------------------------------------------------------------
csiPlot  <- function(dt, filtvar, bktvar, datevar="ORIG_DTE")
{
    miss.pct <- 100*dt[is.na(eval(parse(text=filtvar))),table(eval(parse(text=datevar)))]/nrow(dt)   ## small
    
    ## bucket count/proportions by date
    a           <- dt[!is.na(eval(parse(text=filtvar))),table(eval(parse(text=datevar)), eval(parse(text=bktvar)))]
    b           <- 100*(a/rowSums(a))
    
    ## compute global means
    c           <- 0*b
    tmp.c       <- dt[!is.na(eval(parse(text=filtvar))),table(eval(parse(text=bktvar)))]
    tmp.c       <- tmp.c / sum(tmp.c)
    for (i in 1:nrow(c)) { c[i,] <- 100*tmp.c }
    
    ## compute WoE & IV
    d           <- (b-c)*log(b/c)
    e           <- ifelse(is.infinite(d),NA,d)
    f           <- apply(e, 1, sum, na.rm=TRUE)
    
    ## append additional info
    g           <- as.vector(dt[!is.na(eval(parse(text=filtvar))),table(eval(parse(text=datevar)))])
    h           <- (b-c)
    colnames(h) <- paste0("d",colnames(h))
    
    ## tabulate results
    tbl         <- as.data.table(as.data.frame(cbind(b,h,iv=f,cnt=g)))
    tbl$date    <- as.Date(rownames(b))
    lp          <- ggplot(data=tbl[which(date >= "2000-01-01")], aes(x=date, y=iv)) + geom_line() + geom_point()
    
    return(list(miss=miss.pct, tbl=tbl, lp=lp))
}

##-----------------------------------------------------------------------
## set the working directory
##-----------------------------------------------------------------------
setwd("/Users/alexstephens/Development/fnma/data/bac")

##-----------------------------------------------------------------------
## load the acquisition data
##-----------------------------------------------------------------------

## load the file
file.in     <- dir(path=".", pattern="Acquisitions_Data_BAC_All.Rda$")
load(file.in)

## rename the data.table & drop the original
Data_A  = Acquisitions_Data.bac.all
rm("Acquisitions_Data.bac.all")

##-----------------------------------------------------------------------
## clean and add variables
##-----------------------------------------------------------------------

## Convert character variables to Date type
Data_A$ORIG_DTE <- as.Date(ifelse(Data_A$ORIG_DTE == "", "", paste0(substr(Data_A$ORIG_DTE,1,2),"/01/",substr(Data_A$ORIG_DTE,4,8))), "%m/%d/%Y")   ## origination date
Data_A$FRST_DTE <- as.Date(ifelse(Data_A$FRST_DTE == "", "", paste0(substr(Data_A$FRST_DTE,1,2),"/01/",substr(Data_A$FRST_DTE,4,8))), "%m/%d/%Y")   ## origination date

## Create buckets for continuous attributes and group number of borrowers
Data_A[,c("OcltvBkt", "OltvBkt", "FicoBkt", "DtiBkt", "OrigAmtBkt", "NumBoBkt") :=list(
    as.character(cut(OCLTV, breaks = c(-Inf, 0, 60, 65, 70, 75, 80, 85, 90, 97, Inf),labels = c('NA', '(0-60]', '(60-65]', '(65-70]', '(70-75]', '(75-80]', '(80-85]', '(85-90]', '(90-97]', '(97+)'), right = TRUE, ordered = TRUE)),
    as.character(cut(OLTV, breaks = c(-Inf, 0, 60, 65, 70, 75, 80, 85, 90, 97, Inf),labels = c('NA', '(0-60]', '(60-65]', '(65-70]', '(70-75]', '(75-80]', '(80-85]', '(85-90]', '(90-97]', '(97+)'), right = TRUE, ordered = TRUE)),
    as.character(cut(CSCORE_MN, breaks = c(-Inf, 0, 620, 660, 700, 740, 780, Inf), labels = c('NA','[0-620)', '[620-660)', '[660-700)', '[700-740)', '[740-780)', '[780+)'), right = FALSE, ordered = TRUE)),
    as.character(cut(DTI, breaks = c(-Inf, 0, 20, 30, 40, 45, Inf), labels = c('NA', '[0-20)', '[20-30)', '[30-40)', '[40-45)', '[45+)'), right = FALSE, ordered = TRUE)),
    as.character(cut(ORIG_AMT, breaks = c(-Inf, 0, 85000, 110000, 125000, 150000, 175000, 200000, 417000, Inf), labels = c('NA', '[0-85k)', '[85k-110k)', '[110k-125k)', '[125k-1500k)', '[150k-175k)', '[175k-200k)', '[200k-417k)', '[417k+)'), right = FALSE, ordered = TRUE)),
    as.character(as.character(ifelse(NUM_BO=="","Missing", ifelse(!(NUM_BO %chin% c("1","2")), "3+", NUM_BO))))
)]


##-----------------------------------------------------------------------
## generate continuous variable IV plots
##-----------------------------------------------------------------------
tmp.fico    <- csiPlot(Data_A, filtvar="CSCORE_MN", bktvar="FicoBkt", datevar="ORIG_DTE")
tmp.ocltv   <- csiPlot(Data_A, filtvar="OCLTV", bktvar="OcltvBkt", datevar="ORIG_DTE")
tmp.dti     <- csiPlot(Data_A, filtvar="DTI", bktvar="DtiBkt", datevar="ORIG_DTE")
tmp.bal     <- csiPlot(Data_A, filtvar="ORIG_AMT", bktvar="OrigAmtBkt", datevar="ORIG_DTE")
tmp.state   <- csiPlot(Data_A[!which(STATE %chin% c("GU","VI")),], filtvar="STATE", bktvar="STATE", datevar="ORIG_DTE")

tmp.purp   <- csiPlot(Data_A, filtvar="PURPOSE",  bktvar="PURPOSE",  datevar="ORIG_DTE")
tmp.prop   <- csiPlot(Data_A, filtvar="PROP_TYP", bktvar="PROP_TYP", datevar="ORIG_DTE")
tmp.fthb   <- csiPlot(Data_A, filtvar="FTHB_FLG", bktvar="FTHB_FLG", datevar="ORIG_DTE")
tmp.occ    <- csiPlot(Data_A, filtvar="OCC_STAT", bktvar="OCC_STAT", datevar="ORIG_DTE")
tmp.chan   <- csiPlot(Data_A, filtvar="ORIG_CHN", bktvar="ORIG_CHN", datevar="ORIG_DTE")
## ORIG_RT


#multiplot(lp.fico, lp.ocltv, lp.dti)




