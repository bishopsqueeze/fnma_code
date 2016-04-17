##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## clean the cache
##-----------------------------------------------------------------------
rm(list=ls())

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
## Load HPI data (@ ZIP_3 level)
##-----------------------------------------------------------------------
load("/Users/alexstephens/Development/fnma/data/hpi/HPI_AT_3zip.Rda") ## hpi

##-----------------------------------------------------------------------
## Load UNEMP data (@ STATE level)
##-----------------------------------------------------------------------
load("/Users/alexstephens/Development/fnma/data/unemp/UNEMP_AT_STATE.Rda") ## unemp

##-----------------------------------------------------------------------
## <function> lomf
##-----------------------------------------------------------------------
## Fill missing Current UPBs by setting them to the record prior
##-----------------------------------------------------------------------
na.lomf <- function(x) {
    
    na.lomf.0 <- function(x) {
        non.na.idx <- which(!is.na(x))
        if (is.na(x[1L])) {
            non.na.idx <- c(1L, non.na.idx)
        }
        rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
    }
    dim.len <- length(dim(x))
    
    if (dim.len == 0L) {
        na.lomf.0(x)
    } else {
        apply(x, dim.len, na.lomf.0)
    }
}

##-----------------------------------------------------------------------
## <function> panel_lag
##-----------------------------------------------------------------------
## permit lagging/leading of column variables in data.table
##-----------------------------------------------------------------------
panel_lag <- function(var, k) {
    if (k > 0) {
        # Bring past values forward k times
        return(c(rep(NA, k), head(var, -k)))
    } else {
        # Bring future values backward
        return(c(tail(var, k), rep(NA, -k)))
    }
}

##-----------------------------------------------------------------------
## <function> backfill_UPB
##-----------------------------------------------------------------------
## simple function to replace the missing values in a vector with
## the first observed value (assumes missing values only appear at
## beginning of the vector
##-----------------------------------------------------------------------
backfill_UPB <- function(x) {
    
    na.idx  <- which(is.na(x))
    ok.idx  <- which(!is.na(x))
    
    if (length(x) <= 1) {
        return(x)
    } else {
        y <- x
        y[na.idx] <- x[ok.idx][1]
        return(y)
    }
}


##-----------------------------------------------------------------------
## set the working directory
##-----------------------------------------------------------------------
setwd("/Users/alexstephens/Development/fnma/data/proc")
bacDirectory <- "/Users/alexstephens/Development/fnma/data/bac"

##-----------------------------------------------------------------------
## get the performance file list
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Performance_Data.Rda$")[1:48]     ## limit to [1:48] b/c no BAC sales to FNMA after 2011Q4
file.num    <- length(file.list)

##-----------------------------------------------------------------------
## get the combined acquisitions file; isolate the loan ids
##-----------------------------------------------------------------------
load(paste0(bacDirectory,"/","Acquisitions_Data_BAC_All.Rda"))

    ##-----------------------------------------------------------------------
    ## apply filters to Acquisitions data
    ##-----------------------------------------------------------------------

    ## [Filter 1] remove ZIP_3 with no corresponding HPI match
    ## [Filter 2] remove non-states ("GU", "VI")
    Tmp_A   = Acquisitions_Data.bac.all[ !((ZIP_3 %in% bac.zip3.mismatch.names) | (STATE %in% c("GU","VI"))), ]
    Filt_A  = Acquisitions_Data.bac.all[  ((ZIP_3 %in% bac.zip3.mismatch.names) | (STATE %in% c("GU","VI"))), ] ## complement

    ## isolate the remaining set of viable loans
    loan.id <- Tmp_A[,LOAN_ID]

    ## remove the original acquisitions file
    rm(Acquisitions_Data.bac.all)

##-----------------------------------------------------------------------
## loop over all performance files and produce different subsets
##-----------------------------------------------------------------------
for (i in 1:1) {
#for (i in 1:file.num) {
#foreach (i=1:file.num) %dopar% {
    
    tmp.file    <- file.list[i]                 ## filename
    tmp.hdr     <- substr(tmp.file, 1, 6)       ## origination header
    load(tmp.file)
    
    ## output filenames
    tmp.perf     <- paste0(tmp.hdr, "_Performance_Data_BAC.Rda")
    tmp.loss     <- paste0(tmp.hdr, "_Loss_Data_BAC.Rda")
    tmp.default  <- paste0(tmp.hdr, "_Default_Data_BAC.Rda")
    tmp.comb     <- paste0(tmp.hdr, "_Combined_Data_BAC.Rda")
   
    ## echo progress
    message("processing file :: ", tmp.file)
    
    ##-----------------------------------------------------------------------
    ## extract the subsetted performance & acquisitions data
    ##-----------------------------------------------------------------------
    Data_P                  <- Performance_Data[LOAN_ID %in% loan.id,]
    Data_A                  <- Tmp_A[LOAN_ID %in% Data_P[,unique(LOAN_ID),]]
    
    ## Convert character variables to Date type
    Data_A$ORIG_DTE         <- as.Date(ifelse(Data_A$ORIG_DTE == "", "", paste0(substr(Data_A$ORIG_DTE,1,2),"/01/",substr(Data_A$ORIG_DTE,4,8))), "%m/%d/%Y")   ## origination date
    Data_A$FRST_DTE         <- as.Date(ifelse(Data_A$FRST_DTE == "", "", paste0(substr(Data_A$FRST_DTE,1,2),"/01/",substr(Data_A$FRST_DTE,4,8))), "%m/%d/%Y")   ## origination date
    
    ## Convert character variables to Date type
    Data_P$Monthly.Rpt.Prd  <- as.Date(Data_P$Monthly.Rpt.Prd, "%m/%d/%Y")
    Data_P$DISP_DT          <- as.Date(Data_P$DISP_DT, "%m/%d/%Y")          ## date at which FNMA's interest in property ends
    Data_P$FCC_DTE          <- as.Date(Data_P$FCC_DTE, "%m/%d/%Y")          ## date of first legal action of foreclosure
    Data_P$ZB_DTE           <- as.Date(ifelse(Data_P$ZB_DTE == "", "", paste0(substr(Data_P$ZB_DTE,1,2),"/01/",substr(Data_P$ZB_DTE,4,8))), "%m/%d/%Y")           ## date of zero balance
    Data_P$LPI_DTE          <- as.Date(Data_P$LPI_DTE, "%m/%d/%Y")          ## date of last paid installment

    ## Sort data by Loan ID and Monthly Reporting Period
    setorderv(Data_P, c("LOAN_ID", "Monthly.Rpt.Prd"))
    setkey(Data_P, "LOAN_ID")

    ## Standardize Delinquency Status Codes
    Data_P$Delq.Status<-as.numeric(ifelse(Data_P$Delq.Status=="X", "999", Data_P$Delq.Status))

    ## Count the number of months a loan is active
    Data_P[,Count:=1:.N, by="LOAN_ID"]

    ## Create a second loan identifier by splitting pre-/post-modification
    Data_P[, MOD_ID := paste0(LOAN_ID,MOD_FLAG)]

    ## Count the number of months a loan is active
    Data_P[, MOD_CNT := 1:.N, by="MOD_ID"]

    ##-----------------------------------------------------------------------
    ## save the raw subsetted data
    ##-----------------------------------------------------------------------
    save(Data_P, file=paste0(bacDirectory,"/",tmp.perf))

    ## remove the main file from memory
    rm("Performance_Data")

    ##-----------------------------------------------------------------------
    ## Create standalone tables of Mod, Credit, 180DPD and Repurchase events
    ##-----------------------------------------------------------------------
    
        ## Note: A "NA" Delq.Status only occurs for Zero.Bal.Code in c("03","09")

        ##-----------------------------------------------------------------------
        ## Obtain the date of the first time each loan was modified
        ##-----------------------------------------------------------------------
        ## Example
        ##      Data_P[which(LOAN_ID == "107506622473"),]
        ##      Data_P[which(LOAN_ID == "980634057564"),]
        FMOD_DTE = Data_P[, .SD[MOD_FLAG =="Y"][,c("FMOD_DTE", "CountM1"):=list(Monthly.Rpt.Prd, Count)]][, .SD[1], by = "LOAN_ID"][,c("LOAN_ID", "FMOD_DTE", "CountM1"), with = FALSE, drop = FALSE]

        ##-----------------------------------------------------------------------
        ## Obtain the date and UPB of each loan's first credit event (i.e. 180 DPD SDQ, or Foreclosure or Default)
        ##-----------------------------------------------------------------------
        ## ZB = 03 --> Short Sale | Third Party Sale | Note Sale
        ## ZB = 09 --> Deed in Lieu | REO
        ## Delq.Status <999 & >=6 = 180DPD
        First_CE = Data_P[, .SD[Zero.Bal.Code =="03" | Zero.Bal.Code =="09" | (Delq.Status<999 & Delq.Status>= 6)][, c("FCE_DTE", "FCE_UPB", "SPDelq1", "SPZero1", "CountFC") := list(Monthly.Rpt.Prd, LAST_UPB, Delq.Status, as.integer(Zero.Bal.Code), Count)]][, .SD[1], by = "LOAN_ID"][,c("LOAN_ID", "SPDelq1", "SPZero1", "FCE_DTE", "FCE_UPB", "CountFC"), with = FALSE, drop = FALSE]

        ##-----------------------------------------------------------------------
        ## Obtain the date and UPB of each loan becoming 180 days delinquent
        ##-----------------------------------------------------------------------
        ## Delq.Status <999 & >=6 = 180DPD
        First_D180 = Data_P[, .SD[Delq.Status<999 & Delq.Status >=6][,c("F180_DTE", "F180_UPB", "SPDelq2", "CountF1") := list(Monthly.Rpt.Prd, LAST_UPB, Delq.Status, Count)]][, .SD[1], by = "LOAN_ID"][,c("LOAN_ID", "F180_DTE", "F180_UPB", "SPDelq2", "CountF1"), with = FALSE, drop = FALSE]

        ##-----------------------------------------------------------------------
        ## Obtain the first prepay date
        ##-----------------------------------------------------------------------
        First_PP = Data_P[, .SD[Delq.Status == 999 & Zero.Bal.Code =="01"][,c("PP_DTE", "CountP1") := list(Monthly.Rpt.Prd, Count)]][, .SD[1], by = "LOAN_ID"][,c("LOAN_ID", "PP_DTE", "CountP1"), with = FALSE, drop = FALSE]

        ##-----------------------------------------------------------------------
        ## Obtain the first repurchase date
        ##-----------------------------------------------------------------------
        ## ZB = 06 --> Repurchased
        First_REPO = Data_P[, .SD[Delq.Status == 999 & Zero.Bal.Code =="06"][,c("REPO_DTE", "CountR1") := list(Monthly.Rpt.Prd, Count)]][, .SD[1], by = "LOAN_ID"][,c("LOAN_ID", "REPO_DTE", "CountR1"), with = FALSE, drop = FALSE]

        ##-----------------------------------------------------------------------
        ## Obtain the date and UPB of each loan's first credit event (by MOD_ID)
        ##-----------------------------------------------------------------------
        ## ZB = 03 --> Short Sale | Third Party Sale | Note Sale
        ## ZB = 09 --> Deed in Lieu | REO
        ## Delq.Status <999 & >=6 = 180DPD
        MOD_CE = Data_P[, .SD[Zero.Bal.Code =="03" | Zero.Bal.Code =="09" | (Delq.Status<999 & Delq.Status>= 6)][, c("FCE_DTE.M", "FCE_UPB.M", "SPDelq1.M", "SPZero1.M", "CountFC.M") := list(Monthly.Rpt.Prd, LAST_UPB, Delq.Status, as.integer(Zero.Bal.Code), MOD_CNT)]][, .SD[1], by = "MOD_ID"][,c("MOD_ID", "SPDelq1.M", "SPZero1.M", "FCE_DTE.M", "FCE_UPB.M", "CountFC.M"), with = FALSE, drop = FALSE]

        ##-----------------------------------------------------------------------
        ## Obtain the date and UPB of each loan becoming 180DPD (by MOD_ID)
        ##-----------------------------------------------------------------------
        ## Delq.Status <999 & >=6 = 180DPD
        MOD_D180 = Data_P[, .SD[Delq.Status<999 & Delq.Status >=6][,c("F180_DTE.M", "F180_UPB.M", "SPDelq2.M", "CountF1.M") := list(Monthly.Rpt.Prd, LAST_UPB, Delq.Status, MOD_CNT)]][, .SD[1], by = "MOD_ID"][,c("MOD_ID", "F180_DTE.M", "F180_UPB.M", "SPDelq2.M", "CountF1.M"), with = FALSE, drop = FALSE]

    ## Backfill missing current UPBs
    #Data_P$LAST_UPB <- na.lomf(Data_P$LAST_UPB)

    ##-----------------------------------------------------------------------
    ## Create the loss data
    ##-----------------------------------------------------------------------
    ## Step 1:  Collapse each loan into 1 row per LOAN_ID
    ## Step 2:  Define the last status for each loan
    ## Step 3:  Report expenses/process for forecloses/liquidated loans
    ## Step 4:  Append credit/180DPD event data onto the loss data
    ## Step 5:  Clean-up
    ##-----------------------------------------------------------------------
    Loss_P <- Data_P[, .SD[.N], by = "LOAN_ID"]

    ## Define Last Date variable as Disposition Date *or* Foreclosure Date *or* Last Observed Date
    Loss_P[,c("LAST_DTE") := list(as.Date(ifelse(!(is.na(Loss_P$DISP_DT)), Loss_P$DISP_DT, ifelse(!(is.na(Loss_P$FCC_DTE)), Loss_P$FCC_DTE, Loss_P$Monthly.Rpt.Prd))))]

    ## Define the last status of a loan and calculate the months between Last Paid Installment and Disposition date (for Lost Interest calculation)
    Loss_P[, c("LAST_STAT", "lpi2disp", "lpi2last"):=list(
            ## LAST_STAT
            ifelse(Zero.Bal.Code=='01', 'P',    ## prepaid | matured
            ifelse(Zero.Bal.Code=='03', 'S',    ## short sale | third party sale | note sale
            ifelse(Zero.Bal.Code=='06', 'R',    ## repurchased
            ifelse(Zero.Bal.Code=='09', 'F',    ## deed in lieu | REO
            ifelse(Delq.Status=='999',  'X',    ## unknown DLQ
            ifelse(Delq.Status >9,      '9',    ## more than 9 months past due
            ifelse(Delq.Status==0,      'C',    ## current
            as.character(Delq.Status)))))))),
            ## Last paid date to (DISP, LAST) date
            ifelse( !(is.na(Loss_P$LPI_DTE)) & !(is.na(Loss_P$DISP_DT)), 12*(year(DISP_DT)-year(LPI_DTE))+(month(DISP_DT)-month(LPI_DTE)), 0),
            ifelse( !(is.na(Loss_P$LPI_DTE)) & !(is.na(Loss_P$LAST_DTE)), 12*(year(LAST_DTE)-year(LPI_DTE))+(month(LAST_DTE)-month(LPI_DTE)), 0)
            )]

    ## Calculate Interest Cost, total expenses and total proceeds (for short-sales and foreclosures)
    ## NOTE: Could modify the interest calc to include the censor date
    Loss_P[, c("INT_COST", "INT_LAST", "total_expense", "total_proceeds") := list(
        ifelse( ((LAST_STAT =="F" | LAST_STAT =="S")& !is.na(DISP_DT)), LAST_UPB*(((LAST_RT/100) - .0035)/12)*lpi2disp,0),
        ifelse(  (LAST_STAT =="F" | LAST_STAT =="S"), LAST_UPB*(((LAST_RT/100) - .0035)/12)*lpi2last,0),
        ifelse(  (LAST_STAT =="F" | LAST_STAT =="S"), rowSums(Loss_P[, list(FCC_COST, PP_COST, AR_COST, TAX_COST, IE_COST)], na.rm = TRUE),0),
        ifelse(  (LAST_STAT =="F" | LAST_STAT =="S"), (-1*rowSums(Loss_P[, list(NS_PROCS, CE_PROCS, RMW_PROCS, O_PROCS)], na.rm = TRUE)),0)
        )]

    ## Calculate Net Loss, Net Severity, Total Costs, Total Proceeds, and Total Liquidation Expenses.
    ## Define Last Date variable as Disposition Date *or* Foreclosure Date *or* Last Observed Date
    Loss_P[,c("NET_LOSS", "NET_SEV", "Total_Cost", "Tot_Procs", "Tot_Liq_Ex", "LAST_DTE2") := list(
        ifelse(((LAST_STAT =="F" | LAST_STAT =="S")& !is.na(DISP_DT)),  rowSums(Loss_P[, list(LAST_UPB, INT_COST, total_expense, total_proceeds)], na.rm=TRUE), 0),
        ifelse(((LAST_STAT =="F" | LAST_STAT =="S")& !is.na(DISP_DT)), (rowSums(Loss_P[, list(LAST_UPB, INT_COST, total_expense, total_proceeds)], na.rm=TRUE)/LAST_UPB), 0),
        ifelse( (LAST_STAT =="F" | LAST_STAT =="S"),  rowSums(Loss_P[, list(LAST_UPB, INT_COST, FCC_COST, PP_COST, AR_COST, IE_COST, TAX_COST)], na.rm = TRUE), 0),
        ifelse( (LAST_STAT =="F" | LAST_STAT =="S"),  rowSums(Loss_P[, list(NS_PROCS, CE_PROCS, RMW_PROCS, O_PROCS)], na.rm = TRUE), 0),
        ifelse( (LAST_STAT =="F" | LAST_STAT =="S"),  rowSums(Loss_P[, list(FCC_COST, PP_COST, AR_COST, IE_COST, TAX_COST)], na.rm = TRUE), 0),
        as.Date(ifelse(!(is.na(Loss_P$DISP_DT)), Loss_P$DISP_DT, ifelse(!(is.na(Loss_P$FCC_DTE)), Loss_P$FCC_DTE, Loss_P$Monthly.Rpt.Prd)))
        )]

    ## Merge mod, credit event and 180DPD data with full performance dataset to capture information on First Modification, First Credit Event, and First Default.
    Loss_P[FMOD_DTE, FMOD_DTE := i.FMOD_DTE]
    Loss_P[First_CE, c("FCE_DTE", "FCE_UPB", "SPDelq1", "CountFC") := list(i.FCE_DTE, i.FCE_UPB, i.SPDelq1, i.CountFC)]
    Loss_P[First_D180, c("F180_DTE", "F180_UPB", "SPDelq2", "CountF1") :=list (i.F180_DTE, i.F180_UPB, i.SPDelq2, i.CountF1)]

    ## Delete Performance variables that are not needed.
    #Loss_P[, c("Count", "Monthly.Rpt.Prd", "ZB_DTE", "Servicer.Name", "Loan.Age", "Months.To.Legal.Mat", "Adj.Month.To.Mat", "Maturity.Date", "Delq.Status","total_expense", "total_proceeds", "lpi2disp"):=NULL]
    Loss_P[, c("Count", "Servicer.Name", "Loan.Age", "Months.To.Legal.Mat", "Adj.Month.To.Mat", "Maturity.Date") := NULL]

    ##-----------------------------------------------------------------------
    ## save the loss data
    ##-----------------------------------------------------------------------
    save(Loss_P, file=paste0(bacDirectory,"/",tmp.loss))


    ##-----------------------------------------------------------------------
    ## Create a base performance data file
    ##-----------------------------------------------------------------------
    ## Step 1:  Define a clean last status
    ## Step 2:  Merge mod, delq, repo data onto the performance data
    ## Step 3:  Create mod, delq, repo counters
    ##-----------------------------------------------------------------------
    
    ## clean-up the lat status code
    Data_P[, c("LAST_STAT"):=list(
        ## LAST_STAT
        ifelse(Zero.Bal.Code=='01', 'P',    ## prepaid | matured
        ifelse(Zero.Bal.Code=='03', 'S',    ## short sale | third party sale | note sale
        ifelse(Zero.Bal.Code=='06', 'R',    ## repurchased
        ifelse(Zero.Bal.Code=='09', 'F',    ## deed in lieu | REO
        ifelse(Delq.Status=='999',  'X',    ## unknown DLQ
        ifelse(Delq.Status >9,      '9',    ## more than 9 months past due
        ifelse(Delq.Status==0,      'C',    ## current
        as.character(Delq.Status))))))))
        )]
        
    setkey(Data_P,LOAN_ID,Monthly.Rpt.Prd)
    Data_P[, LAG_STAT:=panel_lag(LAST_STAT,1), by="LOAN_ID"]  ## "ZZ" is an absorbing state
    
    ## clean UPBs
    setkey(Data_P, LOAN_ID, Monthly.Rpt.Prd)
    Data_P[,BACK_UPB := backfill_UPB(LAST_UPB), by = "LOAN_ID"]
    Data_P$FRWD_UPB <- na.lomf(Data_P$LAST_UPB)
    
    Data_P$FIN_UPB <- ifelse(Data_P$Count <= 6, Data_P$BACK_UPB, Data_P$FRWD_UPB)
    Data_P[,c("BACK_UPB","FRWD_UPB") := NULL]

    ## merge mod/delq/repo data (by LOAN_ID)
    Data_P[First_PP, c("PP_DTE", "CountP1") := list(i.PP_DTE, i.CountP1)]
    Data_P[FMOD_DTE, c("FMOD_DTE", "CountM1") := list(i.FMOD_DTE, i.CountM1)]
    Data_P[First_CE, c("FCE_DTE", "SPDelq1", "SPZero1", "CountFC") := list(i.FCE_DTE, i.SPDelq1, i.SPZero1, i.CountFC)]
    Data_P[First_D180, c("F180_DTE", "CountF1") :=list (i.F180_DTE, i.CountF1)]
    Data_P[First_REPO, c("REPO_DTE", "CountR1") :=list (i.REPO_DTE, i.CountR1)]

    ## merge mod/delq/repo data (by MOD_ID)
    setkey(Data_P, MOD_ID)
    Data_P[MOD_CE,   c("FCE_DTE.M", "SPDelq1.M", "SPZero1.M", "CountFC.M") := list(i.FCE_DTE.M, i.SPDelq1.M, i.SPZero1.M, i.CountFC.M)]
    Data_P[MOD_D180, c("F180_DTE.M", "CountF1.M") :=list (i.F180_DTE.M, i.CountF1.M)]

    ## define counter variables
    Data_P[,c("PP_CTR", "MOD_CTR", "FCE_CTR", "F180_CTR", "REPO_CTR") := list( (CountP1-Count), (CountM1-Count), (CountFC-Count), (CountF1-Count), (CountR1-Count) )]
    Data_P[,c("FCE_CTR.M", "F180_CTR.M") := list((CountFC.M-MOD_CNT), (CountF1.M-MOD_CNT))]

    ## Mark the source file
    #Data_P$src <- tmp.hdr

    ## Create Default & Prepay Indices / Flags
    Data_P[, DEF1Y   := 1*(!is.na(FCE_CTR) & (FCE_CTR <= 11) & (FCE_CTR >= 0))]         ## 1Y Default Flag
    Data_P[, DEF1Y.M := 1*(!is.na(FCE_CTR.M) & (FCE_CTR.M <= 11) & (FCE_CTR.M >= 0))]   ## 1Y Default Flag
    Data_P[, PP1Y    := 1*(!is.na(PP_CTR) & (PP_CTR <= 11) & (PP_CTR >= 0))]            ## 1Y Prepay Flag
    Data_P[, DEF     := 1*(!is.na(FCE_CTR) & FCE_CTR == 0)]                             ## Instant Default Flag
    Data_P[, DEF.M   := 1*(!is.na(FCE_CTR.M) & FCE_CTR.M == 0)]                         ## Instant Default Flag
    
    ##-----------------------------------------------------------------------
    ## save the default data
    ##-----------------------------------------------------------------------
    save(Data_P, file=paste0(bacDirectory,"/",tmp.default))

    ##-----------------------------------------------------------------------
    ## Create a combined acquisitions/performance data file
    ##-----------------------------------------------------------------------
    ## Step 1:
    ##-----------------------------------------------------------------------

    ## drop superfluous columns
    Data_P[, c( "Servicer.Name", "MSA",
                "ZB_DTE", "LPI_DTE", "FCC_DTE", "DISP_DT",
                "NON_INT_UPB", "PRIN_FORG_UPB",
                "FCC_COST", "PP_COST", "AR_COST", "IE_COST", "TAX_COST",
                "NS_PROCS", "CE_PROCS", "RMW_PROCS", "O_PROCS",
                "Delq.Status", "Zero.Bal.Code", "MOD_FLAG",
                "PP_DTE", "CountP1", "CountM1", "FCE_DTE",  #"FMOD_DTE",
                "CountFC", "F180_DTE", "CountF1", "REPO_DTE", "CountR1",
                "FCE_DTE.M", "CountFC.M", "F180_DTE.M", "CountF1.M",
                ## additional removals
                "SPDelq1", "SPZero1", "SPDelq1.M", "SPZero1.M", "Adj.Month.To.Mat", "Months.To.Legal.Mat"
                ) := NULL]

    setkey(Data_P, LOAN_ID)
    Data_C = as.data.table(merge(Data_A, Data_P, by.x = "LOAN_ID", by.y = "LOAN_ID", all = TRUE))
 
    ## Append Unemployment
    Data_C[ , UNEMP_KEY := paste0(year(Monthly.Rpt.Prd), month(Monthly.Rpt.Prd), STATE)]
    Data_C$UNEMP     <- unemp[Data_C[,UNEMP_KEY], value]
    Data_C$UNEMP_1Q  <- unemp[Data_C[,UNEMP_KEY], LAG_1Q]
    Data_C$UNEMP_2Q  <- unemp[Data_C[,UNEMP_KEY], LAG_2Q]
    Data_C$UNEMP_3Q  <- unemp[Data_C[,UNEMP_KEY], LAG_3Q]
    Data_C$UNEMP_4Q  <- unemp[Data_C[,UNEMP_KEY], LAG_4Q]
    Data_C$UNEMP_8Q  <- unemp[Data_C[,UNEMP_KEY], LAG_8Q]
    Data_C$UNEMP_12Q <- unemp[Data_C[,UNEMP_KEY], LAG_12Q]

    ## merge HPI data onto the combined data
    Data_C[, c("HPI_KEY_RPT"):= paste0(ZIP_3,year(Monthly.Rpt.Prd),quarter(Monthly.Rpt.Prd)) ]
    Data_C$RPT_IDX    <- hpi[Data_C[,HPI_KEY_RPT], Index]
    Data_C$RPT_IDX_1Q <- hpi[Data_C[,HPI_KEY_RPT], LAG_1Q]
    Data_C$RPT_IDX_2Q <- hpi[Data_C[,HPI_KEY_RPT], LAG_2Q]
    Data_C$RPT_IDX_3Q <- hpi[Data_C[,HPI_KEY_RPT], LAG_3Q]
    Data_C$RPT_IDX_4Q <- hpi[Data_C[,HPI_KEY_RPT], LAG_4Q]
    Data_C$RPT_IDX_8Q <- hpi[Data_C[,HPI_KEY_RPT], LAG_8Q]
    Data_C$RPT_IDX_12Q <- hpi[Data_C[,HPI_KEY_RPT], LAG_12Q]


    Data_C[, c("HPI_KEY_ORIG"):= paste0(ZIP_3,year(ORIG_DTE),quarter(ORIG_DTE)) ]
    Data_C$ORIG_IDX <- hpi[Data_C[,HPI_KEY_ORIG], Index]

    Data_C[, MOD_DTE := as.Date(ifelse( is.na(FMOD_DTE), ORIG_DTE, FMOD_DTE))]
    Data_C[, c("HPI_KEY_FMOD"):= paste0(ZIP_3,year(MOD_DTE),quarter(MOD_DTE)) ]
    Data_C$MOD_IDX <- hpi[Data_C[,HPI_KEY_FMOD], Index]

    Data_C[, c("CLTV") := 100*LAST_UPB/((RPT_IDX/ORIG_IDX)*ORIG_VAL)]
    Data_C[, c("CURR_VAL") := ORIG_VAL*(RPT_IDX/ORIG_IDX)]
 
    Data_C[, c("HPI_KEY_RPT","HPI_KEY_ORIG", "HPI_KEY_FMOD", "UNEMP_KEY") := NULL]
 
    ##-----------------------------------------------------------------------
    ## save the default data
    ##-----------------------------------------------------------------------
    save(Data_C, file=paste0(bacDirectory,"/",tmp.comb))

    ## clean-up for the next loop
    #rm("Loss_P","Data_P","Data_A","Data_C","First_CE","First_D180","First_PP","First_REPO","FMOD_DTE","MOD_CE","MOD_D180")
}

