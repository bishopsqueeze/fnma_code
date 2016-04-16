##-----------------------------------------------------------------------
## Extract a complete set of originators from the FNMA acquisition files
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
setwd("/Users/alexstephens/Development/fnma/data/proc")
resDirectory <- "/Users/alexstephens/Development/fnma/res"

##-----------------------------------------------------------------------
## get the acquisitions file list
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Acquisitions_Data.Rda$")
file.num    <- length(file.list)

##-----------------------------------------------------------------------
## loop over all files and collect the universe of sellers
##-----------------------------------------------------------------------
Acquistions.list    <- list()

for (i in 1:file.num)
{
    tmp.file    <- file.list[i]
    tmp.hdr     <- substr(tmp.file, 1, 6)
    load(tmp.file)
    
    tmp.tbl <- data.frame(table(Acquisitions_Data$Seller.Name))
    colnames(tmp.tbl) <- c("Seller", "Count")
    tmp.tbl$File    <- tmp.hdr
    
    Acquistions.list[[tmp.hdr]] <- tmp.tbl
    
    rm("Acquisitions_Data")
    
}


##-----------------------------------------------------------------------
## Seller table
##-----------------------------------------------------------------------
Seller.df   <- do.call(rbind, Acquistions.list)
Seller.tbl  <- xtabs(Count ~ Seller + File, data=Seller.df)
Seller.date <- as.Date(as.yearqtr((paste(substr(colnames(Seller.tbl),1,4), substr(colnames(Seller.tbl),5,6)))))

##-----------------------------------------------------------------------
## Plot BAC vs. WFC by year
##-----------------------------------------------------------------------
plot(Seller.date, Seller.tbl[which(rownames(Seller.tbl) %in% "BANK OF AMERICA, N.A."),], type="s", col="red", ylab="Count")
lines(Seller.date, apply(Seller.tbl[which(rownames(Seller.tbl) %in% c("WELLS FARGO BANK,  NA", "WELLS FARGO BANK, N.A.")),],2,sum), type="s", col="blue", ylab="Count")
legend("topright", c("WFC","BAC"), col=c("blue","red"), inset = .05)

##-----------------------------------------------------------------------
## save
##-----------------------------------------------------------------------
save(Acquistions.list, Seller.df, Seller.tbl, Seller.date, file=paste0(resDirectory,"/","01_Get_Originators.Rda"))