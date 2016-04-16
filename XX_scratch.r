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
## get the Performance file list
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Performance_Data_BAC.Rda$")
file.num    <- length(file.list)





## Load the file

## create a default indicator
Tmp_C[,def := ifelse(is.na(FCE_CTR),0,ifelse(FCE_CTR<=0, 1, 0))]

Tmp_C2 = Tmp_C[which(is.na(FCE_CTR) | FCE_CTR >=0),]


a <- Tmp_C2[,table(Loan.Age,def)]
colnames(a) <- c("ND","D")
b <- data.frame(
AGE = as.numeric(rownames(a)),
ND  = as.vector(a[,"ND"]),
D   = as.vector(a[,"D"])
)

b$TOT <- b$ND + b$D
b$defrate <- b$D/b$TOT
b$cumdef <- cumsum(b$D)/b$TOT

q <- Tmp_C2[,table(Loan.Age,LAST_STAT)]

