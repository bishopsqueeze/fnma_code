##-----------------------------------------------------------------------
## Load libraries
##-----------------------------------------------------------------------
if (!(require(foreach))) install.packages ("foreach")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(reshape2))) install.packages ("reshape2")
#if (!(require(XLConnect))) install.packages ("XLConnect")
if (!(require(xtable))) install.packages ("xtable")
if (!(require(zoo))) install.packages ("zoo")
if (!(require(ggplot2))) install.packages ("ggplot2")
if (!(require(splines))) install.packages ("splines")
if (!(require(caret))) install.packages ("caret")
if (!(require(logistf))) install.packages ("logistf")
if (!(require(ROCR))) install.packages ("ROCR")
if (!(require(MASS))) install.packages ("MASS")

##-----------------------------------------------------------------------
## Multiple plot function
##-----------------------------------------------------------------------
##
## ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
## - cols:   Number of columns in layout
## - layout: A matrix specifying the layout. If present, 'cols' is ignored.
##
## If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
## then plot 1 will go in the upper left, 2 will go in the upper right, and
## 3 will go all the way across the bottom.
##
##-----------------------------------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
        ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
            layout.pos.col = matchidx$col))
        }
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
