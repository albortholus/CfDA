rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
    what <- list("heart attack"=11, "heart failure" = 17, "pneumonia" = 23)
    
    ## Check that state and outcome are valid
    if (!any(data$State == state)) {
        stop("invalid state")
    }
    if (!any(names(what) == outcome)) {
        stop("invalid outcome");
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    what <- what[[outcome]]
    subdata <- subset(data, data$State == state)
    subdata[,what] <- as.numeric(subdata[,what])
    subdata <- subdata[!is.na(subdata[ ,what]), ]
    if (num == "best") {
        num = 1;
    }
    if (num == "worst") {
        num = dim(subdata)[1]
    }
    if (num > dim(subdata)[1]) {
        return(NA)
    }
    
    order_num = order(subdata[, what], subdata[, "Hospital.Name"], na.last=TRUE)[num]
    if (is.na(subdata[order_num, what])) {
        return(NA)
    }
    subdata[order_num, "Hospital.Name"]
    
}