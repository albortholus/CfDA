best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    what <- what[[outcome]]
    subdata <- subset(data, data$State == state)
    subdata[order(as.numeric(subdata[, what]), subdata[, "Hospital.Name"], na.last=TRUE)[1], "Hospital.Name"]
}