rankh <- function(subdata, outcome, num) {    
    #str(subdata)
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    subdata <- subdata[!is.na(subdata[ ,outcome]), ]
    #warning("order")
    if (num == "best") {
        num = 1;
    }
    if (num == "worst") {
        num = dim(subdata)[1]
    }
    if (num > dim(subdata)[1]) {
        return(NA)
    }
    order_num = order(subdata[, outcome], subdata[, "Hospital.Name"], na.last=TRUE)[num]
    if (is.na(subdata[order_num, outcome])) {
        return(NA)
    }
    subdata[order_num, "Hospital.Name"]
    
}

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
    what <- list(
        "heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
        "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
        "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    
    ## Check that state and outcome are valid
    if (!any(data$State == state)) {
        stop("invalid state")
    }
    if (!any(names(what) == outcome)) {
        stop("invalid outcome");
    }
    
    ## For each state, find the hospital of the given rank
    what <- what[[outcome]]
    data[,what] <- as.numeric(data[, what])
    data <- data[!is.na(data[, what]), ]
    values <- sapply(split(data, data$State), function(subdata) { rankh(subdata[, c(what, "Hospital.Name")], what, num) })
    #str(values)
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    result <- data.frame(hospital = values, state = names(values))
    result
}