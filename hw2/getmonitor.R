getmonitor <- function(id, directory, summarize = FALSE) {
    ## 'id' is a vector of length 1 indicating the monitor ID
    ## number. The user can specify 'id' as either an integer, a
    ## character, or a numeric.
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'summarize' is a logical indicating whether a summary of
    ## the data should be printed to the console; the default is
    ## FALSE
    
    ## Your code here
    
    if (is.na(directory)) {
        directory <- getwd();
    }
    
    fname <- paste(directory, "/", formatC(as.integer(id), flag="0", width=3, format="d"), ".csv", sep = "")
    data <- read.csv(fname)
    if (summarize) {
        print(summary(data))
    }
    data
}