
hist_single <- function (values, main) {
    med <- median(values[!is.na(values)])
    hist(values, main=substitute(main * " (" * bar(X) * "= " * MX * ")", list(MX = med)), xlab="30-day Death Rate", xlim=c(6, 21), prob=TRUE)    
    abline(v=med)
    lines(density(values[!is.na(values)]))
}


hists_2 <- function () {
    par(mfcol=c(3,1))
    hist_single(outcome[,11], main="Heart Attack")
    hist_single(outcome[,17], main="Heart Failure")
    hist_single(outcome[,23], main="Pneumonia")
}

box_3 <- function() {
    par(mfcol=c(1,1))
    boxplot(
        death ~ state,
        ylab="30-day Death Rate",
        main="Heart Attack 30-day Death Rate by State",
        las=2
    )
}

xy_4 <- function() {
    library(lattice)
    xyplot(
        death ~ npatient | owner,
        ylab="30-day Death Rate",
        xlab="Heart Attack 30-day Death Rate by Ownership",
        panel = function(x, y, ...) {
            panel.xyplot(x, y, ...)
            fit <- lm(y ~ x)
            panel.abline(fit)
        }
    )
}
