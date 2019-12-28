loadDataset <- function() {
    dataset <- read.csv("activity.csv")
    dataset
}

plotHistogramTotalSteps <- function(dateSummary) {
    hist(dateSummary$totalSteps, col = "green", xlab = "TotalStepsPerDay")
    rug(dateSummary$totalSteps)
    abline(v = median(dateSummary$totalSteps), col = "magenta", lwd = 4)
    abline(v = mean(dateSummary$totalSteps), col = "blue", lwd = 4)
}

barPlotTotalSteps <- function(dateSummary) {
    par(mfrow = c(3,4), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
    i <- 1
    while(i < nrow(dateSummary)) {
        j <- i + 4
        if(j == 60) j <- 61
        with(dateSummary[i:j,],
             barplot(totalSteps, names.arg = date, xlab = "Date", ylab = "TotalSteps", 
                     col = date, border = "red"))
        i <- i + 5
    }
    mtext("Date wise breakup of TotalSteps", side = 3, outer = TRUE, cex = 0.8, font = 2)
}

totalStepsTakenEachDay <- function(dataset) {
    library(dplyr)
    groupByDataset <- group_by(dataset, date)
    dateSummary <- summarise(groupByDataset, totalSteps = sum(steps, na.rm = TRUE))
    #View(dateSummary)
    
    #Plot  histogram on totalSteps
    plotHistogramTotalSteps(dateSummary)
    
    #Create a Barplot to plot totalSteps per day
    barPlotTotalSteps(dateSummary)
    
    meanStepsTakenPerDay <- mean(dateSummary$totalSteps)
    medianStepsTakenPerDay <- median(dateSummary$totalSteps)
    
    print(paste("Mean Steps Taken Per Day = ", meanStepsTakenPerDay))
    print(paste("Median Steps Taken Per Day = " , medianStepsTakenPerDay))
}

timeSeriesPlot <- function(intervalSummary) {
    par(mfrow=c(1,1))
    with(intervalSummary, plot(x = interval, y = meanSteps, type = "l", xlab = "Interval", ylab = "MeanSteps",
                               col = "blue", main = "Time Series Plot of MeanSteps per Interval"))
}

averageStepsTakenPerInterval <- function(dataset) {
    library(dplyr)
    groupByDataset <- group_by(dataset, interval)
    intervalSummary <- summarise(groupByDataset, meanSteps = mean(steps, na.rm = TRUE))
    #View(intervalSummary)
    
    timeSeriesPlot(intervalSummary)
    
    intervalWithMaxMeanSteps <- with(intervalSummary, filter(intervalSummary, meanSteps == max(meanSteps))$interval)
    
    print(paste("5 Minute interval having max(meanSteps) = ", intervalWithMaxMeanSteps))
    print(paste("max(meanSteps) = ", max(intervalSummary$meanSteps)))
}

inputMissingValues <- function(dataset) {
    library(dplyr)
    
    numberOfNARows <- sum(!complete.cases(dataset))
    print(paste("Number of Rows with NAs = ", numberOfNARows))
    
    #Clone dataset into a new data frame to input NA values
    datasetWithNoNAs <- cbind(dataset)

    #Fill in NAs for column steps with the median values computed in dateSummary
    datasetWithNoNAs <- datasetWithNoNAs %>% group_by(interval) %>% mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
    
    totalStepsTakenEachDay(datasetWithNoNAs)
    
    datasetWithNoNAs
}

weekEndWeekDayPatterns <- function(datasetWithNoNAs) {
    library(timeDate)
    library(ggplot2)
    
    #Create a new factor column to indicate Weekday vs Weekend for a given date
    datasetWithNoNAs <- mutate(datasetWithNoNAs, dayClassification = as.factor(ifelse(isWeekday(date, wday=1:5), "Weekday", "Weekend")))

    print(head(datasetWithNoNAs))
    
    #Group by the dataset with interval and dayClassification
    groupByDataset <- group_by(datasetWithNoNAs, interval, dayClassification)
    intervalSummaryWeekDay <- summarise(groupByDataset, meanSteps = mean(steps, na.rm = TRUE))
    View(intervalSummaryWeekDay)
    
    weekdayTrendPlot <- ggplot(intervalSummaryWeekDay, aes(interval, meanSteps))
    weekdayTrendPlot <- weekdayTrendPlot + geom_line(color = "blue") + labs(x = "Interval", y = "Average Number of Steps") +
                        facet_grid( dayClassification ~ . )
    print(weekdayTrendPlot)
    print(summary(intervalSummaryWeekDay$meanSteps))
}

fitnessDataAnalysis <- function() {
    dataset <- loadDataset()
    
    totalStepsTakenEachDay(dataset)
    
    averageStepsTakenPerInterval(dataset)
    
    datasetWithNoNAs <- inputMissingValues(dataset)
    
    weekEndWeekDayPatterns(datasetWithNoNAs)
}