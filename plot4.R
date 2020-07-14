# INSTRUCTIONS:
# Load the contents of this script (ctrl+a, then ctrl+enter)
# Then type 'plot4()' in the console to generate the png file.

# -------------------------------------------------------------
# FUNCTION: plot4()
# Creates png file containing a plot that matches
# assignment target 'Plot 4'
# -------------------------------------------------------------

plot4 <- function(){
        # Check whether the data is present in the global env.
        # Prepare if it cannot be found.
        if (!exists('pData')) dataPrep()
        
        # Add the plots to a png file
        png(file = "plot4.png")
        
        # Set par
        par(mfrow = c(2, 2))
        
        # Construct plot 1/4
        with(pData,
             plot(DateTime, Global_active_power, 
                  ylab = "Global Active Power", 
                  xlab = "", type = "n"))
        lines(pData$DateTime, pData$Global_active_power)
        
        # Construct plot 2/4
        with(pData, plot(DateTime, Voltage, 
                         ylab = "Voltage", 
                         xlab = "datetime", type = "n"))
        lines(pData$DateTime, pData$Voltage)
        
        # Construct plot 3/4
        with(pData, plot(DateTime, 
                         Global_active_power, type = "n", 
                         xlab = "",
                         ylab = "Energy sub metering",
                         ylim = c(0,40)))
        
        lines(pData$DateTime, pData$Sub_metering_1)
        lines(pData$DateTime, pData$Sub_metering_2, col = "red")
        lines(pData$DateTime, pData$Sub_metering_3, col = "blue")
        axis(2, at = 10)
        legend("topright", lty = 1, col = c("black", "red", "blue"), 
               legend = c("Sub_metering_1", 
                          "Sub_metering_2",
                          "Sub_metering_3"))
        
        # Construct plot 4/4
        with(pData,
             plot(DateTime, Global_reactive_power, 
                  xlab = "datetime", type = "n"))
        lines(pData$DateTime, pData$Global_reactive_power)
        
        dev.off()
}

# -------------------------------------------------------------
# FUNCTION: dataPrep()
# Calls function getFileIfNeeded() to import the raw data
# Then converts Date/Time fields and subsets to cover the 
# required period of time.
# Output = Data frame called 'pData'
# -------------------------------------------------------------

dataPrep <- function(){
        # Load useful packages
        library(lubridate)
        library(dplyr)
        
        # Check for/get the data
        getFileIfNeeded()
        
        # Load the data
        pData <- read.table("household_power_consumption.txt", 
                            header = TRUE, sep = ";", dec = ".",
                            na.strings = "?",
                            colClasses = c("character", "character", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric"))
        # Convert the field: Date
        pData$Date <- dmy(pData$Date)
        
        # Derive a subset of the data covering the required period
        pData <- subset(pData, Date >= as.Date("2007-02-01") & 
                                Date <= as.Date("2007-02-02"))
        
        # Create a field in the data subset which displays Date and Time
        pData$DateTime <- as.POSIXct(paste(pData$Date, pData$Time), 
                                     format = "%Y-%m-%d %H:%M:%S")
        
        # Add a field containing the weekday
        pData <- mutate(pData, Day = wday(Date, label = TRUE))
        
        # Ouput to the global environment
        pData <<- pData
}

# -------------------------------------------------------------
# FUNCTION: getFileIfNeeded()
# Checks if required data is present and downloads if it is not
# -------------------------------------------------------------

getFileIfNeeded <- function() {
        zipfilePath <- paste("exdata_data_household_power_consumption.zip", sep = "")
        url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        if (!file.exists("./exdata_data_household_power_consumption")) {
                if (!file.exists(zipfilePath)) {
                        download.file(url, zipfilePath)
                }
                unzip(zipfilePath, overwrite = TRUE)       
        }
}