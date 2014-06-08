plot1 <- function() {
  ##Read data and add names to columns
  data <- data.frame(read.csv("household_power_consumption.txt", sep = ";", colClasses = c("factor", "factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings = c("?")))
  columns <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  names(data) <- columns
  ##Subset data to fit date range 2007-02-01 through 2007-02-02
  data[, 1] <- as.Date(data[, 1], format = "%d/%m/%Y")
  q <- data$Date - as.Date("2007-01-31") > 0
  w <- data$Date - as.Date("2007-02-03") < 0
  isday <- q & w
  days <- data[isday, ]
  ##Render histogram of Global Active Power data as a png file
  png(file = "plot1.png")
  hist(days$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency")
  dev.off()
}