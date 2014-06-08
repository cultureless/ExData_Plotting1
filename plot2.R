plot2 <- function() {
  ##Read data
  data <- data.frame(read.csv("household_power_consumption.txt", sep = ";", colClasses = c("factor", "factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings = c("?")))
  ##Subset data to fit date range 2007-02-01 through 2007-02-02
  data[, 1] <- as.Date(data[, 1], format = "%d/%m/%Y")
  q <- data[, 1] - as.Date("2007-01-31") > 0
  w <- data[, 1] - as.Date("2007-02-03") < 0
  isday <- q & w
  days <- data[isday, ]
  ##Add column names and convert time and date to new POSIXct vector called "timesct"
  days[, 10] <- 0 ##as.POSIXct("2007-01-01 01:00:00")
  columns <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3", "datetime")
  names(days) <- columns
  days[, 10] <- as.character(paste(days[, 1], days[, 2], sep = " "))
  times <- strptime(days[, 10], "%Y-%m-%d %H:%M:%S")
  timesct <- as.POSIXct(times)
  ##Render line graph of Global Active Power over time as a png file
  png(file = "plot2.png")
  plot(timesct, days$Global_active_power, col = "black", type = "l", main = "", xlab = "", ylab = "Global Active Power (kilowatts)")
  dev.off()
}