plot4 <- function()  {
	
	## set path and working directory
	
	path <- "/Users/arsaksen/Documents/Mac Documents September 1 2011/Coursera Data Science Track/Exploratory Data Analysis/Course Project 1"
	setwd(path)

	## Read inputfile on a line by line basis
	inputFile <- "household_power_consumption.txt"

	## Open a connection.  don't forget to close the connection

	con <- file(inputFile, open = "r")

	## Set Date range
	date1 <- as.Date("2007-02-01")
	date2 <- as.Date("2007-02-02")

	##Initialize a new Data Frame which will be your working input table for graphs
	j <- 1
	temp_working_Data <- data.frame()
	working_Data <- data.frame()  ## Final sorted working data frame


	## Now read line-by-line
	header_Line <- readLines(con, n = 1, warn = FALSE)  ## skip Header
	while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
		myLine <- strsplit(oneLine, ";")
		myRecord <- myLine[[1]]
		myDate <- myRecord[1]
		myNewDate <- as.Date(myDate, "%d/%m/%Y")
		myDateTime <- strptime(paste(myDate, myRecord[2]), format="%d/%m/%Y %H:%M:%S")
		if ((myNewDate == date1) | (myNewDate == date2))  {
			temp_working_Data[j,1] <- myNewDate
			temp_working_Data[j,2] <- strptime(myRecord[2], "%H:%M:%S")
			temp_working_Data[j,3] <- as.numeric(myRecord[3])
			temp_working_Data[j,4] <- as.numeric(myRecord[4])
			temp_working_Data[j,5] <- as.numeric(myRecord[5])
			temp_working_Data[j,6] <- as.numeric(myRecord[6])
			temp_working_Data[j,7] <- as.numeric(myRecord[7])
			temp_working_Data[j,8] <- as.numeric(myRecord[8])
			temp_working_Data[j,9] <- as.numeric(myRecord[9])
			temp_working_Data[j,10] <- as.character(myDateTime)
			j <- j + 1
		}
	}

	close(con)  ## close the connection to tidy up

	num_Row <- nrow(temp_working_Data)

	## Use order for multiple sort.  First by Date and then by Time

	val <- order(temp_working_Data[,10])

	num_Row <- nrow(temp_working_Data)

	for (i in 1:num_Row) {
		j <- val[i]
		working_Data[i,1] <- temp_working_Data[j,1]
		working_Data[i,2] <- temp_working_Data[j,2]
		working_Data[i,3] <- temp_working_Data[j,3]
		working_Data[i,4] <- temp_working_Data[j,4]
		working_Data[i,5] <- temp_working_Data[j,5]
		working_Data[i,6] <- temp_working_Data[j,6]
		working_Data[i,7] <- temp_working_Data[j,7]
		working_Data[i,8] <- temp_working_Data[j,8]
		working_Data[i,9] <- temp_working_Data[j,9]
		working_Data[i,10] <- as.character(temp_working_Data[j,10])
	}

	## So, now we have a sorted working table
	## lets give some names

	names(working_Data) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3", "Date_Time")

	x <- as.POSIXct("2006-12-16 17:25:00 PST")  ## Initialize x
	y <- as.numeric()
	z <- as.numeric()
	w <- as.numeric()
	g <- as.numeric()
	v <- as.numeric()
	h <- as.numeric()

	for (n in 1:num_Row) {
		x[n] <- strptime(working_Data[n,10], format="%Y-%m-%d %H:%M:%S")
		y[n] <- working_Data[n, "Sub_metering_1"]
		z[n] <- working_Data[n, "Sub_metering_2"]
		w[n] <- working_Data[n, "Sub_metering_3"]
		g[n] <- working_Data[n, "Global_active_power"]
		v[n] <- working_Data[n, "Voltage"]
		h[n] <- working_Data[n, "Global_reactive_power"]

	}

	## Launch Graphics Device

	png(filename = "plot4.png", width = 480, height = 480)

	par(mfrow = c(2,2))

	plot(x, g, ylab = "Global Active Power", type = "l")

	plot(x, v, ylab = "Voltage", type = "l")


	plot(x, y, ylab = "Energy sub metering", type = "l")
	lines(x, z, type = "l", col = "red")
	lines(x, w, type = "l", col = "blue")

	legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

	plot(x, h, ylab = "Global Reactive Power", type = "l")

	dev.off()  ## close the png file device


}