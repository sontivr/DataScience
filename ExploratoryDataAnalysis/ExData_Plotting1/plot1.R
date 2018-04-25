library(datasets)

# sources
sourceURL<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
sourceFileCompressed<-"household_power_consumption.zip"
sourceFile<-"household_power_consumption.txt"

# downlod the compressed file if it doesn't exist in the current directory
if (!file.exists(sourceFileCompressed)) {
  download.file(sourceURL, destfile = sourceFileCompressed, mode = "wb")
}

# unzip if it's not already
if (!file.exists(sourceFile)) {
  unzip(sourceFileCompressed, overwrite = T)
}

# read the data file into a data frame
hpower<-read.csv(sourceFile, sep = ";", header = TRUE)

# copy the data for Feb 1st and 2nd of 2007 to hpower0
hpower0<-hpower[hpower$Date=="1/2/2007" | hpower$Date=="2/2/2007",]

# create plot1.png
png("plot1.png", width = 480, height = 480)
hist(as.numeric(as.character(hpower0$Global_active_power)),col="red",main="Global Active Power",xlab="Global Active Power(kilowatts)")

# close the device
dev.off()


