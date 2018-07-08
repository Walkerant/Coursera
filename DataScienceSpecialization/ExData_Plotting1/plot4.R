#read 10000 rows of the raw data,which contains the data with 1|2/1/2007
data <- read.table("household_power_consumption.txt",sep = ";",
                   header = TRUE,nrows = 100000,stringsAsFactors = FALSE)
#select the wanted data
index <- grep("^(1|2){1}/(2/2007)",data[,1])
data <- data[index,]
#coerced column 3rd to type numeric
data[,3] <- as.numeric(data[,3])
data[,4] <- as.numeric(data[,4])
data[,5] <- as.numeric(data[,5])
data[,7] <- as.numeric(data[,7])
data[,8] <- as.numeric(data[,8])
#plot png file
png(filename = "plot4.png",width = 480,height = 480,units = "px")
par(mfrow = c(2,2))
with(data,{
  plot(1:nrow(data),data[,3],type = "l",xlab = "",
       ylab = "Global Active Power",xaxt="n")
  axis(1,at = c(0,nrow(data)/2,nrow(data)),labels = c("Thu","Fri","Sat"))
  plot(1:nrow(data),data[,5],type = "l",xlab = "datetime",
       ylab = "Voltage",xaxt="n")
  axis(1,at = c(0,nrow(data)/2,nrow(data)),labels = c("Thu","Fri","Sat"))
  plot(1:nrow(data),Sub_metering_1,xlab = "",ylab = "Energy sub metering",
       type = "l",xaxt ="n")
  lines(1:nrow(data),Sub_metering_2,col = "red")
  lines(1:nrow(data),Sub_metering_3,col = "blue")
  axis(1,at = c(0,nrow(data)/2,nrow(data)),labels = c("Thu","Fri","Sat"))
  legend("topright",lty = 1,col = c("black","red","blue"),legend = c("Sub_metering_1",
                                                                     "Sub_metering_2",
                                                                     "Sub_metering_3"),
         bty = "n")
  plot(1:nrow(data),data[,5],type = "l",xlab = "datetime",
       ylab = "Global_reactive_power",xaxt="n")
  axis(1,at = c(0,nrow(data)/2,nrow(data)),labels = c("Thu","Fri","Sat"))
})
dev.off()

