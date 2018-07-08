#read 10000 rows of the raw data,which contains the data with 1|2/1/2007
data <- read.table("household_power_consumption.txt",sep = ";",
                   header = TRUE,nrows = 100000,stringsAsFactors = FALSE)
#select the wanted data
index <- grep("^(1|2){1}/(2/2007)",data[,1])
data <- data[index,]
#coerced column 3rd to type numeric
data[,7] <- as.numeric(data[,7])
data[,8] <- as.numeric(data[,8])
#plot png file
png(filename = "plot3.png",width = 480,height = 480,units = "px")
with(data,plot(1:nrow(data),Sub_metering_1,xlab = "",ylab = "Energy sub metering",
               type = "l",xaxt ="n"))
with(data,lines(1:nrow(data),Sub_metering_2,col = "red"))
with(data,lines(1:nrow(data),Sub_metering_3,col = "blue"))
axis(1,at = c(0,nrow(data)/2,nrow(data)),labels = c("Thu","Fri","Sat"))
legend("topright",lty = 1,col = c("black","red","blue"),legend = c("Sub_metering_1",
                                                                   "Sub_metering_2",
                                                                   "Sub_metering_3"))
dev.off()

