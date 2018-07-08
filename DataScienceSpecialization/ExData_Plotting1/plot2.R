#read 10000 rows of the raw data,which contains the data with 1|2/1/2007
data <- read.table("household_power_consumption.txt",sep = ";",
                   header = TRUE,nrows = 100000,stringsAsFactors = FALSE)
#select the wanted data
index <- grep("^(1|2){1}/(2/2007)",data[,1])
data <- data[index,]
#coerced column 3rd to type numeric
data[,3] <- as.numeric(data[,3])
#plot png file
png(filename = "plot2.png",width = 480,height = 480,units = "px")
plot(1:nrow(data),data[,3],type = "l",xlab = "",
     ylab = "Global Active Power(killiwatts)",xaxt="n")
axis(1,at = c(0,nrow(data)/2,nrow(data)),labels = c("Thu","Fri","Sat"))
dev.off()

