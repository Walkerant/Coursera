#read 10000 rows of the raw data,which contains the data with 1|2/1/2007
data <- read.table("household_power_consumption.txt",sep = ";",
                   header = TRUE,nrows = 100000,stringsAsFactors = FALSE)
#select the wanted data
index <- grep("^(1|2){1}/(2/2007)",data[,1])
data <- data[index,]
#coerced column 3rd to type numeric
data[,3] <- as.numeric(data[,3])
#plot png file
png(filename = "plot1.png",width = 480,height = 480,units = "px")
hist(data[,3],col = "red",main = "Global Active Power",
     xlab = "Global Active Power(killiwatts)",
     ylim = c(0,1200))
dev.off()

