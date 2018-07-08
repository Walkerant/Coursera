#read all data in data set
#the training data
train <- read.table("./train/X_train.txt")
subject_train <- read.table("./train/subject_train.txt")
y_train <- read.table("./train/y_train.txt")
#the test data
test <- read.table("./test/X_test.txt")
subject_test <- read.table("./test/subject_test.txt")
y_test <- read.table("./test/y_test.txt")
#read activity_labels data
activities <- read.table("activity_labels.txt")
#read features as variables names
features <- read.table("features.txt")
#merge the training data and test data as a new one named m_data
m_data <- rbind(train,test)

#Extracts only the measurements on the mean and std for 
#each measurement from m_data. 
#name m_data with variable in features file
features <- make.names(features[,2])
features <- gsub("(\\.+)",".",features)
colnames(m_data) <- features
position <- grep("(mean|std)[.+]",features)
m_data <- m_data[,position]

#merge subject and activities in trainning data and test data
subject <- rbind(subject_train,subject_test)
subject[,1] <- as.factor(subject[,1])
activity <- rbind(y_train,y_test)
activity[,1] <- as.factor(activity[,1])
colnames(subject) <- "subject"
colnames(activity) <- "activity"

#combine subjectand activity with m_data in a new dataframe names data
data <- cbind(subject,activity,m_data)

#group and summarise data with subject and activity columns
g_data <- group_by(data,subject,activity)
s_data <- summarise_each(g_data,funs(mean))
write.table(s_data,"analysis_data.txt",row.names = FALSE)
s_data




