
#read files into R#
setwd("C:/Users/ruru/Dropbox/1 U Florida/1_Classes/Coursera/Clean Data/UCI HAR Dataset/UCI HAR Dataset/train")
list.files()
y_train<-read.table("y_train.txt")
X_train<-read.table("X_train.txt")
subject_train<-read.table("subject_train.txt")

setwd("C:/Users/ruru/Dropbox/1 U Florida/1_Classes/Coursera/Clean Data/UCI HAR Dataset/UCI HAR Dataset/test")
list.files()
y_test<-read.table("y_test.txt")
X_test<-read.table("X_test.txt")
subject_test<-read.table("subject_test.txt")

setwd("C:/Users/ruru/Dropbox/1 U Florida/1_Classes/Coursera/Clean Data/UCI HAR Dataset/UCI HAR Dataset")
list.files()

features<-read.table("features.txt")


#Merges the training and the test sets to create one data set#
train_bind<-cbind(subject_train, y_train,X_train)
test_bind<-cbind(subject_test, y_test,X_test)
data_combined<-rbind(train_bind,test_bind)

#Uses descriptive activity names to name the activities in the data set#

activity_lables<-read.table("activity_labels.txt")
y_bind<-rbind(y_train,y_test)

activity<-NULL
for (i in 1:10299){ 
	if (y_bind[i,]==1){
		activity<-c(activity,"WALKING")
		}
	else if (y_bind[i,]==2){
		activity<-c(activity,"WALKING_UPSTAIRS")
		}
	else if (y_bind[i,]==3){
		activity<-c(activity,"WALKING_DOWNSTAIRS")
		}
	else if (y_bind[i,]==4){
		activity<-c(activity,"SITTING")
		}
	else if (y_bind[i,]==5){
		activity<-c(activity,"STANDING")
		}
	else {
		activity<-c(activity,"LAYING")
		}
}
data.frame(activity)
data_labled<-cbind(activity,data_combined)

#Appropriately labels the data set with descriptive variable names. #

feature<-features[,2]
names<-c("ACTIVITY","SUBJECT","LABLE",as.character(feature))
colnames(data_labled)<-names

#Extracts only the measurements on the mean and standard deviation for each measurement#

index<-NULL
for(i in 1:561){
	if (grepl("mean",as.character(features[i,2]))){
		index<-c(index,i+3)
		}
	else if (grepl("std",as.character(features[i,2]))){
		index<-c(index,i+3)
		}
}

 data_mean_std<-data_labled[,c(1,2,index)]

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject#

data_melted <- melt(data_mean_std, id.vars=c("ACTIVITY", "SUBJECT"))
data_tidy<-ddply(data_melted, c("ACTIVITY", "SUBJECT", "variable"), summarise,
      mean = mean(value), sd = sd(value))

write.table(data_tidy,file="Tidy_data.txt",row.names = FALSE)

