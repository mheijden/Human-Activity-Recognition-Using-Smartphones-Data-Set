# I left str() commented in the code that can help checking the code steps





library(stringr)
pa<-"D:\\git\\phones\\UCI HAR Dataset\\"
tr<-"train\\"
ts<-"test\\"
patr<-paste(pa,tr,sep="")
pats<-paste(pa,ts,sep="")
# The next step is adding the variable names to the data and 
#adding the categories for the rows (subjects[persons] and activities) 
# then naming the rows to discern the different sourcefiles(if needed). 
# I removed the brackets in the variable names that prevented me from
# casting them into a list
# the train and test files have different length, meaning that 
# the subject (person) and activity lists are different for these files 
# have to be loaded and bound before the files are made into a single file.

# the variables are for all the same
vars<-read.csv(paste(pa,"features.txt",sep=""),sep=" ",header=FALSE)
# and the activities are all the same
act_lab=read.csv(paste(pa,"activity_labels.txt",sep=""),sep=" ",header=FALSE) # then the activity lables
vr<-sub("\\(\\)","",vars[,2])   #removed the brackets in varnames that prevented casting them into a list
vr<-as.list(vr)
# the training dataset
x_tr<-read.table(paste(patr,"x_train.txt",sep=""), colClasses="numeric",header=FALSE)
# indentation to show that these steps are almost identical to above
    suj<-read.table(paste(patr,"subject_train.txt",sep=""),header=FALSE) #getting the subjects for training files
    srcd<-paste("train",c(1:length(suj[,1])),sep="")
    act<-read.csv(paste(patr,"y_train.txt",sep=""),header=FALSE) # getting the numerics for activities
    act<-merge(act,act_lab) # assigning the lables to the rows as fetched above
    x_tr<-cbind(suj,act[,2],x_tr)
    names(x_tr)<-c("person","activity",vr)   # naming the variables fetched above
    row.names(x_tr)<-srcd   # to discern test and train data
# the training dataset
x_ts<-read.table(paste(pats,"x_test.txt",sep=""), colClasses="numeric",header=FALSE)
# indentation to show that these steps are almost identical to above
    suj<-read.table(paste(pats,"subject_test.txt",sep=""),header=FALSE) #getting the subjects for training files
    srcd<-paste("test",c(1:length(suj[,1])),sep="")
    act<-read.csv(paste(pats,"y_test.txt",sep=""),header=FALSE) # getting the numerics for activities
    act<-merge(act,act_lab) # assigning the lables to the rows as fetched above
    x_ts<-cbind(suj,act[,2],x_ts)
    names(x_ts)<-c("person","activity",vr)   # naming the variables fetched above
    row.names(x_ts)<-srcd  # to discern test and train data
    # get together, to prevent memory issue add x_ts to x_tr
x_tr<-rbind(x_tr,x_ts)
rm(x_ts) #clean up
# then assign() a new name for better quality reference
assign("phonemotion",x_tr)   # a meaninful name for the combined set
phonemotion<-phonemotion[,grepl("acti|person|mean|std",names(phonemotion))]
# str(phonemotion)

# All has been brought together in one single data.frame.
# Now time to calculate for all colums containing mean and std values as required

phonemotion<-with(phonemotion,phonemotion[order(phonemotion[,1],phonemotion[,2]),])
motionsumar<-aggregate(phonemotion,list(phonemotion$person,phonemotion$activity),FUN=mean)
# The aggregate function also attempts to aggregate the grouping columns, 
# which was not intended and adds the Group.# columns for each grouping variable. 
# The original colums are not usable, getting NA values or unwanted aggregate values
# and are therefor deleted, the Group columns renamed
motionsumar<-motionsumar[,-(3:4)]
names(motionsumar)[names(motionsumar)=="Group.1"]<-"person"
names(motionsumar)[names(motionsumar)=="Group.2"]<-"activity"
str(motionsumar)
write.csv(motionsumar, file =".\\motionmeans.csv")