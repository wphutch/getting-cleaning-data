# 
# Getting and cleaning data
#  Acknowledgement of authors
#  [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
#  Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support 
#  Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). 
#  Vitoria-Gasteiz, Spain. Dec 2012
#
# Get working directory.  We will need to reset it twice.
#
wd<-getwd()
# 
# read activity labels and features from parent directory.
#
activity_labels <- read.table("activity_labels.txt")
features<-read.table("features.txt")
#
#  Read test datasets.  X are the metrics, subject are tbe subjects (i.e. volunteers)
#  Y are the activity ID's that refer to the activity labels above.
#
setwd("test")
subject.test <- read.table("subject_test.txt")
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
#
# move to training directory and read those data sets.
# X are the metrics; subject are the subjects (i.e. volunteers)
# Y are the activity ID's that refer to the table of activities above.
#
setwd(wd)
setwd("train")
subject.train <- read.table("subject_train.txt")
x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
# 
# As a matter of hygiene, reset the working directory
# 
setwd(wd)
#
# Combine the test and training datasets
#
x_tot<-rbind(x_train, x_test)
y_tot<-rbind(y_train, y_test)
subject_tot <- rbind(subject_train, subject.test)
# 
#  We have to clean up the metric names from the features file.  There are 
#  duplicates in the features file.  So we paste the sequence number to the 
#  metric.  Also, R seemed to have issues with parentheses in column names.  
#  The outer gsub removes the right parenthesis.  The inner one removes the left.
#
names(x_tot) <- paste(gsub("\\)","",gsub("\\(","",features$V2)),as.character(features$V1))
#
#  Give other tables mnemonic names per assignment instructions.
#
names(y_tot) <- c("activity_id")
names(subject_tot) <- c("subject_id")
#
# The X, Y, and subjeect tables are in the same order, but they do not have a
# common field to merge them.  We create the common field here and cbind it to
# the three tables.
#
id<-seq(1:nrow(y_tot))
names(id) <- c("id")
names(activity_labels) <- c("activity_cd", "activity_desc")
subject_tot <- cbind(id, subject_tot)
y_tot <- cbind (id, y_tot)
#  
# We only need a small subset of columns from the x table because we only
# will be calculating averages for the mean and standard deviation tables.
#
x_narrow <- select(x_tot, 
                   contains("mean-X ")|
                     contains("mean-Y ")| 
                     contains("mean-z ")| 
                     contains("std-X ")| 
                     contains("std-Y ")|
                     contains("std-Z ")|
                     contains("-mean ")|
                     contains("-std "))
#
# Save the names of the metrics from the pruned data set.  Then cbind it to the
# ID table.
#
x_metrics <- names(x_narrow)
x_narrow <- cbind (id, x_narrow) 
#
#  Now merge all the tables together two at a time. First add the activity
#  Descriptions to the activity codes.
#
activity <- merge(y_tot, activity_labels, by.x = "activity_id", by.y = "activity_cd")
# 
#  Now merge this together with the subject tables.
#
activity_subject <- merge (activity, subject_tot, by.x = "id", by.y = "id")
# 
# Finally, merge this to the x table, the table of metrics.
#
final_set <- merge(activity_subject, x_narrow, by.x = "id", by.y = "id")
#
#  Now create the answers by grouping the final set by activity and subject.  
#  We compute the summaries by using the "summarize at" function together with
#  the names of the pruned fields we saved in x-metrics above.  
#
tidy_set<-final_set %>%
  group_by(subject_id, activity_desc) %>%
  summarise_at(vars(one_of(x_metrics)), mean)
#
# done
#