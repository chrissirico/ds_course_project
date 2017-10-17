# Chris Sirico
# Run Analysis on UCI HAR data
#
# You should create one R script called run_analysis.R that does the following.
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation
# for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.

library(tidyverse)
library(readr)


# Read in training subjects, activities (y_train.txt), data (x_train.txt)
features <- read.table("./features.txt")
# activity_labels <- read.table("./activity_labels.txt") # don't actually need to load these

test_set <- read.table("./test/X_test.txt")
test_labels <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

subject_train <- read.table("./train/subject_train.txt")
train_set <- read.table("./train/X_train.txt")
train_labels <- read.table("./train/y_train.txt")


# Preview our objects

# features # we'll need to clean up these names
# activity_labels # number to activity ; we can correlate these manually
# 
# test_set # 2947 rows, 561 cols corresponding to features, only need `mean`s and `std`s
# test_labels # correspond to activities per activity_labels
# subject_test # column w/ subject id
# 
# train_set # 7352 rows, 561 cols corresponding to features
# train_labels # correspond to activities per activity_labels
# subject_train # column w/ subject id


# clean up features column names
  # insert underscore at lower/Upper boundaries; sub underscores for periods, hyphens & commas; nix parens; lowercase everything
  features_snake <- str_replace_all(features$V2, c("([a-z])([A-Z])" = "\\1_\\2", "[,\\-\\.]" = "_", "[\\(\\)]" = "")) %>%
    str_to_lower()
  # make duplicate column names unique
  features_snake[317:330] <- str_replace(features_snake[317:330], "(f_body_acc_bands_energy_)", "\\1ii_")
  features_snake[331:344] <- str_replace(features_snake[331:344], "(f_body_acc_bands_energy_)", "\\1iii_")
  features_snake[396:409] <- str_replace(features_snake[396:409], "(f_body_acc_jerk_bands_energy_)", "\\1ii_")
  features_snake[410:423] <- str_replace(features_snake[410:423], "(f_body_acc_jerk_bands_energy_)", "\\1iii_")
  features_snake[475:488] <- str_replace(features_snake[475:488], "(f_body_gyro_bands_energy_)", "\\1ii_")
  features_snake[489:502] <- str_replace(features_snake[489:502], "(f_body_gyro_bands_energy_)", "\\1iii_")
  # make abbreviations more descriptive
  features_snake <- str_replace_all(features_snake, c("_acc_" = "_accelerometer_",
                                                      "_mag_" = "_magnitude_",
                                                      "^t" = "time",
                                                      "^f" = "freq",
                                                      "_std" = "standard_dev",
                                                      "anglex" = "angle_x",
                                                      "angley" = "angle_y",
                                                      "anglez" = "angle_z",
                                                      "anglet" = "angle_time"
                                                      ))
  
# what's redundant in feature names? Doesn't appear that the results themselves are duplicative.
# features_dup <- group_by(tibble::as_tibble(features_snake), value) %>%
# count() %>% arrange(desc(n)) %>% View()

  
# TEST SET  
  
# add features_snake as colnames to test_set
colnames(test_set) <- features_snake

# add activity column (as factor) to test_set
test_labels$V1 <- as.character(test_labels$V1)
test_labels$V1 <- as.factor(plyr::revalue(test_labels$V1, c("1" = "walking",
                                "2" = "upstairs",
                                "3" = "downstairs",
                                "4" = "sitting",
                                "5" = "standing",
                                "6" = "laying")))
test_set$activity <- test_labels$V1

# add subject column to test_set
test_set$subject <- as.integer(subject_test$V1)

# put new variables first
test_set <- select(test_set, subject, activity, everything())
glimpse(test_set)


# TRAINING SET  

# add features_snake as colnames to train_set
colnames(train_set) <- features_snake

# add activity column (as factor) to train_set
train_labels$V1 <- as.character(train_labels$V1)
train_labels$V1 <- as.factor(plyr::revalue(train_labels$V1, c("1" = "walking",
                                                            "2" = "upstairs",
                                                            "3" = "downstairs",
                                                            "4" = "sitting",
                                                            "5" = "standing",
                                                            "6" = "laying")))
train_set$activity <- train_labels$V1

# add subject column to train_set
train_set$subject <- as.integer(subject_train$V1)

# put new variables first
train_set <- select(train_set, subject, activity, everything())
glimpse(train_set)


# JOIN 'EM
full_set <- full_join(train_set, test_set)
glimpse(full_set) #winning ヾ(-‿- )ゞ


# Extract only the measurements on the mean and standard deviation
# for each measurement.

full_set <- select(full_set, subject, activity, contains("mean"), contains("standard_dev"))
glimpse(full_set)

summary_set <- group_by(full_set, subject, activity) %>%
  summarize_all(mean, na.rm = TRUE)

glimpse(summary_set) # =) looks good

# Save down some CSVs
write_csv(summary_set, "./UCIHAR_summary_set.csv", append = F, col_names = T)
write_csv(full_set, "./UCIHAR_full_data_set.csv", append = F, col_names = T)


# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation
# for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.