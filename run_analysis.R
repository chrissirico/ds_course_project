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
library(stringr)

# Read in training subjects, activities (y_train.txt), data (x_train.txt)
features <- read.table("./features.txt")

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
  # insert underscore at lower/Upper boundaries; sub underscores for periods, hyphens & commas; nix parens;
  # lowercase everything
  features_snake <- str_replace_all(
    features$V2, c("([a-z])([A-Z])" = "\\1_\\2", "[,\\-\\.]" = "_", "[\\(\\)]" = "")
    ) %>%
    str_to_lower()
  
  # what's redundant in feature names? Doesn't appear that the results themselves are duplicative.
  # features_dup <- group_by(tibble::as_tibble(features_snake), value) %>%
  # count() %>% arrange(desc(n)) %>% View() # Used to find duplicate col names
  
  # make duplicate column names unique
  t(features_snake) %>%
  make.unique() -> features_snake
  
  # features_snake[317:330] <- str_replace(features_snake[317:330], "(f_body_acc_bands_energy_)", "\\1ii_")
  # features_snake[331:344] <- str_replace(features_snake[331:344], "(f_body_acc_bands_energy_)", "\\1iii_")
  # features_snake[396:409] <- str_replace(features_snake[396:409], "(f_body_acc_jerk_bands_energy_)", "\\1ii_")
  # features_snake[410:423] <- str_replace(features_snake[410:423], "(f_body_acc_jerk_bands_energy_)", "\\1iii_")
  # features_snake[475:488] <- str_replace(features_snake[475:488], "(f_body_gyro_bands_energy_)", "\\1ii_")
  # features_snake[489:502] <- str_replace(features_snake[489:502], "(f_body_gyro_bands_energy_)", "\\1iii_")

    # make abbreviations more descriptive
  features_snake <- str_replace_all(features_snake, c("_acc_" = "_accelerometer_",
                                                      "_mag_" = "_magnitude_",
                                                      "^t" = "time",
                                                      "^f" = "freq",
                                                      "_std" = "_standard_dev",
                                                      "anglex" = "angle_x",
                                                      "angley" = "angle_y",
                                                      "anglez" = "angle_z",
                                                      "anglet" = "angle_time"
                                                      ))

  
# TEST SET  
  
# add features_snake as colnames to test_set
colnames(test_set) <- features_snake

# add activity column (as factor) to test_set
test_set$activity <- as.factor(plyr::revalue(as.character(test_labels$V1), c("1" = "walking",
                                "2" = "upstairs",
                                "3" = "downstairs",
                                "4" = "sitting",
                                "5" = "standing",
                                "6" = "laying")))

# add subject column to test_set
test_set$subject <- as.integer(subject_test$V1)

# put new variables first
test_set <- select(test_set, subject, activity, everything())
glimpse(test_set)


# TRAINING SET  

# add features_snake as colnames to train_set
colnames(train_set) <- features_snake

# add activity column (as factor) to train_set
train_set$activity <- as.factor(plyr::revalue(as.character(train_labels$V1), c("1" = "walking",
                                                            "2" = "upstairs",
                                                            "3" = "downstairs",
                                                            "4" = "sitting",
                                                            "5" = "standing",
                                                            "6" = "laying")))

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

ms_set <- select(full_set, subject, activity, contains("mean"), contains("standard_dev"))
glimpse(ms_set)


summary_set <- group_by(ms_set, subject, activity) %>%
  summarize_all(mean, na.rm = TRUE)


glimpse(summary_set) # =) looks good

# another way: aggregate() instead of summarize_all()
summary_set_agg <- tibble::as.tibble(aggregate(. ~ activity + subject, ms_set, mean))

all.equal(summary_set, summary_set_agg) # demonstrate these are the same

# Save down some CSVs
write.table(summary_set_agg, "./UCIHAR_summary_set.txt", append = F, col.names = T, row.names = F)
write_csv(summary_set_agg, "./UCIHAR_summary_set.csv", append = F, col_names = T)
write_csv(full_set, "./UCIHAR_full_data_set.csv", append = F, col_names = T)


# Objectives
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation
# for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.

#Learn how to aggregate

## Compute the averages for the variables in 'state.x77', grouped
## according to the region (Northeast, South, North Central, West) that
## each state belongs to.
aggregate(state.x77, list(Region = state.region), mean)

## Compute the averages according to region and the occurrence of more
## than 130 days of frost.
aggregate(state.x77,
          list(Region = state.region,
               Cold = state.x77[,"Frost"] > 130),
          mean)
## (Note that no state in 'South' is THAT cold.)


## example with character variables and NAs
testDF <- data.frame(v1 = c(1,3,5,7,8,3,5,NA,4,5,7,9),
                     v2 = c(11,33,55,77,88,33,55,NA,44,55,77,99) )
by1 <- c("red", "blue", 1, 2, NA, "big", 1, 2, "red", 1, NA, 12)
by2 <- c("wet", "dry", 99, 95, NA, "damp", 95, 99, "red", 99, NA, NA)
aggregate(x = testDF, by = list(by1, by2), FUN = "mean")

# and if you want to treat NAs as a group
fby1 <- factor(by1, exclude = "")
fby2 <- factor(by2, exclude = "")
aggregate(x = testDF, by = list(fby1, fby2), FUN = "mean")


## Formulas, one ~ one, one ~ many, many ~ one, and many ~ many:
aggregate(weight ~ feed, data = chickwts, mean)
aggregate(breaks ~ wool + tension, data = warpbreaks, mean)
aggregate(cbind(Ozone, Temp) ~ Month, data = airquality, mean)
aggregate(cbind(ncases, ncontrols) ~ alcgp + tobgp, data = esoph, sum)

## Dot notation:
aggregate(. ~ Species, data = iris, mean)
aggregate(len ~ ., data = ToothGrowth, mean)

## Often followed by xtabs():
ag <- aggregate(len ~ ., data = ToothGrowth, mean)
xtabs(len ~ ., data = ag)


## Compute the average annual approval ratings for American presidents.
aggregate(presidents, nfrequency = 1, FUN = mean)
## Give the summer less weight.
aggregate(presidents, nfrequency = 1,
          FUN = weighted.mean, w = c(1, 1, 0.5, 1))