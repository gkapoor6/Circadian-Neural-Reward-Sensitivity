library(psych)
library(tidyverse)
library(dplyr)


# Load in Time of Day Data ------------

# Time of day logs from Qualtrics
log_86 <- data.frame(haven::read_sav("st0086 Log Data Entry.sav"))
log_94 <- data.frame(haven::read_sav("st0094 Log Data Entry.sav"))
log_96 <- data.frame(haven::read_sav("st0096 Log Data Entry.sav"))
log_100 <- data.frame(haven::read_sav("st0100 Log Data Entry.sav"))
log_102 <- data.frame(haven::read_sav("st0102 Log Data Entry.sav"))
log_104 <- data.frame(haven::read_sav("st0104 Log Data Entry.sav"))
log_107 <- data.frame(haven::read_sav("st0107 Log Data Entry.sav"))

# DATA 1 LOAD IN ERP DATA ------

# Study IDs and study names
study_ids <- c("st0086", "st0094", "st0096", "st0100", "st0102", "st0104", "st0107")
study_names <- c("erp_86", "erp_94", "erp_96", "erp_100", "erp_102", "erp_104", "erp_107")

# Loop over the study IDs
for (i in seq_along(study_ids)) {
  
  # Get the study ID and variable name for this iteration
  id <- study_ids[i]
  study_name <- study_names[i]
  # Construct existing and modified files names
  input_file <- paste0(id, "_doors_200-350m.txt")
  output_file <- paste0(id, "_doors_200-350m_modified.txt")
  # Read the .txt file to get the original data
  lines <- readLines(input_file)
  # Modify header into new .txt file
  header <- lines[1]
  new_header <- gsub("Baseline Correction Wins", "Wins", header)
  new_header <- gsub("Baseline Correction Losses", "Losses", new_header)
  lines[1] <- new_header
  # Write the modified lines to the new .txt file
  writeLines(lines, output_file)
  # Read in modified .txt file
  data <- read.table(output_file, sep="", header=TRUE, fill=TRUE, check.names=FALSE)
  # TODO: KEANAN CHECK CALC BELOW
  # Get RewP at Cz (Joyner et al., 2019)
  data$RewPCz <- data$`Cz-Wins` - data$`Cz-Losses`
  # Assign the data frame to a variable
  assign(study_name, data)
}

# Clean up ERP data ----------------
# Change column names
# Remove extra letters from subnames
# Keep only Cz-Wins, Cz-Losses, and RewPCz (Joyner et al., 2019)

# STUDY 86
colnames(erp_86)[1] <- "subname"
# Subject 1020 has two rows, remove the first row (?????)
erp_86 <- erp_86[!(erp_86$subname == "1020_Doors"), ]
erp_86[[1]] <- gsub("_Doors", "", erp_86[[1]])
erp_86 <- dplyr::select(erp_86, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(erp_86) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# STUDY 94
colnames(erp_94)[1] <- "subname"
erp_94[[1]] <- gsub("_Doors", "", erp_94[[1]])
erp_94 <- dplyr::select(erp_94, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(erp_94) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# STUDY 96
colnames(erp_96)[1] <- "subname"
erp_96[[1]] <- gsub("_doors_", "_", erp_96[[1]])
erp_96[[1]] <- gsub("_doors", "", erp_96[[1]])
erp_96 <- dplyr::select(erp_96, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(erp_96) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# STUDY 100
colnames(erp_100)[1] <- "subname"
erp_100[[1]] <- gsub("_doors", "", erp_100[[1]])
erp_100 <- dplyr::select(erp_100, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(erp_100) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")


# STUDY 102
colnames(erp_102)[1] <- "subname"
erp_102[[1]] <- gsub("_doors", "", erp_102[[1]])
erp_102[[1]] <- gsub("_Doors", "", erp_102[[1]])
erp_102 <- dplyr::select(erp_102, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(erp_102) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# STUDY 104
colnames(erp_104)[1] <- "subname"
erp_104[[1]] <- gsub("_Doors_convert.cdt", "", erp_104[[1]])
erp_104 <- dplyr::select(erp_104, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(erp_104) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# STUDY 107
colnames(erp_107)[1] <- "subname"
erp_107[[1]] <- gsub("_Doors.cdt", "", erp_107[[1]])
erp_107[[1]] <- gsub("_Oddball2_convert.cdt", "", erp_107[[1]])
erp_107[[1]] <- gsub("_convert.cdt", "", erp_107[[1]])
erp_107[[1]] <- gsub("_Doors", "", erp_107[[1]])
erp_107[[1]] <- gsub("_Doors_Oddball2_convert.cdt", "", erp_107[[1]])
erp_107$subname[erp_107$subname == "st0107_2099"] <- "st0107_s2099"
erp_107$subname[erp_107$subname == "st0107_2101"] <- "st0107_s2101"
erp_107 <- dplyr::select(erp_107, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(erp_107) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# ERP and Log Data Management ---------------------------------------------------------

## OLD CODE ##
# Calculate RewP for Study 96
# erp_94$RewP_FCz <- erp_94$RewPmFCzG - erp_94$RewPmFCzL
# Gain, Loss, RewP, Age
# erp_86 <- dplyr::select(erp_86, subname,FCz_Gain_w3,FCz_Loss_w3,rewp_mean_FCz_w3, AGE)
# erp_94 <- dplyr::select(erp_94, subname,RewPmFCzG,RewPmFCzL,RewP_FCz, AGE)
# colnames(erp_86) <- c("subname","FRN_Gain_FCz","FRN_Loss_FCz","RewP_FCz","Age")
# colnames(erp_94) <- c("subname","FRN_Gain_FCz","FRN_Loss_FCz","RewP_FCz","Age")


# Start and End Times for Doors Task (Studies 86 - 107)
log_86 <- dplyr::select(log_86, subname_log, doors_start, doors_end, hookups_start)
log_94 <- dplyr::select(log_94, subname_log, doors_start, doors_end, hookups_start)
log_96 <- dplyr::select(log_96, subname_log, doors_start, doors_end, hookups_start)
log_100 <- dplyr::select(log_100, subname_log, doors_start, doors_end, hookups_start)
log_102 <- dplyr::select(log_102, subname_log, doors_start, doors_end, hookups_start)
log_104 <- dplyr::select(log_104, subname_log, doors_start, doors_end, hookups_start)
#log_107 <- dplyr::select(log_107, subname_log, doors_start, doors_end, hookups_start)

# Cleaning up LOG subject naming errors ---------------

###### Study 86
log_86$subname_log[log_86$subname_log == "st0086_s1182_20130620"] <- "st0086_s1182"
log_86$subname_log[log_86$subname_log == "st0086_s1081_20120405"] <- "st0086_s1081"
log_86$subname_log[log_86$subname_log == "st0086_s1070_20120206"] <- "st0086_s1070"
log_86$subname_log[log_86$subname_log == "st0086_c"] <- "st0086_s1016"

###### Study 94
log_94$subname_log[log_94$subname_log == "st0094_s1125_20150713"] <- "st0094_s1125"
log_94$subname_log[log_94$subname_log == "st0094_s1124_20150711"] <- "st0094_s1124"
log_94$subname_log[log_94$subname_log == "st0094_s1103_20150430"] <- "st0094_s1103"

###### Study 96 --- NO ISSUES, subnames match that of data2_96

###### Study 100
# Remove random letters
log_100$subname_log <- gsub("[A-Za-z]", "", log_100$subname_log)
# Add prefix st0100_p to all subnames_log column values
log_100$subname_log <-  paste0("st0100_p", log_100$subname_log)
# Remove first row
log_100 <- log_100[-1,]

###### Study 102
log_102$subname_log[log_102$subname_log == "st0102_9003"] <- "st0102_p9003"
log_102$subname_log[log_102$subname_log == "st0102_9011"] <- "st0102_p9011"
log_102$subname_log[log_102$subname_log == "st0102_9012"] <- "st0102_p9012"

##### Study 104 --- NO ISSUES
##### Study 107 --- NO ISSUES

##### Merge ERP and Log data  ---------------------------------------------------------

erplog_86 <- merge(erp_86, log_86, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
erplog_94 <- merge(erp_94, log_94, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
erplog_96 <- merge(erp_96, log_96, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
erplog_100 <- merge(erp_100, log_100, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
erplog_102 <- merge(erp_102, log_102, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
erplog_104 <- merge(erp_104, log_104, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
erplog_107 <- merge(erp_107, log_107, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")


erplog_86$Study <- "86"
erplog_94$Study <- "94"
erplog_96$Study <- "96"
erplog_100$Study <- "100"
erplog_102$Study <- "102"
erplog_104$Study <- "104"
erplog_107$Study <- "107"

#### Merge ALL STUDIES ERP + LOG DATA
data <- rbind(erplog_86, erplog_94, erplog_96, erplog_100, erplog_102, erplog_104)
data$Cap <- NA

# Studies with similar type of cap (Cap 0)
data$Cap[data$Study == "86"] <- 0
data$Cap[data$Study == "94"] <- 0
data$Cap[data$Study == "96"] <- 0
data$Cap[data$Study == "100"] <- 0

# Studies with similar type of cap (Cap 1)
data$Cap[data$Study == "102"] <- 1
data$Cap[data$Study == "104"] <- 1
#data$Cap[data$Study == "107"] <- 1

# TODO
## INVESTIGATE DIFFERENCES -- WHAT FACTORS CAN EXPLAIN VARIANCE
# DIFFERENCES BETWEEN STUDIES, look at averages of demographics
# Time of day when studies were conducted, doors task,
# Seasonality (month/season)
# Hookups/exp start time vs. doors start time - subset of people
# Plot: distribution of individual points 
# (how many people at 10 am, etc.), weight function by density of data points
# What studies look similar to each other? And different? And why?
# Community participants vs. college students coming in at different times

# Clean up time variable  ---------------------------------------------------------
str(data$doors_start)
data$doors_start <- format(as.POSIXct(data$doors_start,format='%I:%M %p'),format="%H:%M:%S")

data <- data[complete.cases(data),]

data$RefDate <- "10/19/2024"
data$RefTime <- "00:00:00"

data$Ref <- as.POSIXct(paste(data$RefDate, data$RefTime), format="%m/%d/%Y %H:%M:%S")
data$doors_start2 <- as.POSIXct(paste(data$RefDate, data$doors_start), format="%m/%d/%Y %H:%M:%S")
data$Time <- as.numeric(data$doors_start2 - data$Ref)

# Create cosinor parameters for analysis
data$cos_Time <- cos(2 * pi * data$Time / 24)
data$sin_Time <- sin(2 * pi * data$Time / 24)

# PSYCHDATA -- Clean Psychopathology & Demographics ---------------------------------------------------------

# subname
# AGE
# GENDER
# SLEEP
# HEIGHT & WEIGHT FOR BMI
# CAFFEINE - USUAL CONSUMPTION AND NOT DAY-OF 
# RACE
# IDAS VARIABLES (12)


### STUDY 86
# LIMITATION CAFFEINE QUESTIONNAIRE - 
# NO DATA MISSING FOR 18 PARTICIPANTS!
st0086 <- foreign::read.spss("st86_Demographics.sav", to.data.frame = T)
st0086_idas <- foreign::read.spss("st86_IDAS_scored.sav", to.data.frame = T)
psych1_86 <- dplyr::select(st0086, SUBNAME, AGE, GENDER, SLEEP, 
                           HEIGHT, WEIGHT, 
                           CAFFEINE, RACE)
psych2_86 <- dplyr::select(st0086_idas, SUBNAME,
                           gd_tot, dys_tot, las_tot, ins_tot,
                           sui_tot, al_tot, ag_tot, tem_tot,
                           wb_tot, sa_tot, pan_tot, ti_tot)

psych_86 <- merge(psych1_86, psych2_86, all.x = T, all.y = T, by.x = "SUBNAME", by.y = "SUBNAME")

# SUBNAME
psych_86$SUBNAME <- trimws(psych_86$SUBNAME)

# GENDER
psych_86$GENDER <- trimws(psych_86$GENDER)
psych_86$GENDER <- gsub("999", NA, psych_86$GENDER)
psych_86$GENDER <- as.factor(psych_86$GENDER)

# AGE
psych_86$AGE <- gsub("999", NA, psych_86$AGE)
psych_86$AGE  <- as.numeric(psych_86$AGE)

# SLEEP LAST NIGHT
# Trim white spaces
psych_86$SLEEP <- trimws(psych_86$SLEEP)
psych_86$SLEEP <- gsub("10\\+", "10", psych_86$SLEEP) 
psych_86$SLEEP <- gsub("7 to 8", "7.5", psych_86$SLEEP)
psych_86$SLEEP <- gsub("6.5-7", "6.75", psych_86$SLEEP)
psych_86$SLEEP <- gsub("999", NA, psych_86$SLEEP)
psych_86$SLEEP <- as.numeric(psych_86$SLEEP)

# HEIGHT
psych_86$HEIGHT <- trimws(psych_86$HEIGHT)
psych_86$HEIGHT <- gsub("999", NA, psych_86$HEIGHT) 
psych_86$HEIGHT <- gsub("140", NA, psych_86$HEIGHT)
psych_86$HEIGHT <- as.numeric(psych_86$HEIGHT)
psych_86$HEIGHT <- psych_86$HEIGHT * 12

# WEIGHT
psych_86$WEIGHT <- gsub("999", NA, psych_86$WEIGHT)
psych_86$WEIGHT <- gsub("5.1", NA, psych_86$WEIGHT)
psych_86$WEIGHT <- as.numeric(psych_86$WEIGHT)

# CAFFEINE
psych_86$CAFFEINE <- trimws(psych_86$CAFFEINE)
psych_86$CAFFEINE <- gsub("no more than 1", "1", psych_86$CAFFEINE)
psych_86$CAFFEINE <- gsub("not much", "0.5", psych_86$CAFFEINE)
psych_86$CAFFEINE <- gsub("3 or 4", "3.5", psych_86$CAFFEINE)
psych_86$CAFFEINE <- gsub("2 to 3", "2.5", psych_86$CAFFEINE)
psych_86$CAFFEINE <- gsub("0 to 1", "0.5", psych_86$CAFFEINE)
psych_86$CAFFEINE <- gsub("1 at most", "1", psych_86$CAFFEINE)
psych_86$CAFFEINE <- gsub("999", NA, psych_86$CAFFEINE)
psych_86$CAFFEINE <- as.numeric(psych_86$CAFFEINE)

summary(psych_86)

### STUDY 94
# TODO RACE (NEED CODING SCHEME - DANIELLE)
st0094 <- foreign::read.spss("st0094_RewP_sx.sav", to.data.frame = T)
psych_94 <- dplyr::select(st0094, subname, AGE, GENDER, SLEEP,
                          HEIGHT, WEIGHT,
                          CAFFEINE, RACE,
                          gd_tot, dys_tot, las_tot, ins_tot,
                          sui_tot, al_tot, ag_tot, tem_tot,
                          wb_tot, sa_tot, pan_tot, ti_tot)

psych_94 <- psych_94 %>%
  rename(SUBNAME = subname)


# TODO RACE
psych_94$RACE <- gsub("999", NA, psych_94$RACE)
psych_94$RACE <- as.numeric(psych_94$RACE)
psych_94$RACE <- gsub("1", "Caucasian", psych_94$RACE)
psych_94$RACE <- gsub("2", "African American", psych_94$RACE)

# SUBNAME
psych_94$SUBNAME <- trimws(psych_94$SUBNAME)

# GENDER
psych_94$GENDER <- trimws(psych_94$GENDER)
psych_94$GENDER <- gsub("male", "Male", psych_94$GENDER)
psych_94$GENDER <- gsub("feMale", "Female", psych_94$GENDER)
psych_94$GENDER <- gsub("999", NA, psych_94$GENDER)
psych_94$GENDER <- as.factor(psych_94$GENDER)

# SLEEP
psych_94$SLEEP <- as.numeric(psych_94$SLEEP)

# HEIGHT
psych_94$HEIGHT <- gsub("'", " ", psych_94$HEIGHT)
psych_94$HEIGHT <- gsub("''", "", psych_94$HEIGHT)
psych_94$HEIGHT <- gsub("     ", "", psych_94$HEIGHT)
psych_94$HEIGHT <- gsub("  ", " ", psych_94$HEIGHT)
psych_94$HEIGHT <- trimws(psych_94$HEIGHT)
psych_94$HEIGHT <- as.numeric(psych_94$HEIGHT)
psych_94$HEIGHT <- psych_94$HEIGHT * 12

# CAFFEINE
psych_94$CAFFEINE <- trimws(psych_94$CAFFEINE)
psych_94$CAFFEINE <- gsub("3, 4", "3.5", psych_94$CAFFEINE)
psych_94$CAFFEINE <- gsub("0-1", "0.5", psych_94$CAFFEINE)
psych_94$CAFFEINE <- gsub("1-2", "1.5", psych_94$CAFFEINE)
psych_94$CAFFEINE <- gsub("2, 5", "3.5", psych_94$CAFFEINE)
psych_94$CAFFEINE <- gsub("1 caffeine pill", "1", psych_94$CAFFEINE)
# Convert 300-500mg (400mg) to 4 cups of coffee
psych_94$CAFFEINE <- gsub("300-500mg", "4", psych_94$CAFFEINE)
psych_94$CAFFEINE <- gsub("400", "4", psych_94$CAFFEINE)
# Remove outliers
psych_94$CAFFEINE <- gsub("42165", NA, psych_94$CAFFEINE)
psych_94$CAFFEINE <- gsub("42006", NA, psych_94$CAFFEINE)
psych_94$CAFFEINE <- gsub("999", NA, psych_94$CAFFEINE)
psych_94$CAFFEINE <- as.numeric(psych_94$CAFFEINE)

summary(psych_94)

### STUDY 96
# TODO RACE CODING SCHEME

st0096 <- foreign::read.spss("st96_PPT_Info_merged.sav", to.data.frame = T)
st0096_idas <- foreign::read.spss("st96_IDAS_mergedscored.sav", to.data.frame = T)
psych1_96 <- st0096 %>%
  dplyr::select(SUBNAME, AGE = Age, GENDER = Gender, SLEEP = Sleep, 
                HEIGHT = Height, WEIGHT = Weight, 
                CAFFEINE = Caffeine, RACE = Race)
psych2_96 <- dplyr::select(st0096_idas, SUBNAME,
                           gd_tot, dys_tot, las_tot, ins_tot,
                           sui_tot, al_tot, ag_tot, tem_tot,
                           wb_tot, sa_tot, pan_tot, ti_tot)
psych_96 <- merge(psych1_96, psych2_96, all.x = T, all.y = T, by.x = "SUBNAME", by.y = "SUBNAME")

# SUBNAME
psych_96$SUBNAME <- trimws(psych_96$SUBNAME)

# GENDER, ASSUMING MALE = 1, FEMALE = 2
psych_96$GENDER <- gsub("999", NA, psych_96$GENDER)
psych_96$GENDER <- gsub("1", "Male", psych_96$GENDER)
psych_96$GENDER <- gsub("2", "Female", psych_96$GENDER)
psych_96$GENDER <- as.factor(psych_96$GENDER)

# T-test for anxiety and depression to check for female coding, 
t.test(gd_tot ~ GENDER, psych_96)

# SLEEP
psych_96$SLEEP <- as.numeric(psych_96$SLEEP)
psych_96$SLEEP[psych_96$SLEEP > 24 | psych_96$SLEEP < 0] <- NA

# HEIGHT
# Convert height to inches
psych_96$HEIGHT <- trimws(psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("'", " ", psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("\"", "", psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("''", "\"", psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("5.10.", 5.10, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("5 11  ", 5.9, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("5 2  ", 5.17, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("6 1  ", 6.1, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("5 7  ", 5.58, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("5 5  ", 5.42, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("6 2", 6.16, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("5 11", 5.11, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("5 2", 5.9, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("6 1", 6.1, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("5 7", 5.7, psych_96$HEIGHT)
psych_96$HEIGHT <- gsub("63", 5.25, psych_96$HEIGHT)
psych_96$HEIGHT <- as.numeric(psych_96$HEIGHT)
psych_96$HEIGHT <- psych_96$HEIGHT * 12

# WEIGHT
psych_96$WEIGHT <- gsub("^\\s+|\\s+$", "", psych_96$WEIGHT)
psych_96$WEIGHT <- gsub("lbs", "", psych_96$WEIGHT)
psych_96$WEIGHT <- gsub("999", NA, psych_96$WEIGHT)
psych_96$WEIGHT <- as.numeric(psych_96$WEIGHT)

# CAFFEINE
psych_96$CAFFEINE <- trimws(psych_96$CAFFEINE)
psych_96$CAFFEINE <- gsub("42769", NA, psych_96$CAFFEINE)
psych_96$CAFFEINE <- as.numeric(psych_96$CAFFEINE)

# RACE
psych_96$RACE <- gsub("999", NA, psych_96$RACE)

summary(psych_96)

### STUDY 100
data2_100_scores <- foreign::read.spss("st0100_merged_questionnaire_scores.sav", to.data.frame = T)
data2_100_raw <- foreign::read.spss("st0100_merged_questionnaires_raw.sav", to.data.frame = T)
psych1_100 <- dplyr::select(data2_100_raw, SubjectID, Age, Gender,
                            Height, Weight, 
                            Race, Caffeine,
                            Sleep.0)

# Replace "p" with "s" in data2_100_scores
data2_100_scores[[1]] <- gsub("p", "s", data2_100_scores[[1]])
psych2_100 <- dplyr::select(data2_100_scores, SubjectID, 
                            gd_tot, dys_tot, las_tot, ins_tot,
                            sui_tot, al_tot, ag_tot, tem_tot,
                            wb_tot, sa_tot, pan_tot, ti_tot)

psych_100 <- merge(psych1_100, psych2_100, all.x = T, all.y = F, by.x = "SubjectID", by.y = "SubjectID")

psych_100 <- psych_100 %>%
  rename(SUBNAME = SubjectID, AGE = Age, GENDER = Gender, SLEEP = Sleep.0, 
         HEIGHT = Height, WEIGHT = Weight, 
         CAFFEINE = Caffeine, RACE = Race)

# SUBNAME
psych_100$SUBNAME <- trimws(psych_100$SUBNAME)
psych_100$SUBNAME <- gsub("_s", "_p", psych_100$SUBNAME)

# GENDER
psych_100$GENDER <- trimws(psych_100$GENDER)
psych_100$GENDER <- as.factor(psych_100$GENDER)

# HEIGHT
psych_100$HEIGHT <- trimws(psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("'", ".", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("\"", "", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("6.", "6.0", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("1.63m", "5.4", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5.6.0", "5.6", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("1.6.0m", "5.24", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5.3 3/4", "5.3", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5.6.03/4", "5.6", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("73 in", "6.0", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5-10", "5.8", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5 foot", "5.0", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5,10", "5.8", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5 foot one inch", "5.1", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5.2..", "5.2", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5.4..", "5.4", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5 feet 3 inches", "5.25", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("6.0feet", "6.0", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("76.0inches", "6.3", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5`3", "5.3", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("6.0 0", "6.0", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5 foot 8in", "5.67", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5.7ft", "5.7", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("6.0 inches", "6.0", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5.0 8in", "5.67", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5.0 one inch", "5.1", psych_100$HEIGHT)
psych_100$HEIGHT <- gsub("5.63/4", "5.6", psych_100$HEIGHT)
psych_100$HEIGHT <- as.numeric(psych_100$HEIGHT)
psych_100$HEIGHT <- psych_100$HEIGHT * 12

# WEIGHT
psych_100$WEIGHT <- gsub("^\\s+|\\s+$", "", psych_100$WEIGHT)
psych_100$WEIGHT <- gsub("lbs", "", psych_100$WEIGHT)
psych_100$WEIGHT <- gsub("999", NA, psych_100$WEIGHT)
psych_100$WEIGHT <- as.numeric(psych_100$WEIGHT)

# CAFFEINE
psych_100$CAFFEINE <- trimws(psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("0-1", "0.5", psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("1-2", "1.5", psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("Not very much", NA, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("1 cup coffee", "1", psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("1 soda", "1", psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("3 cups", "3", psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("2 cans of coke", 2, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("0-8oz", 0.5, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("1 can or cup of coffee", 1, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("1 cup", 1, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("2 cups of coffee/tea", 2, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("0, sometimes 1", 0.5, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("Less than 1", 0.5, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("Zero", 0, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("700", NA, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("2 cups coffee", 2, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("2 cups", 1, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("2-3", 2.5, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("One", 1, psych_100$CAFFEINE)
psych_100$CAFFEINE <- gsub("I drink Arizona tea", NA, psych_100$CAFFEINE)
psych_100$CAFFEINE <- as.numeric(psych_100$CAFFEINE)

# SLEEP
psych_100$SLEEP <- trimws(psych_100$SLEEP)
psych_100$SLEEP <- gsub("5.5-8.0", "6.75", psych_100$SLEEP)
psych_100$SLEEP <- gsub("7-8", "7.5", psych_100$SLEEP)
psych_100$SLEEP <- gsub("7.5-8.0", "7.75", psych_100$SLEEP)
psych_100$SLEEP <- gsub("8-9", "8.5", psych_100$SLEEP)
psych_100$SLEEP <- gsub("6-8", "7", psych_100$SLEEP)
psych_100$SLEEP <- gsub("~7", "7", psych_100$SLEEP)
psych_100$SLEEP <- gsub("8 and 1/2", "8.5", psych_100$SLEEP)
psych_100$SLEEP <- gsub("^$", NA, psych_100$SLEEP)
psych_100$SLEEP <- gsub("^0$", NA, psych_100$SLEEP)
psych_100$SLEEP <- as.numeric(psych_100$SLEEP)
summary(psych_100)


### TODO STUDY 102
# TODO !!! DATA LOOKS WRONG FOR HEIGHT & WEIGHT FOR FIRST 24 PARTICIPANTS
st0102 <- foreign::read.spss("st0102_merged_questionnaire_raw.sav", to.data.frame = T)
st0102_idas <- foreign::read.spss("st0102_questionnaires_scored_p1001tos1092.sav", to.data.frame = T)

psych1_102 <- st0102 %>%
  dplyr::select(SUBNAME = Q1, AGE = Age, GENDER = Gender, SLEEP = Sleep, 
                HEIGHT = Height, WEIGHT = Weight, 
                CAFFEINE = Caffeine, RACE = Race)

psych2_102 <- st0102_idas %>%
  dplyr::select(SUBNAME = subname,
                gd_tot, dys_tot, las_tot, ins_tot,
                sui_tot, al_tot, ag_tot, tem_tot,
                wb_tot, sa_tot, pan_tot, ti_tot)
psych_102 <- merge(psych1_102, psych2_102, all.x = T, all.y = F, by.x = "SUBNAME", by.y = "SUBNAME")

# SUBNAME
psych_102$SUBNAME <- trimws(psych_102$SUBNAME)

# GENDER
psych_102$GENDER <- trimws(psych_102$GENDER)
psych_102$GENDER <- as.factor(psych_102$GENDER)

# SLEEP
psych_102$SLEEP <- trimws(psych_102$SLEEP)
psych_102$SLEEP <- gsub("1-2", "1.5", psych_102$SLEEP)
psych_102$SLEEP <- gsub(",1.5", "1.5", psych_102$SLEEP)
psych_102$SLEEP <- gsub("5 1/2", "5.5", psych_102$SLEEP)
psych_102$SLEEP <- gsub("7-8", "7.5", psych_102$SLEEP)
psych_102$SLEEP <- gsub("7,5", "7.5", psych_102$SLEEP)
psych_102$SLEEP <- gsub("8 hours", "8", psych_102$SLEEP)
psych_102$SLEEP <- gsub("7 1/2", "7.5", psych_102$SLEEP)
psych_102$SLEEP <- gsub("^$", NA, psych_102$SLEEP)
psych_102$SLEEP <- gsub("^0$", NA, psych_102$SLEEP)
psych_102$SLEEP <- as.numeric(psych_102$SLEEP)

# CAFFEINE
psych_102$CAFFEINE <- trimws(psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("1 cup coffee", "1", psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("8 oz coffee", "1", psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("1 can of soda", "1", psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("1-2", "1.5", psych_102$CAFFEINE)
psych_100$CAFFEINE <- gsub("1cup", "1", psych_100$CAFFEINE)
psych_102$CAFFEINE <- gsub("1 bottle", NA, psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("0.5 cups", 0.5, psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("1\2 cup", 0.5, psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("Max 1 coffee", 1, psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("Occasional soda", 0, psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("1 cup", 1, psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("2 cups of coffee", 2, psych_102$CAFFEINE)
psych_102$CAFFEINE <- gsub("1 small cup of coffee", 2, psych_102$CAFFEINE)
psych_102$CAFFEINE <- as.numeric(psych_102$CAFFEINE)
summary(psych_102$CAFFEINE)


# WEIGHT
# TODO !!! DATA LOOKS WRONG FOR HEIGHT & WEIGHT FOR FIRST 24 PARTICIPANTS
psych_102$WEIGHT <- trimws(psych_102$WEIGHT)
psych_102$WEIGHT <- gsub("^\\s+|\\s+$", "", psych_102$WEIGHT)
psych_102$WEIGHT <- gsub("lbs", "", psych_102$WEIGHT)
psych_102$WEIGHT <- gsub("lb", "", psych_102$WEIGHT)
psych_102$WEIGHT <- gsub(" lbs", "", psych_102$WEIGHT)
psych_102$WEIGHT <- gsub(" Ibs", "", psych_102$WEIGHT)
psych_102$WEIGHT <- as.numeric(psych_102$WEIGHT)
psych_102$WEIGHT[1:24] <- NA


# HEIGHT
psych_102$HEIGHT[1:24] <- NA
psych_102$HEIGHT <- trimws(psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("\"", "", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("'", ".", psych_102$HEIGHT) 
psych_102$HEIGHT <- gsub(" +", " ", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("5. 10", "5.10", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("5.4..", "5.4", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("6 feet", "6.0", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("62 inches", "5.2", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("69 inches", "5.75", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("54", "5.4", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("57", "5.7", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("5 ft 3 in", "5.3", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("5ft 8in", "5.8", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("1.7m", "5.5", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("72 in", "6.0", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("6 ft 2 in", "6.2", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("5ft", "5.0", psych_102$HEIGHT)
psych_102$HEIGHT <- gsub("59", "5.9", psych_102$HEIGHT)
psych_102$HEIGHT <- as.numeric(psych_102$HEIGHT)
psych_102$HEIGHT <- psych_102$HEIGHT * 12

summary(psych_102)


### TODO STUDY 104
# TODO !! Half of the participants missing psychodemographic data
# TODO HEIGHT
st0104 <- foreign::read.spss("st0104_IDAS_scored.sav", to.data.frame = T)
st0104 <- st0104 %>%
  mutate(
    RACE = case_when(
      Race_White == "White" ~ "White",
      Race_Black == "Black or African American" ~ "Black",
      Race_AmerInd == "American Indian or Alaska Native" ~ "American Indian",
      Race_AsianInd == "Asian Indian" ~ "Asian Indian",
      Race_Chinese == "Chinese" ~ "Chinese",
      Race_Filipino == "Filipino" ~ "Filipino"
    )
  )

psych_104 <- st0104 %>%
  dplyr::select(SUBNAME = Subj_ID, AGE = Age, GENDER = Gender, SLEEP = Sleep, 
                HEIGHT = Height, WEIGHT = Weight, 
                CAFFEINE = Caffeine, RACE,
                gd_tot, dys_tot, las_tot, ins_tot,
                sui_tot, al_tot, ag_tot, tem_tot,
                wb_tot, sa_tot, pan_tot, ti_tot)

# SUBNAME
psych_104$SUBNAME <- trimws(psych_104$SUBNAME)

# AGE
psych_104$AGE <- as.numeric(psych_104$AGE)

# RACE
psych_104$RACE <- as.factor(psych_104$RACE)

# TODO HEIGHT
psych_104$HEIGHT <- trimws(psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("\"", "", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("'", ".", psych_104$HEIGHT) 
psych_104$HEIGHT <- gsub("74", "6.1", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("5 foot 8 inches", "5.8", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("6 feet", "6.0", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("6 foot", "6.0", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("5 feet 4 inches", "5.4", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("63 inches", "5.25", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("64 inches", "5.25", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("5feet", "5.0", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("5ft 3in", "5.3", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("6ft", "6.0", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("5ft 2in", "5.2", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("5.9..", "5.9", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("5,11", "5.9", psych_104$HEIGHT)
psych_104$HEIGHT <- gsub("6 ft", "6.0", psych_104$HEIGHT)
psych_104$HEIGHT <- as.numeric(psych_104$HEIGHT)
psych_104$HEIGHT <- psych_104$HEIGHT * 12

# WEIGHT
psych_104$WEIGHT <- trimws(psych_104$WEIGHT)
psych_104$WEIGHT <- gsub("lbs", "", psych_104$WEIGHT)
psych_104$WEIGHT <- gsub(" lb", "", psych_104$WEIGHT)
psych_104$WEIGHT <- gsub(" lbs", "", psych_104$WEIGHT)
psych_104$WEIGHT <- gsub(" pounds", "", psych_104$WEIGHT)
psych_104$WEIGHT <- gsub("^$", NA, psych_104$WEIGHT)
psych_104$WEIGHT <- as.numeric(psych_104$WEIGHT)

# SLEEP
psych_104$SLEEP <- trimws(psych_104$SLEEP)
psych_104$SLEEP <- gsub(" hours", "", psych_104$SLEEP)
psych_104$SLEEP <- gsub("8 to 10", "9", psych_104$SLEEP)
psych_104$SLEEP <- gsub("6/7", "6.5", psych_104$SLEEP)
psych_104$SLEEP <- gsub("^$", NA, psych_104$SLEEP)
psych_104$SLEEP <- gsub("^0$", NA, psych_104$SLEEP)
psych_104$SLEEP <- as.numeric(psych_104$SLEEP)

# CAFFEINE
psych_104$CAFFEINE <- trimws(psych_104$CAFFEINE)
psych_104$CAFFEINE <- gsub("2 cups of coffee/soda", 2, psych_104$CAFFEINE)
psych_104$CAFFEINE <- gsub("Cup of coffee", 1, psych_104$CAFFEINE)
psych_104$CAFFEINE <- gsub("1 cup of coffee", 1, psych_104$CAFFEINE)
psych_104$CAFFEINE <- gsub("None", 0, psych_104$CAFFEINE)
psych_104$CAFFEINE <- gsub("8 oz", 1, psych_104$CAFFEINE)
psych_104$CAFFEINE <- gsub("4 cups of tea", 4, psych_104$CAFFEINE)
psych_104$CAFFEINE <- gsub("2-3 cups", 2.5, psych_104$CAFFEINE)
psych_104$CAFFEINE <- gsub("1 can of soda", 1, psych_104$CAFFEINE)
psych_104$CAFFEINE <- gsub("2 or less", 2, psych_104$CAFFEINE)
psych_104$CAFFEINE <- as.numeric(psych_104$CAFFEINE)
summary(psych_104$CAFFEINE)


# TODO STUDY 107
# TODO: Score IDAS vars, Keanan has script
# TODO: Convert DOB to Age (need study date)
st0107 <- foreign::read.spss("st0107_Demographics_RAW.sav", to.data.frame = T)
st0107_idas <- foreign::read.spss("st0107_IDAS_RAW.sav", to.data.frame = T)

psych1_107 <- st0107 %>%
  dplyr::select(SUBNAME, AGE = DOB, GENDER = gender, SLEEP = sleep, 
                HEIGHT = height, WEIGHT = weight, 
                CAFFEINE = caffeine_day, RACE = race)




# MERGE ALL PSYCH + ERP + LOG DATA ----------------


erplogpsych_86 <- merge(erplog_86, psych_86, all.x = F, all.y = F, by.x = "subname", by.y = "SUBNAME")
erplogpsych_94 <- merge(erplog_94, psych_94, all.x = F, all.y = F, by.x = "subname", by.y = "SUBNAME")
erplogpsych_96 <- merge(erplog_96, psych_96, all.x = F, all.y = F, by.x = "subname", by.y = "SUBNAME")
erplogpsych_100 <- merge(erplog_100, psych_100, all.x = F, all.y = F, by.x = "subname", by.y = "SUBNAME")
erplogpsych_102 <- merge(erplog_102, psych_102, all.x = F, all.y = F, by.x = "subname", by.y = "SUBNAME")
erplogpsych_104 <- merge(erplog_104, psych_104, all.x = F, all.y = F, by.x = "subname", by.y = "SUBNAME")
# 107 NOT READY YET erplog_107 <- merge(erp_107, log_107, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")

data_all <- rbind(erplogpsych_86, erplogpsych_94, erplogpsych_96, erplogpsych_100, erplogpsych_102, erplogpsych_104)


# MODEL AND PLOT ALL DATA --------

# Time variable (DOORS START)
data_all$doors_start <- format(as.POSIXct(data_all$doors_start,format='%I:%M %p'),format="%H:%M:%S")

data_all <- data_all[complete.cases(data_all),]

data_all$RefDate <- "04/03/2025"
data_all$RefTime <- "00:00:00"

data_all$Ref <- as.POSIXct(paste(data_all$RefDate, data_all$RefTime), format="%m/%d/%Y %H:%M:%S")
data_all$doors_start2 <- as.POSIXct(paste(data_all$RefDate, data_all$doors_start), format="%m/%d/%Y %H:%M:%S")
data_all$Time <- as.numeric(data_all$doors_start2 - data_all$Ref)

# Create cosinor parameters for analysis
data_all$cos_Time <- cos(2 * pi * data_all$Time / 24)
data_all$sin_Time <- sin(2 * pi * data_all$Time / 24)

# Hookup Times
data_all$hookups_start <- format(as.POSIXct(data_all$hookups_start,format='%I:%M %p'),format="%H:%M:%S")

data_all <- data_all[complete.cases(data_all),]

data_all$RefDate <- "04/03/2025"
data_all$RefTime <- "00:00:00"

data_all$Ref <- as.POSIXct(paste(data_all$RefDate, data_all$RefTime), format="%m/%d/%Y %H:%M:%S")
data_all$hookups_start2 <- as.POSIXct(paste(data_all$RefDate, data_all$hookups_start), format="%m/%d/%Y %H:%M:%S")
data_all$HookupsTime <- as.numeric(data_all$hookups_start2 - data_all$Ref)

data_all$StudyLength <- data_all$doors_start2 - data_all$hookups_start2
data_all$StudyLength[data_all$StudyLength < 0] <- NA

# Cap Type
data_all$Cap <- NA

# Studies with similar type of cap (128 Channel Cap)
data_all$Cap[data_all$Study == "86"] <- 0
data_all$Cap[data_all$Study == "94"] <- 0
data_all$Cap[data_all$Study == "96"] <- 0
data_all$Cap[data_all$Study == "100"] <- 0

# Studies with similar type of cap (64 Channel Cap)
data_all$Cap[data_all$Study == "102"] <- 1
data_all$Cap[data_all$Study == "104"] <- 1
data_all$Cap[data_all$Study == "107"] <- 1

# Seasonality

data_all$Season <- NA

data_all$Season[data_all$Study == "86"] <- "Fall" # November
data_all$Season[data_all$Study == "94"] <- "Fall" # November and early December
data_all$Season[data_all$Study == "96"] <- "Spring" # April
data_all$Season[data_all$Study == "100"] <- "Summer" # May
data_all$Season[data_all$Study == "102"] <- "Summer" # May
data_all$Season[data_all$Study == "104"] <- "Summer" # June
data_all$Season[data_all$Study == "107"] <- "Summer" # June

# BMI = Weight/Height

data_all$BMI <- (data_all$WEIGHT*703)/ (data_all$HEIGHT^2)


data_all$GENDER <- as.character(data_all$GENDER)
data_all$GENDER[data_all$GENDER == "Female"] <- 0
data_all$GENDER[data_all$GENDER == "Male"] <- 1
data_all$GENDER <- as.numeric(data_all$GENDER)

data_all$Sleep_bin <- data_all$SLEEP
data_all$Sleep_bin[data_all$Sleep_bin < 6] <- 0
data_all$Sleep_bin[data_all$Sleep_bin >= 6 & data_all$Sleep_bin < 7.5] <- 1
data_all$Sleep_bin[data_all$Sleep_bin >= 7.5 & data_all$Sleep_bin < 9] <- 2
data_all$Sleep_bin[data_all$Sleep_bin >= 9] <- 3

data_all$StudyLength_bin <- data_all$StudyLength
data_all$StudyLength_bin[data_all$StudyLength_bin < 120] <- 0
data_all$StudyLength_bin[data_all$StudyLength_bin >= 120 & data_all$StudyLength < 180] <- 1
data_all$StudyLength_bin[data_all$StudyLength_bin >= 180 & data_all$StudyLength < 240] <- 2
data_all$StudyLength_bin[data_all$StudyLength_bin >= 240] <- 3

######### MODELING AND VISUALIZATION ##########

# Plot RewP vs. Time for ALL data with points
ggplot(data_all, aes(Time, RewP_Cz)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "All Data with Points")

# Plot RewP vs. Time BY STUDY with points
ggplot(data_all, aes(Time, RewP_Cz, colour = as.factor(Study))) + 
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "By Study with Points")

# Plot RewP vs. Time BY STUDY
ggplot(data_all, aes(Time, RewP_Cz, colour = as.factor(Study))) + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "By Study")

# Plot RewP vs. Time BY Cap
ggplot(data, aes(Time, RewP_Cz, colour = as.factor(Cap))) + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "By Cap")

# Plot RewP vs. Time BY Season
ggplot(data_all, aes(Time, RewP_Cz, colour = as.factor(Season))) + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "By Season")


# LINEAR Model RewP vs. Time, Age, Gender for ALL data
summary(lm(RewP_Cz ~ Time, data = data_all))
summary(lm(RewP_Cz ~ AGE, data = data_all))
summary(lm(RewP_Cz ~ GENDER, data = data_all))
summary(lm(RewP_Cz ~ Time + AGE + GENDER, data = data_all))

# QUADRATIC Model RewP vs. Time, Age, Gender for ALL data
data_all$Time_2 = data_all$Time*data_all$Time
summary(lm(RewP_Cz ~ Time_2 + Time, data = data_all))
summary(lm(RewP_Cz ~ Time_2 + Time + AGE, data = data_all))
summary(lm(RewP_Cz ~ Time_2 + Time + GENDER, data = data_all))
summary(lm(RewP_Cz ~ Time_2 + Time + AGE + GENDER, data = data_all))

# CUBIC Model RewP vs. Time, Age, Gender for ALL data
data_all$Time_3 = data_all$Time*data_all$Time*data_all$Time
summary(lm(RewP_Cz ~ Time_3 + Time_2 + Time, data = data_all))
summary(lm(RewP_Cz ~ Time_3 + Time_2 + Time + AGE, data = data_all))
summary(lm(RewP_Cz ~ Time_3 + Time_2 + Time + GENDER, data = data_all))
summary(lm(RewP_Cz ~ Time_3 + Time_2 + Time + AGE + GENDER, data = data_all))

# LINEAR Model RewP ~ Time for EACH study
# Create a list to store study-specific models
study_models <- list()
# Fit models for each study
for(study in unique(data_all$Study)) {
  study_data <- subset(data_all, Study == study)
  study_models[[study]] <- lm(RewP_Cz ~ Time, data = study_data)
}
# Print summaries for each study
cat("\nStudy-specific RewP ~ Time models:\n")
for(study in names(study_models)) {
  cat("\nStudy", study, ":\n")
  print(summary(study_models[[study]]))
}


# Model Sleep
mod_1 <- lm(RewP_Cz ~ Time + SLEEP, data = data_all)
summary(mod_1)

# Add sleep to the model
model_time_sleep_age_gender <- lm(RewP_Cz ~ Time + SLEEP + AGE + GENDER, data = data_all)
summary(model_time_sleep_age_gender)

# Study-specific models with sleep
for (study in unique(data_all$Study)) {
  study_data <- data_all[data_all$Study == study, ]
  model <- lm(RewP_Cz ~ Time + SLEEP, data = study_data)
  cat("\nStudy", study, ":\n")
  print(summary(model))
}



##### SINUSOIDAL MODELING #####
library(cosinor)

# Create cosinor parameters
T = 24  # 24-hour period
data_all$cos_Time <- cos(2 * pi * data_all$Time / T)
data_all$sin_Time <- sin(2 * pi * data_all$Time / T)

# Fit cosinor model
model_cosinor <- lm(RewP_Cz ~ cos_Time + sin_Time, data = data_all)
summary(model_cosinor)

# Fit cosinor model with sleep
model_cosinor_sleep <- lm(RewP_Cz ~ cos_Time + sin_Time + SLEEP, data = data_all)
summary(model_cosinor_sleep)

# Fit separate models for amplitude and timing effects
# Model 1: Sleep effect on amplitude only
model_amp <- lm(RewP_Cz ~ cos_Time + sin_Time + SLEEP + SLEEP:cos_Time + SLEEP:sin_Time, data = data_all)
summary(model_amp)

# Model 2: Sleep effect on timing only
model_timing <- lm(RewP_Cz ~ cos_Time + sin_Time + SLEEP + SLEEP:cos_Time, data = data_all)
summary(model_timing)

# PLOT COSINOR by calculating cos and sine values
# Function to calculate amplitude from coefficients
calc_amplitude <- function(model, sleep_val) {
  b1 <- coef(model)["cos_Time"] + coef(model)["cos_Time:SLEEP"] * sleep_val
  b2 <- coef(model)["sin_Time"] + coef(model)["sin_Time:SLEEP"] * sleep_val
  return(sqrt(b1^2 + b2^2))
}

# Function to calculate acrophase in hours from coefficients
calc_acrophase <- function(model, sleep_val) {
  b1 <- coef(model)["cos_Time"] + coef(model)["cos_Time:SLEEP"] * sleep_val
  b2 <- coef(model)["sin_Time"] + coef(model)["sin_Time:SLEEP"] * sleep_val
  acrophase_rad <- atan2(b2, b1)
  acrophase_hours <- (acrophase_rad * 24 / (2 * pi)) %% 24
  return(acrophase_hours)
}

# Create sequence of sleep values for plotting
sleep_seq <- seq(min(data_all$SLEEP, na.rm=TRUE), 
                 max(data_all$SLEEP, na.rm=TRUE), 
                 length.out=100)

# Calculate amplitude and acrophase for each sleep value using separate models
results_amp <- data.frame(
  sleep = sleep_seq,
  amplitude = sapply(sleep_seq, function(s) calc_amplitude(model_amp, s))
)

results_timing <- data.frame(
  sleep = sleep_seq,
  acrophase = sapply(sleep_seq, function(s) calc_acrophase(model_timing, s))
)

# Create plots with continuous sleep variable
p1 <- ggplot(results_amp, aes(x = sleep, y = amplitude)) +
  geom_line() +
  geom_ribbon(aes(ymin = amplitude - sd(data_all$RewP_Cz, na.rm=TRUE),
                  ymax = amplitude + sd(data_all$RewP_Cz, na.rm=TRUE)),
              alpha = 0.2) +
  labs(x = "Sleep Duration (hours)", 
       y = "Amplitude (μV)",
       title = "Effect of Sleep Duration on RewP Amplitude") +
  theme_minimal()

p2 <- ggplot(results_timing, aes(x = sleep, y = acrophase)) +
  geom_line() +
  geom_ribbon(aes(ymin = acrophase - sd(data_all$RewP_Cz, na.rm=TRUE),
                  ymax = acrophase + sd(data_all$RewP_Cz, na.rm=TRUE)),
              alpha = 0.2) +
  labs(x = "Sleep Duration (hours)", 
       y = "Acrophase (hours)",
       title = "Effect of Sleep Duration on RewP Timing") +
  theme_minimal()

# Print plots
print(p1)
print(p2)

# Calculate significance of sleep effects separately
# For amplitude: test if sleep moderates the overall rhythm
amp_test <- anova(
  lm(RewP_Cz ~ cos_Time + sin_Time + SLEEP, data = data_all),
  model_amp
)

# For timing: test if sleep moderates the phase
timing_test <- anova(
  lm(RewP_Cz ~ cos_Time + sin_Time + SLEEP, data = data_all),
  model_timing
)

cat("\nP-value for sleep effect on amplitude:", amp_test$`Pr(>F)`[2])
cat("\nP-value for sleep effect on timing:", timing_test$`Pr(>F)`[2])

# Print model summaries for detailed coefficients
cat("\n\nAmplitude model summary:\n")
print(summary(model_amp))

cat("\n\nTiming model summary:\n")
print(summary(model_timing))

# Mixed-Effects Cosinor Analysis using cosinoRmixedeffects -----------------------
library(remotes)
remotes::install_github("maytesuarezfarinas/cosinoRmixedeffects")
library(cosinoRmixedeffects)
library(lmerTest)
library(emmeans)

# Create cosinor parameters for the data
data_all <- create.cosinor.param(time="Time", period=24, data=data_all)

# Fit mixed-effects cosinor model with sleep as predictor
# Using random intercepts for Study to account for study-level variations
f1 <- fit.cosinor.mixed(y = "RewP_Cz", 
                       x = "SLEEP",
                       random = "1|Study", 
                       data = data_all)

# Get estimated means and confidence intervals for different sleep values
db.means <- get.means.ci.cosinor(fit=f1, 
                                contrast.frm="~SLEEP",
                                nsim=500)

# Get pairwise contrasts for sleep effects
db.delta <- get.contrasts.ci.cosinor(fit=f1,
                                    contrast.frm="~SLEEP", 
                                    nsim=500)

# Print results
print(summary(f1))
print(db.means)
print(db.delta)

# Create visualization of the cosinor curves
p <- ggplot.cosinor.lmer(object=f1,
                        x_str="SLEEP",
                        period=24,
                        db.means=db.means,
                        DATA=data_all)

# Customize the plot
p + labs(x="Time (hours)", y="RewP amplitude (μV)") +
    theme_bw() +
    geom_hline(aes(yintercept=MESOR), linetype=2, color="black") +
    geom_segment(aes(x = T_AMP, 
                    y = MESOR+0.05, 
                    xend = T_AMP, 
                    yend = MESOR+Amplitude-0.05),
                arrow = arrow(length = unit(0.15, "cm"),ends="both"), 
                size=0.5, color="gray50",linetype=1)




