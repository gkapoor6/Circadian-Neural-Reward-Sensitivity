library(psych)
library(tidyverse)
library(dplyr)


# DONE! 
# 1. RewP, CzWins, CzLosses merged with Time of Day for All Studies 86-107 
# 2. Modeled RewP ~ ToD, Cz_Wins ~ ToD, Cz_Losses ~ ToD

# TODO
# 1. [In progress] Get Psychopathology + Demographics data for All Studies 86-107
# 2. More modeling

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
study_names <- c("data1_86", "data1_94", "data1_96", "data1_100", "data1_102", "data1_104", "data1_107")

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
colnames(data1_86)[1] <- "subname"
# TODO KEANAN CHECK (?????) Subject 1020 has two rows, remove the first row (?????)
data1_86 <- data1_86[!(data1_86$subname == "1020_Doors"), ]
data1_86[[1]] <- gsub("_Doors", "", data1_86[[1]])
data1_86 <- dplyr::select(data1_86, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(data1_86) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# STUDY 94
colnames(data1_94)[1] <- "subname"
data1_94[[1]] <- gsub("_Doors", "", data1_94[[1]])
data1_94 <- dplyr::select(data1_94, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(data1_94) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# STUDY 96
colnames(data1_96)[1] <- "subname"
data1_96[[1]] <- gsub("_doors_", "_", data1_96[[1]])
data1_96[[1]] <- gsub("_doors", "", data1_96[[1]])
data1_96 <- dplyr::select(data1_96, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(data1_96) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# STUDY 100
colnames(data1_100)[1] <- "subname"
data1_100[[1]] <- gsub("_doors", "", data1_100[[1]])
data1_100 <- dplyr::select(data1_100, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(data1_100) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")


# STUDY 102
colnames(data1_102)[1] <- "subname"
data1_102[[1]] <- gsub("_doors", "", data1_102[[1]])
data1_102[[1]] <- gsub("_Doors", "", data1_102[[1]])
data1_102 <- dplyr::select(data1_102, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(data1_102) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# STUDY 104
colnames(data1_104)[1] <- "subname"
data1_104[[1]] <- gsub("_Doors_convert.cdt", "", data1_104[[1]])
data1_104 <- dplyr::select(data1_104, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(data1_104) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# STUDY 107
colnames(data1_107)[1] <- "subname"
data1_107[[1]] <- gsub("_Doors.cdt", "", data1_107[[1]])
data1_107[[1]] <- gsub("_Oddball2_convert.cdt", "", data1_107[[1]])
data1_107[[1]] <- gsub("_convert.cdt", "", data1_107[[1]])
data1_107[[1]] <- gsub("_Doors", "", data1_107[[1]])
data1_107[[1]] <- gsub("_Doors_Oddball2_convert.cdt", "", data1_107[[1]])
data1_107$subname[data1_107$subname == "st0107_2099"] <- "st0107_s2099"
data1_107$subname[data1_107$subname == "st0107_2101"] <- "st0107_s2101"
data1_107 <- dplyr::select(data1_107, subname, `Cz-Wins`, `Cz-Losses`, RewPCz)
colnames(data1_107) <- c("subname","Cz_Wins","Cz_Losses","RewP_Cz")

# ERP and Log Data Management ---------------------------------------------------------

## OLD CODE ##
# Calculate RewP for Study 96
# data1_94$RewP_FCz <- data1_94$RewPmFCzG - data1_94$RewPmFCzL
# Gain, Loss, RewP, Age
# data1_86 <- dplyr::select(data1_86, subname,FCz_Gain_w3,FCz_Loss_w3,rewp_mean_FCz_w3, AGE)
# data1_94 <- dplyr::select(data1_94, subname,RewPmFCzG,RewPmFCzL,RewP_FCz, AGE)
# colnames(data1_86) <- c("subname","FRN_Gain_FCz","FRN_Loss_FCz","RewP_FCz","Age")
# colnames(data1_94) <- c("subname","FRN_Gain_FCz","FRN_Loss_FCz","RewP_FCz","Age")


# Start and End Times for Doors Task (Studies 86 - 107)
log_86 <- dplyr::select(log_86, subname_log, doors_start, doors_end)
log_94 <- dplyr::select(log_94, subname_log, doors_start, doors_end)
log_96 <- dplyr::select(log_96, subname_log, doors_start, doors_end)
log_100 <- dplyr::select(log_100, subname_log, doors_start, doors_end)
log_102 <- dplyr::select(log_102, subname_log, doors_start, doors_end)
log_104 <- dplyr::select(log_104, subname_log, doors_start, doors_end)
log_107 <- dplyr::select(log_107, subname_log, doors_start, doors_end)

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

data1_86 <- merge(data1_86, log_86, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
data1_94 <- merge(data1_94, log_94, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
data1_96 <- merge(data1_96, log_96, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
data1_100 <- merge(data1_100, log_100, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
data1_102 <- merge(data1_102, log_102, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
data1_104 <- merge(data1_104, log_104, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")
data1_107 <- merge(data1_107, log_107, all.x = T, all.y = F, by.x = "subname", by.y = "subname_log")


#### Merge ALL STUDIES ERP + LOG DATA
data <- rbind(data1_86, data1_94, data1_100, data1_102, data1_104, data1_107)


# Clean up time variable  ---------------------------------------------------------
str(data$doors_start)
data$doors_start <- format(as.POSIXct(data$doors_start,format='%I:%M %p'),format="%H:%M:%S")

data <- data[complete.cases(data),]

data$RefDate <- "10/19/2024"
data$RefTime <- "00:00:00"

data$Ref <- as.POSIXct(paste(data$RefDate, data$RefTime), format="%m/%d/%Y %H:%M:%S")
data$doors_start2 <- as.POSIXct(paste(data$RefDate, data$doors_start), format="%m/%d/%Y %H:%M:%S")
data$Time <- as.numeric(data$doors_start2 - data$Ref)

ggplot(data, aes(Time, RewP_Cz)) + geom_smooth() + theme_bw()


## MODELLING RewP ~ Time of Day ---------------------------------------------------------
# Linear
data$Time <- data$Time - mean(data$Time)
summary(lm(RewP_Cz ~ Time, data))
summary(lm(Cz_Wins ~ Time, data))
summary(lm(Cz_Losses ~ Time, data))

# Quadratic
data$Time_2 <- data$Time*data$Time
summary(lm(RewP_Cz ~ Time + Time_2, data))
summary(lm(Cz_Wins ~ Time + Time_2, data))
summary(lm(Cz_Losses ~ Time + Time_2, data))

# Cubic
data$Time_3 <- data$Time*data$Time*data$Time
summary(lm(RewP_Cz ~ Time + Time_2 + Time_3, data))
summary(lm(Cz_Wins ~ Time + Time_2 + Time_3, data))
summary(lm(Cz_Losses ~ Time + Time_2 + Time_3, data))




# DATA 2 READ IN Psych & Demographic  measures (data2) -----------------
data2_86 <- foreign::read.spss("st0086_RewP_sx.sav", to.data.frame = T)
data2_94 <- foreign::read.spss("st0094_RewP_sx.sav", to.data.frame = T)
data2_96 <- foreign::read.spss("st0096_Questionnaires_Merged_ALLmerged.sav", to.data.frame = T)

# STUDY 100
data2_100_scores <- foreign::read.spss("st0100_merged_questionnaire_scores.sav", to.data.frame = T)
# ****Raw has Age and Gender
data2_100_raw <- foreign::read.spss("st0100_merged_questionnaires_raw.sav", to.data.frame = T)

# STUDY 102
data2_102_raw <- foreign::read.spss("st0102_merged_questionnaire_raw.sav", to.data.frame = T)
# ****Scored has Age and Gender (and subjectnames )
data2_102_scores <- foreign::read.spss("st0102_questionnaires_scored_p1001tos1092.sav", to.data.frame = T)

# TODO STUDY 104 ----- PENDING, THERE ARE 14 FILES SO WILL CHECK WHICH ONES HAVE THE VARS WE NEED


# TODO STUDY 107 ----- NO SCORED FILE AVAILABLE
##### TODO MUST FIX --- HAS 898 ENTRIES
#### SUBJECT NAME COLUMN IS data2_107_raw$SUBNAME
data2_107_raw <- foreign::read.spss("st0107_SurveyV1-V2-V3-V3IP-IPadditions_2021-12-3_merge_w-ages-subnames.sav", to.data.frame = T)
data2_107_subnames <- data2_107_raw$SUBNAME


# DATA 3 -- Get Psychopathology & Demographics from DATA 2 ---------------------------------------------------------

# subname
# AGE
# GENDER
# SLEEP
# HEIGHT & WEIGHT FOR BMI
# CAFFEINE
# RACE
# IDAS VARIABLES (12)


### STUDY 86
data3_86 <- dplyr::select(data2_86, subname, AGE, GENDER)

### STUDY 94
data3_94 <- dplyr::select(data2_94, subname, AGE, GENDER, SLEEP,
                          HEIGHT, WEIGHT,
                          CAFFEINE, RACE,
                          gd_tot, dys_tot, las_tot, ins_tot,
                          sui_tot, al_tot, ag_tot, tem_tot,
                          wb_tot, sa_tot, pan_tot, ti_tot)

### STUDY 96
# TODO MISSING AGE, GENDER, SLEEP, HEIGHT, WEIGHT, CAFFEINE, RACE
data3_96 <- dplyr::select(data2_96, subname,
                          gd_tot, dys_tot, las_tot, ins_tot,
                          sui_tot, al_tot, ag_tot, tem_tot,
                          wb_tot, sa_tot, pan_tot, ti_tot)


### STUDY 100
# TODO Score SLEEP 
# TODO Score CAFFEINE
part1_100 <- dplyr::select(data2_100_raw, SubjectID, Age, Gender,
                               Height, Weight, 
                                Race, Caffeine,
                                Sleep.0)

# Replace "p" with "s" in data2_100_scores
data2_100_scores[[1]] <- gsub("p", "s", data2_100_scores[[1]])

part2_100 <- dplyr::select(data2_100_scores, SubjectID, 
                                gd_tot, dys_tot, las_tot, ins_tot,
                                sui_tot, al_tot, ag_tot, tem_tot,
                                wb_tot, sa_tot, pan_tot, ti_tot)

data3_100 <- merge(part1_100, part2_100, all.x = T, all.y = F, by.x = "SubjectID", by.y = "SubjectID")
 

# TODO STUDY 104 ----- PENDING, THERE ARE 14 FILES SO WILL CHECK WHICH ONES HAVE THE VARS WE NEED


# TODO STUDY 107 ----- NO SCORED FILE AVAILABLE
##### TODO MUST FIX --- HAS 898 ENTRIES


# st86 only ---------------------------------------------------------------

# str(data1_86$doors_start)
# data1_86$doors_start <- format(as.POSIXct(data1_86$doors_start,format='%I:%M %p'),format="%H:%M:%S")
# 
# data1_86 <- data1_86[complete.cases(data1_86),]
# 
# data1_86$RefDate <- "4/18/2024"
# data1_86$RefTime <- "00:00:00"
# 
# data1_86$Ref <- as.POSIXct(paste(data1_86$RefDate, data1_86$RefTime), format="%m/%d/%Y %H:%M:%S")
# data1_86$doors_start2 <- as.POSIXct(paste(data1_86$RefDate, data1_86$doors_start), format="%m/%d/%Y %H:%M:%S")
# data1_86$Time <- as.numeric(data1_86$doors_start2 - data1_86$Ref)
# data1_86$test <- data1_86$doors_start2 - data1_86$Ref
# 
# 
# # PLOT
# ggplot(data1_86, aes(x = test, y = RewP_FCz)) +
#   geom_smooth(color = "#003262") +
#   ggtitle("Diurnal Variation in Reward Positivity (RewP) Amplitude") +
#   theme(plot.title = element_text(hjust = 0.5)) + 
#   labs(
#     x = "Hours (Military Time)",
#     y = "RewP Amplitude (μV)"
#   ) +
#   theme_bw() +
#   theme(
#     plot.title = element_text(color = "#003262", size = 20, face = "bold", hjust = 0.5),
#     legend.text = element_text(color = "black"),
#     legend.title = element_text(color = "#003262", face = "bold"),
#     axis.text = element_text(size = 18),
#     axis.title = element_text(size = 18)
#   )
# 
# # Create time buckets: Morning, Midday, and Late Afternoon
# ## RUN THIS CODE BEFORE CHANGING data1_86$TIME (done during linear modelling)
# data1_86$TimeCat <- NA
# data1_86$TimeCat[data1_86$Time < 13] <- "Morning"
# data1_86$TimeCat[data1_86$Time >= 13 & data1_86$Time < 15.5] <- "Midday"
# data1_86$TimeCat[data1_86$Time >= 15.5] <- "LateAfternoon"
# 
# 
# data1_86 <- merge(data1_86, data2_86[c("subname","Dysthymia_Sx_avg","WorstMDE_Avg",
#                                        "depd_prop","ZCurrentMDE_avg","Zbdi",
#                                        "Zid5p_anhedo","id5p_depres","AUD_SUD_tot",
#                                        "GENDER")], by = "subname")
# 
# # Linear
# data1_86$Time <- data1_86$Time - mean(data1_86$Time, na.rm = T)
# 
# summary(lm(RewP_FCz ~ Time, data1_86))
# summary(lm(FRN_Gain_FCz ~ Time, data1_86))
# summary(lm(FRN_Loss_FCz ~ Time, data1_86))
# lm.beta::lm.beta(lm(RewP_FCz ~ Time, data1_86))
# 
# summary(lm(RewP_FCz ~ scale(Time)*scale(Age), data1_86))
# lm.beta::lm.beta(lm(RewP_FCz ~ scale(Time)*scale(Age), data1_86))
# 
# summary(lm(FRN_Gain_FCz ~ Time*Age, data1_86))
# summary(lm(FRN_Loss_FCz ~ Time*Age, data1_86))
# 
# # Quadratic
# data1_86$Time_2 <- data1_86$Time*data1_86$Time
# 
# summary(lm(RewP_FCz ~ Time + Time_2, data1_86))
# lm.beta::lm.beta(lm(RewP_FCz ~ Time + Time_2, data1_86))
# 
# summary(lm(FRN_Gain_FCz ~ Time + Time_2, data1_86))
# summary(lm(FRN_Loss_FCz ~ Time + Time_2, data1_86))
# 
# summary(lm(RewP_FCz ~ scale(Time) + scale(Time_2)*scale(Age), data1_86))
# lm.beta::lm.beta(lm(RewP_FCz ~ scale(Time) + scale(Time_2)*scale(Age), data1_86))
# 
# summary(lm(FRN_Gain_FCz ~ Time*Age + Time_2*Age, data1_86))
# summary(lm(FRN_Loss_FCz ~ Time*Age + Time_2*Age, data1_86))
# 
# # Cubic
# data1_86$Time_3 <- data1_86$Time*data1_86$Time*data1_86$Time
# 
# summary(lm(RewP_FCz ~ Time + Time_2 + Time_3, data1_86))
# lm.beta::lm.beta(lm(RewP_FCz ~ Time + Time_2 + Time_3, data1_86))
# 
# summary(lm(RewP_FCz ~ scale(Time) + scale(Time_2) + scale(Time_3)*scale(Age), data1_86))
# lm.beta::lm.beta(lm(RewP_FCz ~ scale(Time) + scale(Time_2) + scale(Time_3)*scale(Age), data1_86))
# 
# summary(lm(FRN_Gain_FCz ~ Time + Time_2 + Time_3, data1_86))
# summary(lm(FRN_Loss_FCz ~ Time + Time_2 + Time_3, data1_86))
# 
# summary(lm(RewP_FCz ~ Time*Age + Time_2*Age + Time_3*Age, data1_86))
# summary(lm(FRN_Gain_FCz ~ Time*Age + Time_2*Age + Time_3*Age, data1_86))
# summary(lm(FRN_Loss_FCz ~ Time*Age + Time_2*Age + Time_3*Age, data1_86))
# 
# 
# 
# 
# 
# # Time Interactions st86 --------------------------------------------------
# 
# 
# ## Control for gender
# summary(lm(WorstMDE_Avg ~ RewP_FCz*TimeCat + GENDER, data1_86))
# lm.beta::lm.beta(lm(WorstMDE_Avg ~ RewP_FCz*TimeCat + GENDER, data1_86))
# 
# #summary(pscl::zeroinfl(WorstMDE_Avg*10 ~ RewP_FCz*TimeCat, test, dist = "negbin"))
# #summary(pscl::zeroinfl(Dysthymia_Sx_avg*8 ~ RewP_FCz*TimeCat, test, dist = "negbin"))
# 
# #summary(lm(WorstMDE_Avg ~ RewP_FCz*TimeCat, test))
# #summary(lm(GENDER ~ RewP_FCz*TimeCat, test))
# 
# 
# # PLOT
# ggplot(data1_86, aes(RewP_FCz, WorstMDE_Avg, color = TimeCat)) +
#   geom_smooth(method = "lm") +
#   ggtitle("Severity of Worst MDE vs. RewP Amplitude Across the Day") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   labs(
#     x = "RewP Amplitude (μV)",
#     y = "Severity of Worst MDE",
#     color = "Time of Day"
#   ) +
#   theme_bw() +
#   theme(
#     plot.title = element_text(color = "#003262", size = 20, face = "bold", hjust = 0.5),
#     legend.text = element_text(color = "black", size = 18),
#     legend.title = element_text(color = "#003262", face = "bold", size = 18),
#     axis.text = element_text(size = 20),
#     axis.title = element_text(size = 20)
#   )
# 
# 
# # st94 only ---------------------------------------------------------------
# 
# str(data1_94$doors_start)
# data1_94$doors_start <- format(as.POSIXct(data1_94$doors_start,format='%I:%M %p'),format="%H:%M:%S")
# 
# data1_94 <- data1_94[complete.cases(data1_94),]
# 
# data1_94$RefDate <- "4/5/2024"
# data1_94$RefTime <- "00:00:00"
# 
# data1_94$Ref <- as.POSIXct(paste(data1_94$RefDate, data1_94$RefTime), format="%d/%m/%Y %H:%M:%S")
# data1_94$doors_start2 <- as.POSIXct(paste(data1_94$RefDate, data1_94$doors_start), format="%d/%m/%Y %H:%M:%S")
# data1_94$Time <- as.numeric(data1_94$doors_start2 - data1_94$Ref)
# 
# # Create time buckets: Morning, Midday, and Late Afternoon
# ## RUN THIS CODE BEFORE CHANGING data1_94$TIME (done during linear modelling)
# data1_94$TimeCat <- NA
# data1_94$TimeCat[data1_94$Time < 13] <- "Morning"
# data1_94$TimeCat[data1_94$Time >= 13 & data1_94$Time < 15.5] <- "Midday"
# data1_94$TimeCat[data1_94$Time >= 15.5] <- "LateAfternoon"
# 
# # Linear
# data1_94$Time <- data1_94$Time - mean(data1_94$Time, na.rm = T)
# 
# summary(lm(RewP_FCz ~ Time, data1_94))
# summary(lm(FRN_Gain_FCz ~ Time, data1_94))
# summary(lm(FRN_Loss_FCz ~ Time, data1_94))
# 
# summary(lm(RewP_FCz ~ Time*Age, data1_94))
# summary(lm(FRN_Gain_FCz ~ Time*Age, data1_94))
# summary(lm(FRN_Loss_FCz ~ Time*Age, data1_94))
# 
# # Quadratic
# data1_94$Time_2 <- data1_94$Time*data1_94$Time
# 
# summary(lm(RewP_FCz ~ Time + Time_2, data1_94))
# summary(lm(FRN_Gain_FCz ~ Time + Time_2, data1_94))
# summary(lm(FRN_Loss_FCz ~ Time + Time_2, data1_94))
# 
# summary(lm(RewP_FCz ~ Time*Age + Time_2*Age, data1_94))
# summary(lm(FRN_Gain_FCz ~ Time*Age + Time_2*Age, data1_94))
# summary(lm(FRN_Loss_FCz ~ Time*Age + Time_2*Age, data1_94))
# 
# # Cubic
# data1_94$Time_3 <- data1_94$Time*data1_94$Time*data1_94$Time
# 
# summary(lm(RewP_FCz ~ Time + Time_2 + Time_3, data1_94))
# summary(lm(FRN_Gain_FCz ~ Time + Time_2 + Time_3, data1_94))
# summary(lm(FRN_Loss_FCz ~ Time + Time_2 + Time_3, data1_94))
# 
# summary(lm(RewP_FCz ~ Time*Age + Time_2*Age + Time_3*Age, data1_94))
# summary(lm(FRN_Gain_FCz ~ Time*Age + Time_2*Age + Time_3*Age, data1_94))
# summary(lm(FRN_Loss_FCz ~ Time*Age + Time_2*Age + Time_3*Age, data1_94))
# 
# 
# data1_94 <- merge(data1_94, data2_94[c("subname","id5p_anhedo","id5p_depres",
#                               "id5p_emolab","mpq_PEt","mpq_wbt",
#                               "gd_tot","dys_tot","BSS_tot")], by = "subname")
# 
# summary(lm(BSS_tot ~ RewP_FCz*TimeCat, data1_94))
# summary(lm(id5p_depres ~ RewP_FCz*TimeCat, data1_94))
# 
# ### WHAT IS THE VARIABLE FOR WORSTMDE_AVG?
# ggplot(data1_94, aes(RewP_FCz, WorstMDE_Avg, color = TimeCat)) + geom_smooth(method = "lm") + theme_bw()
