require(data.table)
# generate a string to hold the path to the data files
# this variable is used throughout the code - switch if needed
pathw <- 'c:/Users/Ricsi/Documents/!!ResearchBMECOGSCI/ALVAS_LAB/data/'
# we want to mark triggers in the BioTrace outputs according to OpenSesame stimulus presentation times
# we have study and recall sessions, which have OS and BT outputs

# Study sessions

# number of subjects could be calculated by generating a vector from filenames
# chopping the first 2 characters, transforming them to numners and choosing the highest
# i simply use "60" for now
# data of first subject is in 3 files, i leave it for now
for (i in 2:60){
  # Read OS data ####
  if (i < 10){
    path <- paste(pathw, '0', sep = "")
  } else {
    path <- pathw
  }
  # there are two versions of the study phase
  # version A
  if (file.exists(paste(path, as.character(i), '_A_teszt.csv', sep =""))){
    data_OS <- read.csv(paste(path, as.character(i), '_A_teszt.csv', sep =""))
    # some .csv-s have been saved in excel (?)
    # and have ";"-separated values which generate a 1 column data.table
    if (ncol(data_OS) == 1){
      data_OS <- read.csv(paste(path, as.character(i), '_A_teszt.csv', sep =""), sep=";")
    }
  }
  # version B
  if (file.exists(paste(path, as.character(i), '_B_teszt.csv', sep =""))){
    data_OS <- read.csv(paste(path, as.character(i), '_B_teszt.csv', sep =""))
    # it may have ";" as separating characters
    if (ncol(data_OS) == 1){
      data_OS <- read.csv(paste(path, as.character(i), '_B_teszt.csv', sep =""), sep=";")
    }
  }
  # Get OS starting time ####
  # we have the data from OS outputs in "data_OS"
  # we need the time of the start (if there is an output for a given subject)
  if (exists('data_OS') == TRUE){
    # there is a column "datetime" which contains this info in all its rows
    time_OS <- as.character(data_OS$datetime[1])
    # transform it into POSIXct - date handling format of R
    time_OS <- as.POSIXct(substring(time_OS, 10), format = "%H:%M:%S")
  }
  # Read BT data ####
  # sometimes it is in 3 separate files
  # these cases should be prioritized, because the others have an additional processing step
  if (file.exists(paste(path,i,'_A_emot_ECG.txt', sep =""))){
    data_BT <- readLines(paste(path,i,'_A_emot_ECG.txt', sep =""))
    OS_ver <- 'A'
  } else if (file.exists(paste(path,i,'_B_emot_ECG.txt', sep =""))){
    data_BT <- readLines(paste(path,i,'_B_emot_ECG.txt', sep =""))
    OS_ver <- 'B'
  }
  if (exists('data_BT') == TRUE){
    # ECG ####
    # read ECG data for current subject
    data_BT_ECG <- as.data.table(read.table(paste(path, i, '_', OS_ver,'_emot_ECG.txt', sep=""), skip = 12, fill = TRUE))
    # delete last 3 rows as it is a message from BioTrace ("end of exported RAW data")
    data_BT_ECG <- data_BT_ECG[1:(nrow(data_BT_ECG)-3)]
    #"Light Triggers" are inserted as separate rows, this changes "light" to NA
    data_BT_ECG$V1 <- as.numeric(as.character(data_BT_ECG$V1))
    # deleting NAs (light triggers)
    data_BT_ECG <- data_BT_ECG[!(is.na(data_BT_ECG$V1))]
    # first column is not relevant
    data_BT_ECG$V1 <- NULL
    # add a new column with the elapsed time
    data_BT_ECG$trig <- c((1:length(data_BT_ECG$V2))*1/1.024)
    # EMG ####
    # EMG sampling rate is the highest: 2048 Hz
    data_BT_EMG <- as.data.table(read.table(paste(path, i, '_', OS_ver,'_emot_EMG.txt', sep=""), skip = 12, fill = TRUE))
    # delete last 3 rows as it is a message from BioTrace ("end of exported RAW data")
    data_BT_EMG <- data_BT_EMG[1:(nrow(data_BT_EMG)-3)]
    # change "light Trigger" rows to NA
    data_BT_EMG$V1 <- as.numeric(as.character(data_BT_EMG$V1))
    # deleting NAs (light triggers)
    data_BT_EMG <- data_BT_EMG[!(is.na(data_BT_EMG$V1))]
    # first column is not relevant
    data_BT_EMG$V1 <- NULL
    # add a new column with the elapsed time
    data_BT_EMG$trig <- c((1:length(data_BT_EMG$V2))*1/2.048)
    # SCR ####
    data_BT_SCR <- as.data.table(read.table(paste(path, i, '_', OS_ver,'_emot_SCR.txt', sep=""), skip = 12, fill = TRUE))
    # delete last 3 rows as it is a message from BioTrace ("end of exported RAW data")
    data_BT_SCR <- data_BT_SCR[1:(nrow(data_BT_SCR)-3)]
    #"Light Triggers" are inserted as separate rows, this changes "light" to NA
    data_BT_SCR$V1 <- as.numeric(as.character(data_BT_SCR$V1))
    # deleting NAs (light triggers)
    data_BT_SCR <- data_BT_SCR[!(is.na(data_BT_SCR$V1))]
    # first column is not relevant
    data_BT_SCR$V1 <- NULL
    # add a new column with the elapsed time
    data_BT_SCR$trig <- c((1:length(data_BT_SCR$V2))*1/0.032)
    # some outputs are in one file called "subjnr_A/B_emot.txt"
  } else if (file.exists(paste(path,i,'_', OS_ver, '_emot.txt', sep ="")) & exists("data_BT") == FALSE){
    data_BT <- readLines(paste(path,i,'_', OS_ver, '_emot.txt', sep =""))
    data_BTraw <- read.table(paste(path, i, '_', OS_ver, '_emot.txt', sep=""), skip = 12, fill = TRUE)
    # some are in one file called "subjnr_A/B_emot_time.txt"
  } else if (file.exists(paste(path,i,'_', OS_ver, '_emot_time.txt', sep ="")) & exists("data_BT") == FALSE){
    data_BT <- readLines(paste(path,i,'_', OS_ver, '_emot_time.txt', sep =""))
    data_BTraw <- read.table(paste(path, i, '_', OS_ver, '_emot_time.txt', sep=""), skip = 12, fill = TRUE)
  }
  # if there was only one BT file we need a few more steps
  if (exists("data_BTraw") == TRUE){
    if (ncol(data_BTraw) == 4){
      data_BTraw$V1 <- NULL
    }
    # remaining columns are named appropriately
    colnames(data_BTraw) <- c("ecg", "emg", "scr")
    # now we have a 3 column data.table, which contains ECG,EMG and SCR data
    # this should be separated to 3 data.tables each containing a single column
    # convert to data.table
    data_BTraw <- as.data.table(data_BTraw)
    # delete last 2 rows as it is a message from BioTrace ("end of exported RAW data")
    data_BTraw <- data_BTraw[1:(nrow(data_BTraw)-2)]
    # deleting light trigger lines
    data_BTraw <- data_BTraw[data_BTraw$ecg != "Light"]
    # ECG ####
    data_BT_ECG <- as.data.table(data_BTraw[,ecg])
    # sampling rate is 1048, every second row is not a real measurement -> drop them
    data_BT_ECG <- data_BT_ECG[(1:length(data_BT_ECG$V1) %% 2 != 0),]
    # add a column which contains the amount of time (msec) elapsed since start
    data_BT_ECG$trig <- c((1:length(data_BT_ECG$V1))*1/1.024)
    # EMG ####
    data_BT_EMG <- as.data.table(data_BTraw[,emg])
    # add a column which contains the amount of time (msec) elapsed since start
    data_BT_EMG$trig <- c((1:length(data_BT_EMG$V1))*1/2.048)
    # SCR ####
    data_BT_SCR <- as.data.table(data_BTraw[,scr])
    # sampling rate is 32 Hz, only every 64th row is a real measurement
    data_BT_SCR <- data_BT_SCR[(1:length(data_BT_SCR$V1) %% 64 == 0),]
    # add a column which contains the amount of time (msec) elapsed since start
    data_BT_SCR$trig <- c((1:length(data_BT_SCR$V1))*1/0.032)
  }
  # Get BT starting time & calculate time difference & estimated trigger times ####
  # if there is a file to get it from
  if (exists('data_BT') == TRUE){
    time_BT <- gsub("Time:\t","",data_BT[7])
    time_BT <- as.POSIXct(time_BT, format = "%H:%M:%S")
    # calculate time difference ####
    tDiff <- as.numeric(time_BT - time_OS) # = x !PERC!
    tDiff <- tDiff * 60 * 1000 # különbség msec-ben
    # calculate vector for estimated trigger times in msec ####
    # stimulus presentation times from OS
    OS_stim_prestimes <- c(data_OS$time_f_picture[1:3],data_OS$time_picture[4:43],data_OS$time_f_picture[44:46])
    # calculate the stim. pres. times for the BioTrace data
    NEXUS_estimated_trig <- OS_stim_prestimes - tDiff
    # negative values generated in some case
    # BioTrace was started while picture presentation was already running
    # OR the time on the two computers is not synced
    # either way, these cannot be marked on the BioTrace output
    # remove them
    NEXUS_estimated_trig <- NEXUS_estimated_trig[NEXUS_estimated_trig > 0]
  }
  # mark calculated trigger times in ECG data and write output ####
  if (exists('data_BT') == TRUE){
    data_BT_ECG$est_trigger <- NA
    for (m in 1:length(NEXUS_estimated_trig)){
      data_BT_ECG$est_trigger[which(abs(data_BT_ECG$trig - NEXUS_estimated_trig[m]) == min(abs(data_BT_ECG$trig - NEXUS_estimated_trig[m])))] <- "+"
    }
    # delete $trig column, as it is not needed anymore
    data_BT_ECG$trig <- NULL
    # generate output .csv
    write.table(data_BT_ECG, file = paste(i,"_",OS_ver,"_emot_ECG_esttrig.csv", sep = ""), row.names=FALSE, col.names=FALSE, sep = ",")
    
    # mark calculated trigger times in EMG data and write output####
    data_BT_EMG$est_trigger <- NA
    for (m in 1:length(NEXUS_estimated_trig)){
      data_BT_EMG$est_trigger[which(abs(data_BT_EMG$trig - NEXUS_estimated_trig[m]) == min(abs(data_BT_EMG$trig - NEXUS_estimated_trig[m])))] <- "+"
    }
    # delete $trig column, as it is not needed anymore
    data_BT_EMG$trig <- NULL
    # generate output .csv
    write.table(data_BT_EMG, file = paste(i,"_",OS_ver,"_emot_EMG_esttrig.csv", sep = ""), row.names=FALSE, col.names=FALSE, sep = ",")
    
    # mark calculated trigger times in SCR data and write output####
    data_BT_SCR$est_trigger <- NA
    for (m in 1:length(NEXUS_estimated_trig)){
      data_BT_SCR$est_trigger[which(abs(data_BT_SCR$trig - NEXUS_estimated_trig[m]) == min(abs(data_BT_SCR$trig - NEXUS_estimated_trig[m])))] <- "+"
    }
    # delete $trig column, as it is not needed anymore
    data_BT_SCR$trig <- NULL
    # generate output .csv
    write.table(data_BT_SCR, file = paste(i,"_",OS_ver,"_emot_SCR_esttrig.csv", sep = ""), row.names=FALSE, col.names=FALSE, sep = ",")
  }
  # remove all variables to not interfere with the next cycle ####
  remove('data_BT', 'data_OS', 'tDiff', 'NEXUS_estimated_trig', 'OS_stim_prestimes', 'm', 'data_BTraw', 'data_BT_ECG', 'data_BT_EMG', 'data_BT_SCR')
}

# Recall sessions

# number of subjects could be calculated by generating a vector from filenames
# chopping the first 2 characters, transforming them to numners and choosing the highest
# i simply use "60" for now
for (i in 1:60){
  # Read OS data ####
  if (i < 10){
    path <- paste(pathw, '0', sep = "")
  } else {
    path <- pathw
  }
  if (file.exists(paste(path, as.character(i), '_recall_teszt.csv', sep =""))){
    data_OS <- read.csv(paste(path, as.character(i), '_recall_teszt.csv', sep =""))
    # some .csv-s have been saved in excel (?)
    # and have ";"-separated values which generate a 1 column data.table
    if (ncol(data_OS) == 1){
      data_OS <- read.csv(paste(path, as.character(i), '_recall_teszt.csv', sep =""), sep=";")
    }
  }
  # in one instance OS had to be restarted, thus the output is in 2 files
  if (file.exists(paste(path, as.character(i), '_recall_teszt_2.csv', sep =""))){
    data_OS_2 <- read.csv(paste(path, as.character(i), '_recall_teszt_2.csv', sep =""))
    # it may have ";" as separating characters
    if (ncol(data_OS_2) == 1){
      data_OS_2 <- read.csv(paste(path, as.character(i), '_recall_teszt_2.csv', sep =""), sep=";")
    }
    data_OS <- rbind(data_OS, data_OS_2)
  }
  # Get OS starting time ####
  # we have the data from OS outputs in "data_OS"
  # we need the time of the start (if there is an output for a given subject)
  if (exists('data_OS') == TRUE){
    # there is a column "datetime" which contains this info in all its rows
    time_OS <- as.character(data_OS$datetime[1])
    # transform it into POSIXct - date handling format of R
    time_OS <- as.POSIXct(substring(time_OS, 10), format = "%H:%M:%S")
  }
  # Read BT data ####
  # sometimes it is in 3 separate files
  # these cases should be prioritized, because the others have an additional processing step
  if (file.exists(paste(path,i,'_recall_ECG.txt', sep =""))){
    data_BT <- readLines(paste(path,i,'_recall_ECG.txt', sep =""))
    # ECG ####
    # read ECG data for current subject
    data_BT_ECG <- as.data.table(read.table(paste(path, i, '_recall_ECG.txt', sep=""), skip = 12, fill = TRUE))
    # delete last 3 rows as it is a message from BioTrace ("end of exported RAW data")
    data_BT_ECG <- data_BT_ECG[1:(nrow(data_BT_ECG)-3)]
    #"Light Triggers" are inserted as separate rows, this changes "light" to NA
    data_BT_ECG$V1 <- as.numeric(as.character(data_BT_ECG$V1))
    # deleting NAs (light triggers)
    data_BT_ECG <- data_BT_ECG[!(is.na(data_BT_ECG$V1))]
    # first column is not relevant
    data_BT_ECG$V1 <- NULL
    # add a new column with the elapsed time
    data_BT_ECG$trig <- c((1:length(data_BT_ECG$V2))*1/1.024)
    # EMG ####
    # EMG sampling rate is the highest: 2048 Hz
    data_BT_EMG <- as.data.table(read.table(paste(path, i, '_recall_EMG.txt', sep=""), skip = 12, fill = TRUE))
    # delete last 3 rows as it is a message from BioTrace ("end of exported RAW data")
    data_BT_EMG <- data_BT_EMG[1:(nrow(data_BT_EMG)-3)]
    # change "light Trigger" rows to NA
    data_BT_EMG$V1 <- as.numeric(as.character(data_BT_EMG$V1))
    # deleting NAs (light triggers)
    data_BT_EMG <- data_BT_EMG[!(is.na(data_BT_EMG$V1))]
    # first column is not relevant
    data_BT_EMG$V1 <- NULL
    # add a new column with the elapsed time
    data_BT_EMG$trig <- c((1:length(data_BT_EMG$V2))*1/2.048)
    # SCR ####
    data_BT_SCR <- as.data.table(read.table(paste(path, i, '_recall_SCR.txt', sep=""), skip = 12, fill = TRUE))
    # delete last 3 rows as it is a message from BioTrace ("end of exported RAW data")
    data_BT_SCR <- data_BT_SCR[1:(nrow(data_BT_SCR)-3)]
    #"Light Triggers" are inserted as separate rows, this changes "light" to NA
    data_BT_SCR$V1 <- as.numeric(as.character(data_BT_SCR$V1))
    # deleting NAs (light triggers)
    data_BT_SCR <- data_BT_SCR[!(is.na(data_BT_SCR$V1))]
    # first column is not relevant
    data_BT_SCR$V1 <- NULL
    # add a new column with the elapsed time
    data_BT_SCR$trig <- c((1:length(data_BT_SCR$V2))*1/0.032)
  } else if (file.exists(paste(path,i,'_recall_teszt.txt', sep ="")) & exists("data_BT") == FALSE){
    data_BT <- readLines(paste(path,i,'_recall_teszt.txt', sep =""))
    data_BTraw <- read.table(paste(path, i, '_recall_teszt.txt', sep=""), skip = 12, fill = TRUE)
    # some are "subjectnr_recall.txt"
  } else if (file.exists(paste(path,i,'_recall.txt', sep ="")) & exists("data_BT") == FALSE){
    data_BT <- readLines(paste(path,i,'_recall.txt', sep =""))
    data_BTraw <- read.table(paste(path, i, '_recall.txt', sep=""), skip = 12, fill = TRUE)
    # some are named: "subjectnr_recall_time.txt"
  } else if (file.exists(paste(path,i,'_recall_time.txt', sep ="")) & exists("data_BT") == FALSE){
    data_BT <- readLines(paste(path,i,'_recall_time.txt', sep =""))
    data_BTraw <- read.table(paste(path, i, '_recall_time.txt', sep=""), skip = 12, fill = TRUE)
  }
  # if there was only one BT file we need a few more steps
  if (exists("data_BTraw") == TRUE){
    if (ncol(data_BTraw) == 4){
      data_BTraw$V1 <- NULL
    }
    # remaining columns are named appropriately
    colnames(data_BTraw) <- c("ecg", "emg", "scr")
    # now we have a 3 column data.table, which contains ECG,EMG and SCR data
    # this should be separated to 3 data.tables each containing a single column
    # convert to data.table
    data_BTraw <- as.data.table(data_BTraw)
    # delete last 2 rows as it is a message from BioTrace ("end of exported RAW data")
    data_BTraw <- data_BTraw[1:(nrow(data_BTraw)-2)]
    # deleting light trigger lines
    data_BTraw <- data_BTraw[data_BTraw$ecg != "Light"]
    # ECG ####
    data_BT_ECG <- as.data.table(data_BTraw[,ecg])
    # sampling rate is 1048, every second row is not a real measurement -> drop them
    data_BT_ECG <- data_BT_ECG[(1:length(data_BT_ECG$V1) %% 2 != 0),]
    # add a column which contains the amount of time (msec) elapsed since start
    data_BT_ECG$trig <- c((1:length(data_BT_ECG$V1))*1/1.024)
    # EMG ####
    data_BT_EMG <- as.data.table(data_BTraw[,emg])
    # add a column which contains the amount of time (msec) elapsed since start
    data_BT_EMG$trig <- c((1:length(data_BT_EMG$V1))*1/2.048)
    # SCR ####
    data_BT_SCR <- as.data.table(data_BTraw[,scr])
    # sampling rate is 32 Hz, only every 64th row is a real measurement
    data_BT_SCR <- data_BT_SCR[(1:length(data_BT_SCR$V1) %% 64 == 0),]
    # add a column which contains the amount of time (msec) elapsed since start
    data_BT_SCR$trig <- c((1:length(data_BT_SCR$V1))*1/0.032)
  }
  # Get BT starting time & calculate time difference & estimated trigger times ####
  # if there is a file to get it from
  if (exists('data_BT') == TRUE){
    time_BT <- gsub("Time:\t","",data_BT[7])
    time_BT <- as.POSIXct(time_BT, format = "%H:%M:%S")
    # calculate time difference ####
    tDiff <- as.numeric(time_BT - time_OS) # = x !PERC!
    tDiff <- tDiff * 60 * 1000 # különbség msec-ben
    # calculate vector for estimated trigger times in msec ####
    # stimulus presentation times from OS
    OS_stim_prestimes <- c(data_OS$time_picture_f[1:3],data_OS$time_picture[4:43],data_OS$time_picture_f[44:46])
    # calculate the stim. pres. times for the BioTrace data
    NEXUS_estimated_trig <- OS_stim_prestimes - tDiff
    # negative values generated in some case
    # BioTrace was started while picture presentation was already running
    # OR the time on the two computers is not synced
    # either way, these cannot be marked on the BioTrace output
    # remove them
    NEXUS_estimated_trig <- NEXUS_estimated_trig[NEXUS_estimated_trig > 0]
  }
  # mark calculated trigger times in ECG data and write output ####
  if (exists('data_BT') == TRUE){
    data_BT_ECG$est_trigger <- NA
    for (m in 1:length(NEXUS_estimated_trig)){
      data_BT_ECG$est_trigger[which(abs(data_BT_ECG$trig - NEXUS_estimated_trig[m]) == min(abs(data_BT_ECG$trig - NEXUS_estimated_trig[m])))] <- "+"
    }
    # delete $trig column, as it is not needed anymore
    data_BT_ECG$trig <- NULL
    # generate output .csv
    write.table(data_BT_ECG, file = paste(i,"_recall_ECG_esttrig.csv", sep = ""), row.names=FALSE, col.names=FALSE, sep = ",")
    
    # mark calculated trigger times in EMG data and write output####
    data_BT_EMG$est_trigger <- NA
    for (m in 1:length(NEXUS_estimated_trig)){
      data_BT_EMG$est_trigger[which(abs(data_BT_EMG$trig - NEXUS_estimated_trig[m]) == min(abs(data_BT_EMG$trig - NEXUS_estimated_trig[m])))] <- "+"
    }
    # delete $trig column, as it is not needed anymore
    data_BT_EMG$trig <- NULL
    # generate output .csv
    write.table(data_BT_EMG, file = paste(i,"_recall_EMG_esttrig.csv", sep = ""), row.names=FALSE, col.names=FALSE, sep = ",")
    
    # mark calculated trigger times in SCR data and write output####
    data_BT_SCR$est_trigger <- NA
    for (m in 1:length(NEXUS_estimated_trig)){
      data_BT_SCR$est_trigger[which(abs(data_BT_SCR$trig - NEXUS_estimated_trig[m]) == min(abs(data_BT_SCR$trig - NEXUS_estimated_trig[m])))] <- "+"
    }
    # delete $trig column, as it is not needed anymore
    data_BT_SCR$trig <- NULL
    # generate output .csv
    write.table(data_BT_SCR, file = paste(i,"_recall_SCR_esttrig.csv", sep = ""), row.names=FALSE, col.names=FALSE, sep = ",")
  }
  # remove all variables to not interfere with the next cycle ####
  remove('data_BT', 'data_OS', 'tDiff', 'NEXUS_estimated_trig', 'OS_stim_prestimes', 'm', 'data_BTraw', 'data_BT_ECG', 'data_BT_EMG', 'data_BT_SCR')
}