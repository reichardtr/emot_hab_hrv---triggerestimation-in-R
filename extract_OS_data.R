# we need the relevant data from OS in a separate file
require(data.table)
require(xlsx)
# this the location of the data
pathw <- 'c:/Users/Ricsi/Documents/!!ResearchBMECOGSCI/ALVAS_LAB/data/'
# we need the IDs of all pictures and their valence
# take that from an existing processed file (02_recall_B_teszt_excel.xlsx)
# this should be changed - [generate a file which holds this data]
OS_pict_data <- read.xlsx(paste(pathw, "02_recall_B_teszt_excel.xlsx", sep=""), sheetName = "Munka1")
OS_pict_data <- OS_pict_data[,1:2]
# the procedure is cycled
# get OS data for study phase ####
for (i in 2:60){
  # starts with 0 or not
  if (i < 10){
    path <- paste(pathw, "0", sep = "")
  } else {
    path <- pathw
  }
  # A or B type
  if (file.exists(paste(path, as.character(i), "_B_teszt.csv", sep = ""))){
    OS_raw_data <- read.csv(paste(path, as.character(i), '_B_teszt.csv', sep =""))
    aorb <- 'B'
    if (ncol(OS_raw_data) == 1){
      OS_raw_data <- read.csv(paste(path, as.character(i), '_B_teszt.csv', sep =""), sep=";")
    }
  } else if (file.exists(paste(path, as.character(i), "_A_teszt.csv", sep = ""))){
    OS_raw_data <- read.csv(paste(path, as.character(i), '_A_teszt.csv', sep =""))
    aorb <- 'A'
    if (ncol(OS_raw_data) == 1){
      OS_raw_data <- read.csv(paste(path, as.character(i), '_A_teszt.csv', sep =""), sep=";")
    }
  }
  # starting to put together the data for the desired output
  # picture IDs from OS
  if (exists("OS_raw_data") == TRUE){
    OS_relev_data <- data.table(OS_raw_data$Pictures)
    colnames(OS_relev_data) <- "Pictures"
    # valence of pictures (based on OS_pict_data)
    for (j in 1:length(OS_relev_data$Pictures)){
      OS_relev_data$NeutNeg[j] <- OS_pict_data$Neut.Neg.[OS_pict_data$Pictures == OS_relev_data$Pictures[j]]
    }
    # block NR: 1:23[1] - 24:43[2] - 44:66[3]
    OS_relev_data$Block[1:23] <- 1
    OS_relev_data$Block[24:43] <- 2
    OS_relev_data$Block[44:66] <- 3
    # arousal responses (OS_raw_data - response_arousal)
    OS_relev_data$arousal <- OS_raw_data$response_arousal
    # valence responses
    OS_relev_data$valence <- OS_raw_data$response_valencia
    # last 3 are fillers, replace them with "NA"
    OS_relev_data[64:66, c('arousal', 'valence')] <- NA
    # remove raw data, so the cycle wont enter this "if" in the next cycle if there is no actual data
    remove("OS_raw_data")
    # write the results in an excel file
    write.xlsx(x = OS_relev_data, file = paste(i, "_", aorb, "_teszt_excel.xlsx", sep = ""),
               sheetName = "Munka1", row.names = FALSE)
  }
}

# get OS data for test phase ####

for (i in 1:60){
  # starts with 0 or not
  if (i < 10){
    path <- paste(pathw, "0", sep = "")
  } else {
    path <- pathw
  }
  # A or B type - we use the study phase outputs, to determine this
  if (file.exists(paste(path, as.character(i), "_B_teszt.csv", sep = ""))){
    OS_raw_data <- read.csv(paste(path, as.character(i), '_recall_teszt.csv', sep =""))
    aorb <- 'B'
    if (ncol(OS_raw_data) == 1){
      OS_raw_data <- read.csv(paste(path, as.character(i), '_recall_teszt.csv', sep =""), sep=";")
    }
  } else if (file.exists(paste(path, as.character(i), "_A_teszt.csv", sep = ""))){
    OS_raw_data <- read.csv(paste(path, as.character(i), '_recall_teszt.csv', sep =""))
    aorb <- 'A'
    if (ncol(OS_raw_data) == 1){
      OS_raw_data <- read.csv(paste(path, as.character(i), '_recall_teszt.csv', sep =""), sep=";")
    }
  }
  # starting to put together the data for the desired output
  # picture IDs from OS
  if (exists("OS_raw_data") == TRUE){
    OS_relev_data <- data.table(OS_raw_data$Pictures)
    colnames(OS_relev_data) <- "Pictures"
    # valence of pictures (based on OS_pict_data)
    for (j in 1:length(OS_relev_data$Pictures)){
      OS_relev_data$NeutNeg[j] <- OS_pict_data$Neut.Neg.[OS_pict_data$Pictures == OS_relev_data$Pictures[j]]
    }
    # arousal responses (OS_raw_data - response_arousal_f) - only responses 1 to 9 are valid
    OS_relev_data$arousal <- OS_raw_data$response_arousal_f
    OS_relev_data$arousal[(OS_relev_data$arousal %in% 1:9) == FALSE] <- NA
    # familiarity - only responses 1 AND 9 are valid
    OS_relev_data$familiarity <- OS_raw_data$response_familiarity_f_1
    OS_relev_data$familiarity[(OS_relev_data$familiarity == 1) | (OS_relev_data$familiarity == 9)] <- NA
    # valence responses - bit different from study phase column name (+'_f') - valid resp: 1:9
    OS_relev_data$valence <- OS_raw_data$response_valencia_f
    OS_relev_data$valence[(OS_relev_data$valence %in% 1:9) == FALSE] <- NA
    # last 3 are fillers, replace them with "NA"
    OS_relev_data[1:3, c('arousal', 'valence', 'familiarity')] <- NA
    OS_relev_data[44:46, c('arousal', 'valence', 'familiarity')] <- NA
    # remove raw data, so the cycle wont enter this "if" in the next cycle if there is no actual data
    remove("OS_raw_data")
    # write the results in an excel file
    write.xlsx(x = OS_relev_data, file = paste(i, "_recall_", aorb, "_teszt_excel.xlsx", sep = ""),
               sheetName = "Munka1", row.names = FALSE)
  }
}
