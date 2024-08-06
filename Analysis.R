library(readxl)
library(tidyverse)
library(data.table) 
library(tidyr)
library(zoo)
library(signal)
library(cowplot)
library(fuzzyjoin)

root_dir <- '/Users/rachelxia/CNS/EDA/testdata'
BHV_code_folder <- file.path(root_dir, 'BHV/Code/Todo/')
Data_folder <- file.path(root_dir, 'Data/Ledalab output/Todo')
data_output <- file.path(root_dir, 'Data/R Analysis output')
event_folder <- file.path('/Users/rachelxia/CNS/EDA/testdata/Evt/Todo')
event_output <- file.path(root_dir, 'Evt/output')
plot_output <- file.path(root_dir, 'plots')

# Get files
BHV_code <- list.files(path = BHV_code_folder, pattern=".xlsx")
Evt_files <- list.files (path = event_folder, pattern = ".txt")
Data_files <- list.files(path = Data_folder, pattern = ".csv")

trial_time <- data.frame(Index = double(),
                         Time = double(),
                         ID = character())
trial_count_Word <- data.frame(ID = character(),
                               Old_Correct = double(),
                               New_Correct = double(),
                               Old_Incorrect = double(),
                               New_Incorrect = double())
trial_count_confidence <- data.frame(ID = character(),
                                     Correct_High = double(),
                                     Incorrect_High = double(),
                                     Correct_Low = double(),
                                     Incorrect_Low = double())

# Assign trial times function
assigntime <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  
  trial_counter <- 1
  data$trial_time <- NA
  for (m in 1:nrow(data)) {
    if (data$trial[m] == trial_counter) {
      end_index <- min(nrow(data), m + 420)  # Ensure the end index does not exceed the number of rows
      data$trial_time[m:end_index] <- seq(-0.7, 3.5, by = 0.01)[1:(end_index - m + 1)]
      trial_counter <- trial_counter + 1
    }
  }
  data
}

# loop to work on each participant file
for (i in 1:length(BHV_code)){
  raw_code <- read_excel(paste(BHV_code_folder, BHV_code[i], sep = ""), 
                         col_names = FALSE)
  colnames(raw_code)<-("code")
  raw_code$code <- as.character(raw_code$code)
  raw_code[, "group"] <- NA
  group_code <- c()
  counter <- 0
  block_num <- 1
  par_note <- c()
  
  # Define condition files
  Old_Corr <- data.frame()
  Old_Corr_num = 0
  New_Corr <- data.frame()
  New_Corr_num = 0
  Old_Incorr <- data.frame()
  Old_Incorr_num = 0
  New_Incorr <- data.frame()
  New_Incorr_num = 0
  Corr_High <- data.frame()
  Corr_High_num = 0
  Corr_Low <- data.frame()
  Corr_Low_num = 0
  Incorr_High <- data.frame()
  Incorr_High_num = 0
  Incorr_Low <- data.frame()
  Incorr_Low_num = 0
  
  # Assign each trial into a group in raw code file
  for (j in 1: nrow(raw_code)){
    if (raw_code$code[j] == 10){
      group_code <- c(group_code, "O")
    } else if(raw_code$code[j] == 11){
      group_code <- c(group_code, "N")
    } else if(raw_code$code[j] == 20){
      group_code <- c(group_code, "R")
    } else if(raw_code$code[j] == 21){
      group_code <- c(group_code, "W")
    } else if(raw_code$code[j] == 22){
      group_code <- c(group_code, "N")
    } else if(raw_code$code[j] == 30){
      group_code <- c(group_code, "H")
    } else if(raw_code$code[j] == 31){
      group_code <- c(group_code, "L")
    } else if(raw_code$code[j] == 32){
      group_code <- c(group_code, "N")
    } else if(raw_code$code[j] == 40){
      group_code <- c(group_code, "P")
    } else if(raw_code$code[j] == 41){
      group_code <- c(group_code, "N")
    }
    counter <- counter + 1
    if (counter == 4){
      raw_code$group[(j-3):j] <- paste(group_code, collapse = "")
      group_code <- c()
      counter <- 0
    }
  }
  

  
  
  # Read txt event info file
  evt_filename <- Evt_files[i]
  
  participant_id_char <- gsub(x=evt_filename, pattern="\\_event.txt", replacement="")
  
  evtcon <- file(description=file.path(event_folder, evt_filename), open="r")
  evtdat <- readLines(evtcon)
  close(evtcon)
  # Delete first lines without data
  evtdat <- evtdat[2:length(evtdat)]%>%
    tstrsplit(evtdat, split="\t")%>%
    as.data.table(evtdat)%>%
    subset(select = -c(V1))
  colnames(evtdat) <- c("code", "seconds")
  evtdat$code <- as.character(evtdat$code)
  evtdat$seconds <- as.numeric(evtdat$seconds)
  # Exlucde event without information
  evtdat <- evtdat[evtdat$code %in% c("Old Word Appeared",
                                      "New Word Appeared",
                                      "Correct Answer Given",
                                      "Incorrect Answer Given",
                                      "Unanswered",
                                      "Answered Confident",
                                      "Answered Unconfident",
                                      "Positive Feedback Appeared",
                                      "Negative Feedback Appeared")]
  
  # Match and replace event code with raw code, return error if mismatch
  check <- c()
  evtdat[, "block"] <- NA
  for (k in 1: nrow(evtdat)){
    if (evtdat$code[k] == "Old Word Appeared"){
      check <- c(10)
    } else if (evtdat$code[k] == "New Word Appeared") {
      check <- 11
    } else if (evtdat$code[k] == "Correct Answer Given") {
      check <- 20
    } else if (evtdat$code[k] == "Incorrect Answer Given") {
      check <- 21
    } else if (evtdat$code[k] == "Unanswered") {
      check <- 22
    } else if (evtdat$code[k] == "Answered Confident") {
      check <- 30
    } else if (evtdat$code[k] == "Answered Unconfident") {
      check <- 31
    } else if (evtdat$code[k] == "Positive Feedback Appeared") {
      check <- 40
    } else if (evtdat$code[k] == "Negative Feedback Appeared") {
      check <- 41
    } 
    if (raw_code$code[k] == check){
      evtdat$code[k] <- raw_code$code[k]
      # replace answered unconfident with unanswered confident 
    }else if (raw_code$code[k] == 32 && check == 31){
      evtdat$code[k] <- raw_code$code[k]
    }else{
      evtdat$code[k] <- raw_code$code[k]
      par_note <- c(paste(participant_id_char, "error at", k,",", evtdat$code[k],"!=", raw_code$code[k]))
      
    }
    if (evtdat$seconds[k+1] - evtdat$seconds[k] < 3 | k == 800){
      evtdat$block[k] <- block_num
    } else{
      evtdat$block[k] <- block_num
      block_num = block_num + 1
      if (block_num > 4){
        break
        print("error: too many blocks")
      }
    }
  }
  
  if(length(par_note) > 0){
    writeLines(par_note, paste(event_output, participant_id_char,"_note.txt", sep=""))
  } 
  
  # Move group column to event data file
  evtdat$group <- raw_code$group
  
  # Record number of trials and trial lengths for all participant from word onset to next word onset
  new <- c()
  index <- 0
  for (t in 1:nrow(evtdat)){
    if (evtdat$code[t] == 10 | evtdat$code[t] == 11){
      index = index+1
      new <- c(index, evtdat$seconds[(t+4)]-evtdat$seconds[t], participant_id_char)
      trial_time[nrow(trial_time) + 1, ] <- new 
    } else{}
  }
  
  # Read data files, merge with event data files
  print(paste("processing:", participant_id_char,sep=""))
  dat_filename <- Data_files[i]
  dat_filtpath <- file.path(Data_folder, dat_filename)
  mydat<-read.csv(dat_filtpath, header = TRUE)
  mydat$orig_idx <- 1:nrow(mydat)
  mydat$participant_id<-participant_id_char
  
  # merge evtdat, fill group and block number
  print("Merging event data")
  mydat <- fuzzy_full_join(mydat,evtdat, by="seconds", match_fun = ~abs(.x-.y) <0.008)
  
  for (c in 1:nrow(mydat)){
    if (is.na(mydat$code[c]) || is.na(mydat$code[c+1])){
      next
    } else if (mydat$code[c] == mydat$code[c+1]){
      mydat$code[c] <- NA
      mydat$block[c] <- NA
      mydat$group[c] <- NA
    }
  }
  mydat <- mydat%>% 
    select(-seconds.y)%>%
    arrange(orig_idx)
  
  fwrite(mydat, file = paste(data_output, participant_id_char, ".csv", sep=""), 
         col.names = TRUE)
  # 
  print("Extracting Word conditions")
  for (g in 1:nrow(mydat)){
    if (is.na(mydat$group[g])){
      next
      
    } else if ((mydat$group[g] == "ORHP" | mydat$group[g] == "ORLP") & mydat$code[g] == 10){
      Old_Corr_num = Old_Corr_num + 1
      new_cols <-  mydat[(g-70):(g + 350),]%>%
        mutate(trial = Old_Corr_num)
      Old_Corr <- rbind(Old_Corr, new_cols)
      
    } else if ((mydat$group[g] == "NRHP" | mydat$group[g] == "NRLP") & mydat$code[g] == 11){
      New_Corr_num = New_Corr_num + 1
      new_cols <-  mydat[(g-70):(g + 350),] %>%
        mutate(trial = New_Corr_num)
      New_Corr <- rbind(New_Corr, new_cols)
      
    } else if ((mydat$group[g] == "OWHN" | mydat$group[g] == "OWLN") & mydat$code[g] == 10){
      Old_Incorr_num = Old_Incorr_num + 1
      new_cols <-  mydat[(g-70):(g + 350),] %>%
        mutate(trial = Old_Incorr_num)
      Old_Incorr <- rbind(Old_Incorr, new_cols)
      
    } else if ((mydat$group[g] == "NWHN" | mydat$group[g] == "NWLN") & mydat$code[g] == 11){
      New_Incorr_num = New_Incorr_num + 1
      new_cols <-  mydat[(g-70):(g + 350),] %>%
        mutate(trial = New_Incorr_num)
      New_Incorr <- rbind(New_Incorr, new_cols)
    }
  }
  
  
  Old_Corr <- assigntime(Old_Corr)
  Old_Incorr <- assigntime(Old_Incorr)
  New_Corr <- assigntime(New_Corr)
  New_Incorr <- assigntime(New_Incorr)
  
  print("Writing Word conditions files")
  fwrite(Old_Corr, file = paste(data_output, "/Correct/",participant_id_char, "_Old_Correct.csv", sep=""), col.names = TRUE)
  fwrite(New_Corr, file = paste(data_output, "/Correct/",participant_id_char, "_New_Correct.csv",sep=""), col.names = TRUE)
  fwrite(Old_Incorr, file = paste(data_output, "/Incorrect/",participant_id_char, "_Old_Incorrect.csv",sep=""), col.names = TRUE)
  fwrite(New_Incorr, file = paste(data_output, "/Incorrect/",participant_id_char, "_New_Incorrect.csv",sep=""), col.names = TRUE)
  tc <- c(participant_id_char ,Old_Corr_num, New_Corr_num, Old_Incorr_num, New_Incorr_num)
  trial_count_Word[nrow(trial_count_Word) + 1, ] <- tc
  
  print("Extracting confidence conditions")
  for (g in 1:nrow(mydat)){
    if (is.na(mydat$group[g])){
      next
      
    } else if ((mydat$group[g] == "ORHP" | mydat$group[g] == "NRHP") & 
               (mydat$code[g] == 10 | mydat$code[g] == 11)){
      Corr_High_num = Corr_High_num + 1
      new_cols <-  mydat[(g-70):(g + 350),] %>%
        mutate(trial = Corr_High_num)
      Corr_High <- rbind(Corr_High, new_cols)
      
    } else if ((mydat$group[g] == "OWHN" | mydat$group[g] == "NWHN") & 
               (mydat$code[g] == 10 | mydat$code[g] == 11)){
      Incorr_High_num = Incorr_High_num + 1
      new_cols <-  mydat[(g-70):(g + 350),] %>%
        mutate(trial = Incorr_High_num)
      Incorr_High <- rbind(Incorr_High, new_cols)
      
    } else if ((mydat$group[g] == "ORLP" | mydat$group[g] == "NRLP") & 
               (mydat$code[g] == 10 | mydat$code[g] == 11)){
      Corr_Low_num = Corr_Low_num + 1
      new_cols <- mydat[(g-70):(g + 350),] %>%
        mutate(trial = Corr_Low_num)
      Corr_Low <- rbind(Corr_Low, new_cols)
      
    } else if ((mydat$group[g] == "OWLN" | mydat$group[g] == "NWLN") & 
               (mydat$code[g] == 10 | mydat$code[g] == 11)){
      Incorr_Low_num = Incorr_Low_num + 1
      new_cols <- mydat[(g-70):(g + 350),] %>%
        mutate(trial = Incorr_Low_num)
      Incorr_Low <- rbind(Incorr_Low, new_cols)
    }
  }
  
  # Assign trials times
  Corr_High <- assigntime(Corr_High)
  Corr_Low <- assigntime(Corr_Low)
  Incorr_High <- assigntime(Incorr_High)
  Incorr_Low <- assigntime(Incorr_Low)
  
  print("Writing confidence conditions files")
  fwrite(Corr_High, file = paste(data_output, "/High/",participant_id_char, "_Correct_High.csv",sep=""), col.names = TRUE)
  fwrite(Incorr_High, file = paste(data_output, "/High/",participant_id_char, "_Incorrect_High.csv",sep=""), col.names = TRUE)
  fwrite(Corr_Low, file = paste(data_output, "/Low/",participant_id_char, "_Correct_Low.csv",sep=""), col.names = TRUE)
  fwrite(Incorr_Low, file = paste(data_output, "/Low/",participant_id_char, "_Incorrect_Low.csv",sep=""), col.names = TRUE)
  
  tc <- c(participant_id_char, Corr_High_num, Incorr_High_num, Corr_Low_num, Incorr_Low_num)
  trial_count_confidence[nrow(trial_count_confidence) + 1, ] <- tc
  
  print("plotting")
  
  phasic_p <- ggplot(data = mydat,
                     aes(x = seconds.x, y = phasic))+
    geom_line()
  ggsave(paste(plot_output,"/Phasic_tonic/",participant_id_char,"phasic.png",sep=""), plot = phasic_p, width = 12, height = 8, dpi = 300)
  
  
  table_plot <- mydat %>%
    pivot_longer(cols = c(GSR, tonic))
  
  table_plot$name <- factor(table_plot$name, levels = c("tonic", "GSR"))
  
  tonic_p <- ggplot(data = table_plot,
                    aes(x = seconds.x, y = value, colour = name)) + 
    geom_line()
  ggsave(paste(plot_output,"/Phasic_tonic/",participant_id_char,"tonic.png", sep=""), plot = tonic_p, width = 12, height = 8, dpi = 300)
  
}
fwrite(trial_count_Word, file = paste(data_output,"trial_count_word.csv",sep=""), col.names = TRUE)
fwrite(trial_count_confidence, file = paste(data_output,"trial_count_confidence.csv",sep=""), col.names = TRUE)

print("finished")

