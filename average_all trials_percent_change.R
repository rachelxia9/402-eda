library(ggplot2)
library(tidyverse)
library(data.table)

# Read data (adjust path as needed)
Data_folder <- "/Users/rachelxia/CNS/EDA/testdata/Data/post-threshold csv"
plot_output <- "/Users/rachelxia/CNS/EDA/testdata/plots/Artifacts"

# List data files
Data_files <- list.files(path = Data_folder, pattern = ".csv")
bslinfo <- read.csv("/Users/rachelxia/CNS/EDA/testdata/Baseline/Baseline Correction/Baseline_Average.csv", header = TRUE)

#Create dataframe for percent change results
percent_change <- data.frame(ID = character(), Percent_Change = numeric(), BSL = numeric(), mean_SCR = numeric())


# Loop through data files
for (i in 1:length(Data_files)) {
  data_filename <- Data_files[i]
  id_char <- str_extract(Data_files[i], "\\d+_\\d+")
  
  # Extract and convert bsl to numeric
  bsl <- as.numeric(bslinfo$phasic[bslinfo$ID == id_char])

  # Extract session info from cleaned data
  allcondition <- read.csv (paste0("/Users/rachelxia/CNS/EDA/testdata/Plots/Artifacts/second-cleaning-csv/", id_char, "_threshold.csv"))
  
  
  #average GSR_blc across times for each trial in allcondition
  average_gsr <- subset(allcondition, trial_time > 0) %>%
    group_by(trial) %>%
    summarise(avg_gsr = mean(GSR_blc),
              sd_gsr = sd(GSR_blc))
  
  #Removing trials with small SCR (< 0.02 + baseline phasic) Need to check with Noah if want to include
  temp <- allcondition %>%
    inner_join(average_gsr, by = "trial") %>%
    filter(avg_gsr > 0.02)
  
  #average GSR_blc across times for each trial in temp
  average_gsr_2 <- temp %>%
    group_by(trial) %>%
    summarise(avg_gsr = mean(GSR_blc),
              sd_gsr = sd(GSR_blc))
  
  #Get total Mean and SD
  #TODO: create dataframe and save data_avg from all sessions
  data_avg <- average_gsr_2 %>%
    summarise(Mean = mean(avg_gsr),
              SD = mean(avg_gsr))
  
  #TODO: compute percent change from baseline phasic
  SCR_change = ((data_avg$Mean - bsl)/bsl) * 100 
  
  # Create total_filt data frame (assuming it was initialized elsewhere)
  percent_change <- rbind(percent_change, data.frame(ID = id_char, Percent_change = SCR_change, BSL = bsl, Mean_SCR = data_avg$Mean))

  
  # #plot: visualizing all trials and mean and sd
  # p <- ggplot(temp,aes(trial_time, GSR_blc))+
  #   theme_classic()+
  #   geom_line(aes(group = trial), alpha = 0.3, linewidth = 0.2)+
  #   guides(alpha= "none")+
  #   stat_summary(fun.data = mean_cl_boot,geom = "ribbon",alpha = 0.2,aes(fill = "red"))+
  #   guides(fill= "none")+
  #   stat_summary(fun = mean,geom = "line",linewidth = 1,aes(colour = "red"))+
  #   guides(colour = "none")+
  #   geom_vline(xintercept = 0,linetype = "dashed" )
  # 
  # ggsave(paste(plot_output, "/final/", id_char, ".png", sep = ""), plot = p, width = 10, height = 8, dpi = 300)
   }

# Write percent change summary to CSV

# output_file <- file.path("/Users/rachelxia/CNS/EDA/testdata/Data", "PercentChange.csv")
# fwrite(percent_change, file = output_file)
  
  # summary file of percent change with bsl and mean SCR info
output_file <- file.path("/Users/rachelxia/CNS/EDA/testdata/Data", "PercentChange_WithBSL.csv")
fwrite(percent_change, file = output_file)

print("finished")

