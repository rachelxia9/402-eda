# Install devtools chroma package
# devtools::install_github("jiho/chroma")

# Load libraries
library(readxl)
library(tidyverse)
library(data.table)
library(tidyr)
library(ggplot2)
library(chroma)
library(cowplot)

# Define directories
root_dir <- '/Users/rachelxia/CNS/EDA/testdata'
Data_folder <- file.path(root_dir, 'Baseline/Ledalab output/Done/')
data_output <- file.path(root_dir, 'Baseline/Full Trial Artifact Output/')
plot_output <- file.path(root_dir, 'plots/Artifacts/')
temp_path <- '/Users/rachelxia/CNS/EDA/testdata/Data/R Analysis output/'

# List data files
Data_files <- list.files(path = Data_folder, pattern = ".csv")
bslinfo <- read.csv("/Users/rachelxia/CNS/EDA/testdata/Baseline/Baseline Correction/Baseline_Average.csv", header = TRUE)

# Load threshold file and create an empty dataframe to record the number of trials removed
trshld <- read_excel("/Users/rachelxia/CNS/EDA/testdata/Threshold Ratings - Rachel.xlsx") %>%
  select("ID", "Lower Threshold", "Upper Threshold") %>%
  na.omit()

removed_trials <- data.frame(ID = character(), Before = numeric(), After = numeric(), Removed = numeric())

# Baseline function
baseline <- function(data, bsl) {
  data <- data %>%
    group_by(trial) %>%
    mutate(baseline = bsl,
           GSR_blc = phasic - (phasic[71] - bsl))
  data
}

# Max trial function
maxtrial <- function(data1, data2) {
  trial_count <- max(data1$trial)
  data2$trial <- data2$trial + trial_count
  data2
}

# Loop through data files
for (i in 1:length(Data_files)) {
  data_filename <- Data_files[i]
  id_char <- str_extract(Data_files[i], "\\d+_\\d+")
  
  # Extract and check bsl value, convert to numeric
  bsl <- as.numeric(bslinfo$phasic[bslinfo$ID == id_char])
  if (is.na(bsl)) {
    warning(paste("bsl not found for ID", id_char, "- skipping this file"))
    next
  }
  
  Old_Corr <- read.csv(paste(temp_path, "Correct/", id_char, "_Old_Correct.csv", sep = ""), header = TRUE)
  Old_Incorr <- read.csv(paste(temp_path, "Incorrect/", id_char, "_Old_Incorrect.csv", sep = ""), header = TRUE)
  New_Corr <- read.csv(paste(temp_path, "Correct/", id_char, "_New_Correct.csv", sep = ""), header = TRUE)
  New_Incorr <- read.csv(paste(temp_path, "Incorrect/", id_char, "_New_Incorrect.csv", sep = ""), header = TRUE)
  
  # Convert 'code' columns to character to avoid type mismatch
  Old_Corr$code <- as.character(Old_Corr$code)
  Old_Incorr$code <- as.character(Old_Incorr$code)
  New_Corr$code <- as.character(New_Corr$code)
  New_Incorr$code <- as.character(New_Incorr$code)
  
  New_Corr <- baseline(New_Corr, bsl)
  New_Incorr <- baseline(New_Incorr, bsl)
  Old_Corr <- baseline(Old_Corr, bsl)
  Old_Incorr <- baseline(Old_Incorr, bsl)
  
  Old_Incorr <- maxtrial(Old_Corr, Old_Incorr)
  New_Corr <- maxtrial(Old_Incorr, New_Corr)
  New_Incorr <- maxtrial(New_Corr, New_Incorr)
  
  allcondition <- bind_rows(Old_Corr, Old_Incorr, New_Corr, New_Incorr)
  
  # Record number of trials before filtering
  trials_pre <- n_distinct(allcondition$trial)
  
  # Remove data exceeding thresholds
  upper <- trshld$"Upper Threshold"[trshld$ID == id_char]
  lower <- trshld$"Lower Threshold"[trshld$ID == id_char]
  
  allcondition <- allcondition %>%
    group_by(trial) %>%
    dplyr::filter(all(GSR_blc < upper & GSR_blc > lower))
  
  ##TODO: save the filtered allcondition into a csv file
  output_path <- file.path (paste0("/Users/rachelxia/CNS/EDA/testdata/plots/Artifacts/", id_char, "_threshold.csv"))
  fwrite(allcondition, file = output_path)
  
  # Record number of trials after filtering
  trials_post <- n_distinct(allcondition$trial)
  removed_trials <- rbind(removed_trials, data.frame(ID = id_char,
                                                     Before = trials_pre,
                                                     After = trials_post,
                                                     Removed = trials_pre - trials_post))
  
  # Plot
  p <- ggplot(data = allcondition) +
    aes(x = trial_time, y = GSR_blc, group = trial) +
    labs(x = "Time after event (seconds)", y = "GSR amplitude", title = id_char) +
    scale_color_cubehelix_d(h = 0, rot = -0.5, c = 1, l = c(0.2, 0.7), reverse = TRUE) +
    geom_line(linewidth = 0.3) +
    geom_vline(xintercept = 0, lty = "dashed") +
    theme_bw()
  
  ggsave(paste(plot_output, "/cleaned/", id_char, ".png", sep = ""), plot = p, width = 10, height = 8, dpi = 300)
}

# Write removed trials to CSV
output_file <- file.path("/Users/rachelxia/CNS/EDA/testdata/Data", "Removed_Trials.csv")
fwrite(removed_trials, file = output_file)
print("finished")
