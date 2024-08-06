library(readxl)
library(tidyverse)
library(data.table)
library(tidyr)
library(stringr)

# Set directory paths
root_dir <- '/Users/rachelxia/CNS/EDA/testdata'
data_folder <- file.path(path = '/Users/rachelxia/CNS/EDA/testdata/Baseline/Ledalab output/Todo')
data_files <- list.files(data_folder, pattern = ".csv")

# Read the event start times
evt <- read_excel(file.path(root_dir, 'Baseline/EventStart.xlsx'), col_names = TRUE)

# Initialize the data frame for storing averages
avg <- data.frame(ID = character(),
                  GSC = numeric(),
                  tonic = numeric(),
                  phasic = numeric(),
                  stringsAsFactors = FALSE)

# Process each data file
for (data_file in data_files) {
  # Read the data file
  data <- read.csv(file.path(data_folder, data_file), header = TRUE)
  
  # Extract the ID from the filename
  id_char <- str_extract(data_file, "\\d+_\\d+")
  
  # Find the start time from the event file
  evt_start <- as.character(evt$Start[evt$ID == id_char])
  

  # Convert to numeric, ensuring non-numeric values become NA
  evt_start <- as.numeric(evt_start)
  
  # DEBUGGING - Check if evt_start contains any NA values after conversion
  if (any(is.na(evt_start))) {
    warning(paste("Non-numeric or missing event start time for ID:", id_char))
    next
  }
  if (length(evt_start) == 0) {
    warning(paste("Start time not found for ID:", id_char))
    next
  }
  
  # DEBUGGING - Check if evt_start associated with id_char is numeric
  
  if (!all(is.numeric(as.numeric(evt_start)))) {
    warning(paste("Non-numeric event start time for ID:", id_char))
    next
  }
  
  # Find the starting index in the data
  start <- which.min(abs(data$seconds - evt_start))
  
  # Select the data from 50s-250s, filter out the first and last 50s
  selected_data <- data[(start + 5000):(start + 25000), ]
  
  # Calculate averages
  temp <- data.frame(ID = id_char,
                     GSC = mean(selected_data$GSR, na.rm = TRUE),
                     tonic = mean(selected_data$tonic, na.rm = TRUE),
                     phasic = mean(selected_data$phasic, na.rm = TRUE))
  
  # Append to the results
  avg <- rbind(avg, temp)
}

# Write the results to a CSV file
output_file <- file.path(data_folder, "Baseline_Average.csv")
fwrite(avg, file = output_file, col.names = TRUE)
