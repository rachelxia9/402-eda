# Load libraries (assuming these are necessary for your analysis)
library(readxl)
library(tidyverse)
library(splus2R)  # Assuming this is for the `peaks` function
library(data.table)
library(tidyr)
library(ggplot2)
library(chroma)
library(cowplot)


# Read data (adjust path as needed)
Data_folder <- "/Users/rachelxia/CNS/EDA/testdata/Data/post-threshold csv"
plot_output <- "/Users/rachelxia/CNS/EDA/testdata/plots/Artifacts"

# Dataframe of removed trials
total_filt <- data.frame(ID = character(), Filtered = numeric())


# List data files
Data_files <- list.files(path = Data_folder, pattern = ".csv")



# Loop through data files
for (i in 1:length(Data_files)) {
  data_filename <- Data_files[i]
  id_char <- str_extract(Data_files[i], "\\d+_\\d+")
  
  # Initialize variable to count filtered trials
  num_filt <- 0
  
  # Initialize empty data frame to store artifacts
  artifact <- data.frame()
  
  allcondition <- read.csv (paste0("/Users/rachelxia/CNS/EDA/testdata/Data/post-threshold csv/", id_char, "_threshold.csv"))
  
  # NEW: additional smoothing function by moving average across 20 data points (0.2s). https://rpubs.com/clayford/955067
  allcondition <- allcondition %>%
    group_by(trial) %>%
    mutate(GSR_blc = stats::filter(GSR_blc, rep(1/20, 20), sides = 1))
  allcondition$GSR_blc <- c(allcondition$GSR_blc[11:nrow(allcondition)], rep(NA, 10))
  allcondition <- allcondition[complete.cases(allcondition$GSR_blc), ]
  allcondition <- allcondition %>%
    group_by(trial) %>%
    mutate(GSR_blc = GSR_blc - (GSR_blc[62] - baseline))
  
  # Function to calculate slope between positive and negative peaks
  slope <- function(data) {
    z <- peaks(data$GSR_blc, span = 101)
    z_2 <- peaks(-data$GSR_blc, span = 101)
    z[1] <- TRUE # if it has one peak, calculate distance between start to peak time
    # Ensure at least one positive and one negative peak are found
    if (length(z) == 0 || length(z_2) == 0) {
      stop("No peaks found in the data.")
    }
    
    slope_value <- abs(c((data$GSR_blc[z] - data$GSR_blc[z_2]) / (data$trial_time[z] - data$trial_time[z_2]),
                         (data$GSR_blc[z[-1]] - data$GSR_blc[z_2[-length(z_2)]]) / (data$trial_time[z[-1]] - data$trial_time[z_2[-length(z_2)]])))
    
    return(slope_value)
  }
  
  # Function to calculate width between consecutive negative peaks
  width <- function(data) {
    z <- peaks(data$GSR_blc, span = 101)
    z_2 <- peaks(-data$GSR_blc, span = 101)
    # Ensure at least two negative peaks are found
    if (length(z_2) < 2) {
      stop("Less than two negative peaks found.")
    }
    
    a <- data$trial_time[which(z_2)]
    width_value <- abs(c(a[-length(a)] - a[-1]))
    
    # width_value <- abs(data$trial_time[z_2] - data$trial_time[z_2 + 1])
    
    return(width_value)
  }
  

  # Loop over unique trials
  for (j in unique(allcondition$trial)) {
    temp <- allcondition %>%
      filter(trial == j)
    slope_value <- slope(temp)
    width_value <- width(temp)
    
    if (length(slope_value) == 0 || length(width_value) == 0) {
      print(paste("No peaks found in trial ", j))
    }
    else if (any(slope_value > 2) || any(width_value < 1)) {
      allcondition <- allcondition %>%
        filter(trial != j)
      artifact <- rbind(artifact, temp)
      num_filt <- num_filt + 1}
    
  }

  print(num_filt)
 

  # Create total_filt data frame (assuming it was initialized elsewhere)
  total_filt <- rbind(total_filt, data.frame(ID = id_char, filtered = num_filt))

  
  #save the filtered allcondition into a csv file
  output_path <- file.path (paste0("/Users/rachelxia/CNS/EDA/testdata/plots/Artifacts/", id_char, "_threshold.csv"))
  fwrite(allcondition, file = output_path)

  
  
  # Plotting
  
  p <- ggplot(data = allcondition) +
    aes(x = trial_time, y = GSR_blc, group = trial) +
    labs(x = "Time after event (seconds)", y = "GSR amplitude", title = id_char) +
    scale_color_cubehelix_d(h = 0, rot = -0.5, c = 1, l = c(0.2, 0.7), reverse = TRUE) +
    geom_line(linewidth = 0.3) +
    geom_vline(xintercept = 0, lty = "dashed")
  theme_bw()
  
  ggsave(paste(plot_output, "/cleaned_v3/", id_char, ".png", sep = ""), plot = p, width = 10, height = 8, dpi = 300)
  

  if (num_filt == 0) {
  next
  } else {
    # plot with artifacts in red
    p_marked <- ggplot(data = allcondition) +
      aes(x = trial_time, y = GSR_blc, group = trial) +
      geom_line(linewidth = 0.3) +
      geom_line(data = artifact, aes(group = trial), color = "red", linewidth = 0.3) +
      theme_bw()
    print(p_marked)
    
    ggsave(paste(plot_output, "/cleaned_marked/", id_char, ".png", sep = ""), plot = p_marked, width = 10, height = 8, dpi = 300)
    
    # plot of just artifact
    p_artifact <- ggplot(data = artifact) +
      aes(x = trial_time, y = GSR_blc, group = trial) +
      geom_line(linewidth = 0.3) +
      theme_bw()

    ggsave(paste(plot_output, "/cleaned_artifact_only/", id_char, ".png", sep = ""), plot = p_artifact, width = 10, height = 8, dpi = 300)
  }
  
}

  # Write removed trials to CSV

 output_filt <- file.path("/Users/rachelxia/CNS/EDA/testdata/Data", "Filtered2.csv")
 fwrite(total_filt, file = output_filt)

  print("finished")


