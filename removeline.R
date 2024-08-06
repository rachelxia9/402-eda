library(data.table)
library(tidyr)
library(tidyverse)


root_dir <- '/Users/rachelxia/CNS/EDA/testdata'
Data_folder <- file.path(root_dir, 'Baseline/Raw Data/TODO')
Data_output <- file.path(root_dir, 'Baseline/Done')

Data_files <- list.files(path = Data_folder, pattern = ".txt")

for (i in 1:length(Data_files)){
  
  filename <- Data_files[i]
  print(paste("Loading and formatting data from", filename, "(Participant", i,")"))
  mycon <- file(description=file.path(Data_folder, filename), open="r")
  mydat <- readLines(mycon)
  close(mycon)
  mydat <- mydat[3:length(mydat)]
  mydat <- tstrsplit(mydat, split="\t")
  mydat <- as.data.table(mydat)
  
  mydat <- mydat %>%
    select(-V3) %>%
    mutate(V1 = as.numeric(mydat$V1),
           V2 = as.numeric(mydat$V2), 
           V3 = as.numeric(mydat$V3))
  
  output_file <- file.path(Data_output, filename)  # Specify output file path
  
  write.table(mydat, file = output_file, sep="\t", row.names = FALSE, col.names = FALSE)
}

