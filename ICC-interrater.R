library(irr)

threshold_data <- "/Users/rachelxia/CNS/EDA/testdata/Data/Thresholds_Combined.csv"


# inter-rater: two way mixed effects - single rater - absolute agreement
# > 0.9 = excellent reliability
# test-retest: consistency

threshold <- read.csv(threshold_data)

icc_lower <- icc(cbind(threshold$LowerThres_Rater1, threshold$LowerThres_Rater2), type = "agreement", unit = "single", model="twoway") #ICC = 0.931
icc_upper <- icc(cbind(threshold$UpperThres_Rater1, threshold$UpperThres_Rater2), type = "agreement", unit = "single", model="twoway") #ICC = 0.901


print(icc_lower)
print(icc_upper)

icc_results <- data.frame(
  "Measure" = c("Lower", "Upper"),
  "ICC.subjects" = c(icc_lower$subjects, icc_upper$subjects), 
  "ICC.raters" = c(icc_lower$raters, icc_upper$raters), 
  "ICC_value" = c(icc_lower$value, icc_upper$value)
)

# Specify the file path to save the CSV file
output_file <- "/Users/rachelxia/CNS/EDA/testdata/Data/icc_results.csv"

# Write ICC results to CSV file
write.csv(icc_results, file = output_file)


# Print a message indicating success
cat("ICC results have been saved to:", output_file, "\n")
