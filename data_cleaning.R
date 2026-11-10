# =============================================================================
# Deviating from Preregistration: Does It Hurt Researchers' Credibility?
# Data cleaning script
#
# Creates the data file shared on OSF (Data_PreregDevation_OSF.txt) from the
# raw survey export. The raw data are not shared to protect participants'
# privacy; they are available upon request.
#
# Input:  data/source_2024-03-30.csv        (raw Qualtrics export, not shared)
# Output: data/Data_PreregDevation_OSF.txt  (shared on OSF)
# =============================================================================

rm(list = ls())

# Set the working directory to the folder that contains this script.
# (Not needed if you open the project via preregistration-deviations.Rproj.)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
  if (nzchar(script_path)) setwd(dirname(script_path))
}

library(readr)  # read_csv()

# Load data
source_2024_03_30 <- read_csv("data/source_2024-03-30.csv")

# Remove the two extra header rows of the Qualtrics export
data <- source_2024_03_30[3:nrow(source_2024_03_30), ]

# Progress is read as text; make it numeric before comparing it with 97
data$Progress <- as.numeric(as.character(data$Progress))


# Drop-outs: where did participants stop? -------------------------------------

# Everyone who did not complete the survey or did not give consent
dropouts <- data[!(data$Progress > 97 & data$consent == "Yes"), ]

# Question columns (from column 10 onwards)
question_cols <- dropouts[, 10:ncol(dropouts)]

# Find the first question after which all answers are missing
find_dropout_point <- function(row) {
  na_flags <- is.na(row)
  if (all(na_flags)) return(names(row)[1])  # stopped before answering anything
  for (i in seq_along(row)) {
    if (is.na(row[i]) && all(na_flags[i:length(row)])) {
      return(names(row)[i])
    }
  }
  return(NA)  # did not stop in the middle
}

dropout_points <- apply(question_cols, 1, find_dropout_point)
dropout_freq   <- table(factor(dropout_points, levels = colnames(question_cols)))
print(dropout_freq)


# Exclusions ------------------------------------------------------------------

# We exclude everyone who completed less than 98% of the survey or did not
# give consent. Participants with 98% answered all questions but did not
# click the final submit button (Finished = FALSE in the data).
dat <- data[which(data$Progress > 97 & data$consent == "Yes"), ]


# Data for OSF ----------------------------------------------------------------

# Keep the variables used in the analyses (open-text responses are removed)
data_OSF <- dat[, c(4:26, 28, 30:32, 34, 36:38, 40, 42:45, 47, 49:53, 55, 57:59, 61:63, 66)]
write.table(data_OSF, "data/Data_PreregDevation_OSF.txt", row.names = FALSE)

rm(source_2024_03_30, data)
