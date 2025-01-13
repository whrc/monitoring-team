

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(here)


 ###########

# Define a vector for NA strings to avoid redundancy
na_strings <- c('-9999', 'NA', 'NaN', 'NAN', '-7999')

# Function to read data and set column names
read_and_name <- function(file_path, skip_lines) {
  # Read the data
  data <- fread(file_path, skip = skip_lines, na.strings = na_strings, header = FALSE)
  
  # Read header names
  header <- fread(file_path, skip = 1, nrows = 0, na.strings = na_strings)
  
  # Set names
  setnames(data, names(header))
  
  return(data)
}

# Read and name data for both files
df <- read_and_name('C:/Users/klynoe/Documents/resolute_bay/met/ResoluteMet_AllBiomet.dat', skip_lines = 4)
df1 <- read_and_name('C:/Users/klynoe/Documents/resolute_bay/met/ResoluteMet_AllLogger.dat', skip_lines = 4)

#############

epdf <- df1 %>%
  select(
    TIMESTAMP = 1,
    TA_1_1_1 = 8,
    RH_1_1_1 = 9,
    WS_1_1_1 = 10,
    MWS_1_1_1 = 11,
    WD_1_1_1 = 12,
    PA_1_1_1 = 13,
    P_RAIN_1_1_1 = 14,
    SHF_1_1_1 = 15,
    SHF_2_1_1 = 16,
    PPFD_1_1_1 = 21,
    PPFDR_1_1_1 = 23,
    SWIN_1_1_1 = 25,
    SWOUT_1_1_1 = 26,
    LWIN_1_1_1 = 29,
    LWOUT_1_1_1 = 30
  ) %>%
  # Convert PA_4_2_1_1_1 from mbar to kPa
  mutate(PA_1_1_1 = PA_1_1_1 / 10) #%>%
  # Separate TIMESTAMP into TIMESTAMP_1 and TIMESTAMP_2
  #separate(TIMESTAMP, into = c("TIMESTAMP_1", "TIMESTAMP_2"), sep = " ") %>%
  # Replace NA in TIMESTAMP_2 with default value
  #mutate(TIMESTAMP_2 = replace_na(TIMESTAMP_2, "00:00:00"))

str(epdf)

###subset the data by year and month and write/save them as individual files

#############

# Define the base file path
base_path <- "C:/Users/klynoe/Documents/resolute_bay/R_outputs/met/"

# Nest the data and prepare for saving
data_nested <- epdf %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(year_month = format(TIMESTAMP, "%Y%m")) %>%
  group_by(year_month) %>%
  nest() %>%
  ungroup() %>%  # Important to prevent errors with grouped data
  mutate(file_path = paste0(base_path, year_month, ".csv"))

########

# Define the units row as a data frame, ensuring all types are character
units_row <- data.frame(
  TIMESTAMP = "yyyy-mm-dd HH:MM", 
  SWOUT_1_1_1 = "W+1m-2", 
  SWIN_1_1_1 = "W+1m-2", 
  LWIN_1_1_1 = "W+1m-2", 
  LWOUT_1_1_1 = "W+1m-2",
  PPFD_1_1_1 = "umol+1m-2s-1", 
  PPFDR_1_1_1 = "umol+1m-2s-1", 
  TA_1_1_1 = "C", 
  RH_1_1_1 = "%", 
  SHF_1_1_1 = "W+1m-2", 
  SHF_2_1_1 = "W+1m-2", 
  WS_1_1_1 = "m+1m-1",
  MWS_1_1_1 = "m+1m-1", 
  WD_1_1_1 = "degrees", 
  PA_1_1_1 = "kPa", 
  P_RAIN_1_1_1 = "mm",
  stringsAsFactors = FALSE  # Ensure strings are not converted to factors
)

# Write each nested data frame to the specified file path, adding the units row
walk2(
  .x = data_nested$data, 
  .y = data_nested$file_path, 
  ~ {
    # Ensure the nested data frame has the same column names and types as units_row
    .x[] <- lapply(.x, as.character)  # Convert all columns to character
    
    # Combine units_row and the nested data frame
    combined_data <- bind_rows(units_row, .x)
    
    # Write to CSV
    write.csv(combined_data, .y, row.names = FALSE)
  }
)


