# Assignment-3
Assignment 3

# Set the source file path
source_file <- "downloads/StormEvents_details-ftp_v1.0_d2009_c20220425.csv"

# Set the destination directory path within your current working directory
destination_dir <- "Desktop"

# Copy the file to the destination directory
file.copy(from = source_file, to = destination_dir)

# Read the CSV file from the destination directory
data <- read.csv(source_file)

# Save the modified data to a new CSV file within the destination directory
new_file_path <- paste0(destination_dir, "modified_file.csv")
write.csv(data, file = new_file_path, row.names = FALSE)

# Selecting the desired columns
filtered_data <- data[, c("BEGIN_YEARMONTH", "EPISODE_ID", "STATE", "STATE_FIPS", "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE")]

# Viewing the filtered dataframe
print(filtered_data)

library(dplyr)

# Arrange the data by state name
arranged_data <- arrange(filtered_data, STATE)

# Viewing the arranged dataframe
print(arranged_data)

library(stringr)

# Change state and county names to title case
arranged_data$STATE <- str_to_title(arranged_data$STATE)
arranged_data$CZ_NAME <- str_to_title(arranged_data$CZ_NAME)

# Viewing the modified dataframe
print(arranged_data)

library(dplyr)

# Limit to events listed by county FIPS (CZ_TYPE of "C")
filtered_data <- filter(arranged_data, CZ_TYPE == "C")

# Remove the CZ_TYPE column
filtered_data <- select(filtered_data, -CZ_TYPE)

# Viewing the modified dataframe
print(filtered_data)

library(stringr)
library(tidyr)

# Pad the state and county FIPS codes with a "0" at the beginning
filtered_data$STATE_FIPS <- str_pad(filtered_data$STATE_FIPS, width = 2, pad = "0")
filtered_data$CZ_FIPS <- str_pad(filtered_data$CZ_FIPS, width = 3, pad = "0")

# Unite the state and county FIPS codes into a new FIPS column
filtered_data <- unite(filtered_data, col = "FIPS", STATE_FIPS, CZ_FIPS, sep = "-")

# Viewing the modified dataframe
print(filtered_data)

library(dplyr)

# Change all column names to lowercase
filtered_data <- rename_all(filtered_data, tolower)

# Viewing the modified dataframe with lowercase column names
print(filtered_data)

# Get the state data from base R
data("state")

# Create a dataframe with state name, area, and region
state_df <- data.frame(
  State = state.name,
  Area = state.area,
  Region = state.region
)

# Viewing the state dataframe
print(state_df)

# Create a dataframe with the number of events per state
events_per_state <- count(filtered_data, state)

# Merge the event count dataframe with the state information dataframe
merged_df <- merge(state_df, events_per_state, by.x = "State", by.y = "state", all.x = TRUE)

# Remove states that are not in the state information dataframe
merged_df <- merged_df[complete.cases(merged_df), ]

# Viewing the merged dataframe with the number of events per state
print(merged_df)

library(ggplot2)

# Create the scatterplot with inverted axes
scatterplot <- ggplot(merged_df, aes(x = Area, y = n, color = Region)) +
  geom_point() +
  labs(x = "Land Area (sq. miles)", y = "Number of Storm Events", title = "Storm Events vs. Land Area by Region") +
  scale_color_discrete(name = "Region") +
  theme_minimal()

# Display the scatterplot
print(scatterplot)
