# Organize data to run a model with two years for extinction 
# Load libraries
library(dplyr)
library(tidyr)
library(lubridate)

# Format detection data for SSOM 
load("0_data/raw/BTNWSurveys&Covs.Rdata")

load("C:/Users/hartt/Documents/Chapter 2/Choosing new BTNW sites/From Elly/ABbirddataset.Rdata")
# Load the dataset
visits <- read.csv("0_data/raw/visits_allAB_unfiltered.csv") 

# Convert BTNW column to binary detections/nondetections
visits <- visits %>% 
  mutate(BTNW_binary = ifelse(BTNW > 0, 1, 0))

visits <-read.csv("1_Data/multiyearvisits_bufferedptsremoved.csv")
# Filter for locations with 3 or more visits per gisid and year, sampling exactly 3 visits per year
filtered_visits <- visits %>%
  group_by(gisid, year) %>%
  filter(n() >= 3) %>%
  slice_sample(n = 3) %>%
  ungroup()

# Group by coordinates and filter for sites with two or more distinct years
multi_year_visits <- filtered_visits %>%
  group_by(Easting, Northing) %>%
  filter(n_distinct(year) >= 2) %>%
  ungroup() 

# Sample exactly two years for each site with more than two years
multi_year_visits <- multi_year_visits %>%
  group_by(Easting, Northing) %>%
  filter(year %in% sample(unique(year), 2)) %>%
  ungroup()

# Create a unique identifier for each site based on coordinates
multi_year_visits <- multi_year_visits %>%
  mutate(gisid = paste(Easting, Northing, sep = "_"))

# Add a unique identifier for the year (1 for the first year, 2 for the second)
multi_year_visits <- multi_year_visits %>%
  arrange(gisid, year) %>%
  group_by(gisid) %>%
  mutate(
    year_id = dense_rank(year)          # Assign year identifier: 1 for first year, 2 for second
  ) %>%
  ungroup()

# Extract year, latitude, and longitude values for each gisid
site_info <- multi_year_visits %>%
  group_by(gisid, year_id) %>%
  summarize(
    year = first(year),
    Easting = first(Easting),
    Northing = first(Northing),
    latitude = first(latitude),    # Explicitly pull latitude
    longitude = first(longitude),  # Explicitly pull longitude
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = year_id, values_from = year, names_prefix = "year_")

# Add a visit number within each year, ensuring only three visits per year
multi_year_visits <- multi_year_visits %>%
  group_by(gisid, year) %>%
  arrange(date_time) %>%
  mutate(
    visit_num = row_number(),           # Visit number within the year
    visit_num = ifelse(visit_num > 3, NA, visit_num) # Ensure only three visits per year
  ) %>%
  filter(!is.na(visit_num)) %>%         # Remove any visits beyond the third visit per year
  mutate(
    visit_id = paste(year_id, visit_num, sep = "_")  # Create the combined visit identifier
  ) %>%
  ungroup()

# Pivot to create a matrix where each row is a site and columns are the visits per year
visit_matrix <- multi_year_visits %>%
  pivot_wider(
    id_cols = gisid,                    # Each row represents a unique site
    names_from = visit_id,              # Column names are the visit identifiers
    values_from = BTNW_binary           # Detection data as the values, keeping NAs for missing visits
  )

# Combine the year and location information with the visit matrix
visit_matrix <- visit_matrix %>%
  left_join(site_info, by = "gisid")

# Print the resulting visit matrix with year and location information
print(visit_matrix)

write.csv(visit_matrix, "1_Data/2yearvisitmatrix.csv")



# Filter for visits that have BTNW habitat after extracting patch metrics 
# use the processed habitat metrics data from patch extraction outputs folder to match the sites with the visit matrix 
habitat_metrics <- read.csv("~/Chapter 2/Chapter 2 Analysis/Extract_patch_metrics/2_outputs/habitat_metrics_hyp1.csv")
multi_year_visits <- read.csv("1_Data/multiyearvisits_bufferedptsremoved.csv")

# Match surveyid to gisid and retrieve corresponding gisid (site) names
# Filter habitat_metrics to retain only relevant surveyid entries
# This will add the location name alongside each matched gisid
matched_sites <- habitat_metrics %>%
  select(surveyid) %>%
  distinct() %>%
  left_join(multi_year_visits, by = "surveyid") %>%
  select(gisid) %>%  # Replace "location_name" with the correct column name if different
  distinct()


# Filter the visit matrix to only include sites in habitat metrics
filtered_visits <- multi_year_visits %>%
  filter(gisid %in% matched_sites$gisid)

# save this so you can use to get detection covariates 
write.csv(filtered_visits, "1_Data/visitsfordetcovs.csv")

# Extract year, latitude, and longitude values for each gisid
site_info <- filtered_visits %>%
  group_by(gisid, year_id) %>%
  summarize(
    year = first(year),
    Easting = first(Easting),
    Northing = first(Northing),
    latitude = first(latitude),    # Explicitly pull latitude
    longitude = first(longitude),  # Explicitly pull longitude
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = year_id, values_from = year, names_prefix = "year_")


# Pivot to create a matrix where each row is a site and columns are the visits per year
visit_matrix <- filtered_visits %>%
  pivot_wider(
    id_cols = gisid,                    # Each row represents a unique site
    names_from = visit_id,              # Column names are the visit identifiers
    values_from = BTNW_binary           # Detection data as the values, keeping NAs for missing visits
  )


# Combine the year and location information with the visit matrix
visit_matrix <- visit_matrix %>%
  left_join(site_info, by = "gisid")

# Save the detection matrix
write.csv(visit_matrix, "1_Data/2yearvisitmatrix.csv", row.names = FALSE)



# get all relevant info and make a csv with site info 
names(multi_year_visits)
# Summarize site information by 'gisid' only and get the first year of data collection
site_info <- filtered_visits %>%
  group_by(gisid) %>%
  summarize(
    Easting = first(Easting),
    Northing = first(Northing),
    latitude = first(latitude),
    longitude = first(longitude),
    surveyid = first(surveyid),
    BTNW = first(BTNW),
    offset = first(offset),
    project_id = first(project_id),
    location = first(location),
    source = first(source),
    organization = first(organization),
    sensor = first(sensor),
    buffer = first(buffer),
    date_time = first(date_time),
    task_method = first(task_method),
    duration = first(duration),
    distance = first(distance),
    BTNW_binary = first(BTNW_binary),
    visit_num = first(visit_num),
    visit_id = first(visit_id),
    # Get the first and second year of data collection for each gisid
    year_1 = first(year),
    .groups = 'drop'
  )

# Write the summarized site info to a CSV file
write.csv(site_info, "1_Data/locations.csv", row.names = FALSE)











# By rounding lat and lon - i didnt use this method because it ddont chnage much and just complicated things 
# Round latitude and longitude to 4 decimal places to capture sites within ~20 meters
visits <- visits %>%
  mutate(
    lat_round = round(latitude, 4),
    lon_round = round(longitude, 4)
  )

# Filter for locations with 3 or more visits per rounded gisid and year, sampling exactly 3 visits per year
filtered_visits <- visits %>%
  group_by(lat_round, lon_round, year) %>%
  filter(n() >= 3) %>%
  slice_sample(n = 3) %>%
  ungroup()

# Group by rounded coordinates and filter for sites with two or more distinct years
multi_year_visits <- filtered_visits %>%
  group_by(lat_round, lon_round) %>%
  filter(n_distinct(year) >= 2) %>%
  ungroup()

# Sample exactly two years for each rounded site with more than two years
multi_year_visits <- multi_year_visits %>%
  group_by(lat_round, lon_round) %>%
  filter(year %in% sample(unique(year), 2)) %>%
  ungroup()

# Create a unique identifier for each site based on rounded coordinates
multi_year_visits <- multi_year_visits %>%
  mutate(gisid = paste(lat_round, lon_round, sep = "_"))

# Recalculate the year-specific columns with only two years per `gisid`
# Also use distinct and values_fn to handle duplicate years
year_info <- multi_year_visits %>%
  group_by(gisid) %>%
  distinct(gisid, year, .keep_all = TRUE) %>%
  mutate(year_id = dense_rank(year)) %>%
  filter(year_id <= 2) %>%
  pivot_wider(names_from = year_id, values_from = year, names_prefix = "year_") %>%
  ungroup()

# Add year_id back into multi_year_visits by joining with year_info
multi_year_visits <- multi_year_visits %>%
  left_join(year_info %>% select(gisid, year_1, year_2), by = "gisid") %>%
  mutate(year_id = case_when(
    year == year_1 ~ 1,
    year == year_2 ~ 2,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(year_id))

# Add a visit number within each year, ensuring only three visits per year
multi_year_visits <- multi_year_visits %>%
  group_by(gisid, year) %>%
  arrange(date_time) %>%
  mutate(
    visit_num = row_number(),           # Visit number within the year
    visit_num = ifelse(visit_num > 3, NA, visit_num) # Ensure only three visits per year
  ) %>%
  filter(!is.na(visit_num)) %>%         # Remove any visits beyond the third visit per year
  mutate(
    visit_id = paste(year_id, visit_num, sep = "_")  # Create the combined visit identifier
  ) %>%
  ungroup()

multi_year_visits <- multi_year_visits %>%
  group_by(gisid, year)

# Pivot to create a matrix where each row is a site and columns are the visits per year
visit_matrix <- multi_year_visits %>%
  pivot_wider(
    id_cols = gisid,                    # Each row represents a unique site
    names_from = visit_id,              # Column names are the visit identifiers
    values_from = BTNW_binary           # Detection data as the values
  )

# Combine the year information with the visit matrix
visit_matrix <- visit_matrix %>%
  left_join(year_info, by = "gisid")

# Print the resulting visit matrix with year and location information
print(visit_matrix)
