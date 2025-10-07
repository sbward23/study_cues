### Functions ###
library(dplyr)
library(tidyr)

# Function 1: Read and Clean Raw Data
read_and_clean_data <- function(raw_data_path) {
  library(dplyr)
  library(readr)
  
  d_raw <- read_csv(raw_data_path) %>% clean_names()
  d_raw <- d_raw[-c(1, 2), ]
  d_raw$recorded_date <- as.Date(d_raw$recorded_date)
  return(d_raw)
}

# Function 2: Add Sona ID
add_sona_id <- function(d_raw, pid_data_path) {
  library(dplyr)
  library(readxl)
  
  pid_data <- read_excel(pid_data_path) %>% clean_names()
  with_sona <- merge(d_raw, pid_data, by = "pid", all.x = TRUE)
  with_sona <- with_sona %>% select(sona_id, recorded_date, everything())
  return(with_sona)
}

# Function 3: Create Identifiers and Remove Duplicates
create_identifiers_and_remove_duplicates <- function(with_sona) {
  library(dplyr)
  
  with_sona$id_day <- paste(with_sona$sona_id, with_sona$day, sep = "-")
  with_sona <- with_sona %>% select(sona_id, recorded_date, id_day, everything())
  with_sona$id_date <- paste(with_sona$sona_id, with_sona$recorded_date, sep = "_")
  with_sona <- with_sona %>% select(id_date, id_day, progress, finished, au_db1, psb1, everything())
  
  check <- with_sona %>% filter(progress != 0)
  check <- check %>% group_by(id_date) %>% slice_max(order_by = progress)
  check <- check %>% mutate(au_or_psb = ifelse(au_db1 == 1 | psb1 == 1, 1, 0)) %>% 
    select(id_date, progress, au_db1, psb1, au_or_psb, everything())
  check <- check %>% group_by(id_date) %>% slice_max(order_by = au_or_psb)
  ww <- check %>% group_by(id_date) %>% filter(n() > 1)
  check <- anti_join(check, ww, by = "id_date")
  return(check)
}

# Function 4: Create Day Order Identifier
create_day_order_identifier <- function(check) {
  library(dplyr)
  
  sorted_data <- check %>%
    group_by(sona_id) %>%
    arrange(recorded_date) %>%
    mutate(days_order = row_number()) %>%
    ungroup()
  
  sorted_data <- sorted_data %>%
    select(id_date, days_order, everything())
  
  sorted_data$id_day_order <- paste(sorted_data$sona_id, sorted_data$days_order, sep = "-")
  
  sorted_data <- sorted_data %>%
    select(id_date, days_order, id_day_order, everything())
  return(sorted_data)
}

# Function 5: Remove Columns and Rows Based on Specific Conditions
remove_unnecessary_columns_and_rows <- function(sorted_data) {
  library(dplyr)
  
  single_var_columns <- colnames(sorted_data)[sapply(sorted_data, function(x) length(unique(x)) == 1)]
  columns_to_keep <- !(colnames(sorted_data) %in% single_var_columns)
  cleaned_data <- sorted_data[, columns_to_keep]
  cleaned_data <- cleaned_data %>% filter(sona_id != "PIVOTLABRA")
  cleaned_data <- subset(cleaned_data, select = -c(id_day))
  
  # Remove specific entries
  cleaned_data <- cleaned_data %>% filter(id_date != "lmhuber_2022-11-19", id_date != "vsilvera_2022-11-26")
  
  return(cleaned_data)
}

# Tidy variable names for diary data
rename_variables <- function(dataframe) {
  dataframe <- dataframe %>% 
    rename(
      # Daily Screeners
      aub1 = au_db1,
      osb1 = osbb1,
      ssb1 = ssab1,
      dub1 = drugs_screen_fu,
      bpa = bpa1,
      pub1 = porn_screen_fu,
      
      # Alcohol
      au_t = au_db2,
      au_v = au_db3,
      au_l = au_db4,
      au_loc.txt = au_db4_7_text,
      au_pbs = au_db5,
      au_pbs.txt = au_db5_6_text,
      au_pg = au_db6,
      au_dg = au_db7,
      au_bib = au_db8,
      au_fib = au_db9,
      au_drg = q186,
      
      # Partnered Sexual Behavior 
      cub1 = psb3,
      cub2 = psb4,
      cub3 = psb5,
      cub4 = psb6,
      cub5 = psb8,
      cub6 = psb9,
      psb3 = psb10,
      psb4 = psb10_only_one,
      psb5 = psb11_only_one,
      psb5a = psb11_tried,
      psb6 = psb12_only_one,
      psb6a = psb12_tried,
      str_p = q211,
      psb7 = psb13,
      psb8 = q198_1,
      psb9 = psb14_only_one_1,
      psb7a = psb13_tried,
      psb8a = psb_selfdrunk_tried_1,
      psb9a = psb14_tried_1,
      psb10 = psb15,
      psb10a = psb15_tried,
      psb11 = psb16,
      psb11a = psb16_tried,
      med1_p = pbs17_2,
      med2_p = pbs17_3,
      med3_p = pbs17_4,
      med4_p = pbs17_5,
      aro_pf = psb18_to_23_2,
      pls_pf = psb18_to_23_1,
      con_pf = psb18_to_23_3,
      ver_pc = q141_1,
      enon_pc = q141_2,
      inon_pc = q141_3,
      prp_v = pbs28_31_1,
      prp_p = pbs28_31_2,
      prp_sf = pbs28_31_3,
      vic_v = psb24_to_27_1,
      vic_p = psb24_to_27_2,
      vic_sf = psb24_to_27_3,
      vic_in = psb24_to_27_4,
      exp_p = psb32_1,
      st_p = psb33_1,
      exci_p = psb34_1_to_34_5_1, #excited
      angr_p = psb34_1_to_34_5_2, #angry
      dist_p = psb34_1_to_34_5_3, #distressed
      irri_p = psb34_1_to_34_5_4, #irritable 
      empt_p = psb34_1_to_34_5_5, #empty
      asha_p = psb34_1_to_34_5_6, #ashamed 
      calm_p = psb34_1_to_34_5_7,  #calm
      er1_p = psb34,
      er2_p = psb35,
      er3_p = psb36,
      er4_p = psb37,
      
      # Online Sexual Behavior
      osb2 = osbb2,
      osb3 = osbb3,
      osb4 = osbb4,
      osb5 = osbb5,
      osb6 = osbb6,
      osb7 = osbb7,
      osb8 = osbb8,
      osb9 = osbb9,
      osb10 = osbb10_11,
      aro_of = osbb12_to_13_1,
      pls_of = osbb12_to_13_2,
      pss = osbb14,
      exp_o = osbb16_1,
      str_o = osbb17_1,
      pa1_o = osbb18_1_to_5_1,
      na1_o = osbb18_1_to_5_2,
      na2_o = osbb18_1_to_5_3,
      na3_o = osbb18_1_to_5_4,
      na4_o = osbb18_1_to_5_5,
      pa2_o = osbb18_1_to_5_6,
      na5_o = osbb18_1_to_5_7,
      er1_o = osbb19,
      er2_o = osbb20,
      er3_o = osbb21,
      er4_o = osbb22,
      
      # Solo Sexual Behavior
      ssb2 = ssab2,
      ssb3 = ssab3,
      ssb4 = ssab4,
      ssb5 = ssab5,
      ssb6 = ssab6_7,
      aro_sf = ssab8_to_9_1,
      pls_sf = ssab8_to_9_2,
      exp_s = ssab10_1,
      str_s = ssab11_1,
      pa1_s = ssab12_1_to_5_1,
      na1_s = ssab12_1_to_5_2,
      na2_s = ssab12_1_to_5_3,
      na3_s = ssab12_1_to_5_4,
      na4_s = ssab12_1_to_5_5,
      na5_s = ssab12_1_to_5_6,
      pa2_s = ssab12_1_to_5_7,
      er1_s = ssab13,
      er2_s = ssab14,
      er3_s = ssab15,
      er4_s = ssab16,
      
      # Drug Use Behavior
      dub2 = q215,
      dub3_bf = q202,
      dub3_du = q208,
      dub3_af = q210,
      med1_d = q212_1,
      med2_d = q212_2,
      med3_d = q212_3,
      med4_d = q212_4,
      exp_d = q195,
      str_d = q196_1,
      dub4 = q179,
      dub5 = q180,
      dub6 = q181,
      
      # General Mental Health 
      imp1 = mis1,
      imp2 = mis2,
      imp3 = mis3,
      imp4 = mis4,
      dep1 = phq8_1_fu,
      dep2 = phq8_2_fu,
      insom = phq8_3_fu,
      fatig = phq8_4_fu,
      anx1 = gad_fu_nervous,
      anx2 = gad_fu_notabletostop,
      traum1 = b1,
      traum2 = c2,
      traum3 = d2,
      traum4 = e4
    ) %>%
    rename(
      # Open-text entry
      dub2.txt = q215_8_text,
      dub3_bf.txt = q202_36_text,
      dub3_du.txt = q208_35_text,
      dub3_af.txt = q210_34_text,
      str_p.txt = q211_7_text,
      osb2.txt = osbb2_13_text,
      prp_in = pbs28_31_4,
      osb6.txt = osbb6_5_text,
      ssb3.txt = ssab3_8_text
    )
  return(dataframe)
}


# Main Function to Run All Steps (Excluding Export)
clean_diary_data <- function(raw_data_path, pid_data_path) {
  d_raw <- read_and_clean_data(raw_data_path)
  with_sona <- add_sona_id(d_raw, pid_data_path)
  check <- create_identifiers_and_remove_duplicates(with_sona)
  sorted_data <- create_day_order_identifier(check)
  cleaned_data <- remove_unnecessary_columns_and_rows(sorted_data)
  renamed_data <- rename_variables(cleaned_data)
  return(renamed_data)
}

# Separate Function for Export (when needed)
export_cleaned_data <- function(cleaned_data, export_path) {
  write.csv(cleaned_data, file = export_path, row.names = FALSE)
}

# Example Usage
# Paths to the data
# raw_data_path <- "data/diary.raw.csv"
# pid_data_path <- "data/PID_SonaID_match.xlsx"
# export_path <- "data/cleaned_diary_data.csv"

# Run the cleaning process
# cleaned_data <- clean_diary_data(raw_data_path, pid_data_path)

# Optionally, export the cleaned data
# export_cleaned_data(cleaned_data, export_path)

### Counting number of unique participants per event(s) ###

# Define the function
participants_per_event <- function(dataframe, event_var, id_var) {
  
  # Filter the dataset for particular event/variable
  participants_per_event <- dataframe %>%
    filter(!!sym(event_var) >= 1) %>% # Use sym and !! to refer to the column names
    distinct(!!sym(id_var)) %>%  # Extract unique participant IDs
    nrow()
  
  return(participants_per_event)
}

# Example usage
# unique_count <- participants_per_event(d, "psb_alc", "sona_id")

# Define the function
pts_per_co_events <- function(dataframe, event_var1, event_var2, id_var) {
  library(dplyr)
  
  # Filter the dataset for both events occurring on the same day
  events <- dataframe %>%
    filter(!!sym(event_var1) == 1, !!sym(event_var2) == 1) %>% 
    distinct(!!sym(id_var)) # Extract unique participant IDs
  
  # Count the number of unique participants
  unique_participants_count <- n_distinct(events[[id_var]])
  
  return(unique_participants_count)
}

# Example usage
# unique_count <- pts_per_co_events(d, "aub1", "psb1", "sona_id")

### Transform multi-select character variables into multiple binary variables ###
library(dplyr)
library(tidyr)
library(stringr)
library(rlang)  # Ensure this is loaded for symbol handling

# General function to recode multi-select character variables into binary variables
multi_select_vars <- function(df, column_name, prefix) {
  col_sym <- ensym(column_name)  # Convert column_name to a symbol
  
  # Expand the comma-separated values into multiple rows
  df <- df %>%
    mutate(across(all_of(as_string(col_sym)), ~strsplit(as.character(.), ","))) %>%
    unnest(c(!!col_sym)) %>%
    mutate(!!col_sym := as.integer(!!col_sym))
  
  # Get the list of all unique codes
  unique_codes <- sort(unique(na.omit(df[[as_string(col_sym)]])))
  
  # Create binary columns for each unique code
  for (code in unique_codes) {
    col_name <- paste0(prefix, code)
    df <- df %>%
      mutate(!!col_name := ifelse(!!col_sym == code, 1, 0))
  }
  
  # Summarize back to the original rows
  df <- df %>%
    group_by(across(-!!col_sym)) %>%
    summarize(across(starts_with(prefix), ~ max(.x, na.rm = TRUE)), .groups = 'drop')
  
  return(df)
}

### Rename binary variables created from multi-select "alcohol use location" variable ###

rename_loc <- function(dataframe) {
  dataframe <- dataframe %>% 
    rename(
      loc_bar = loc_6,
      loc_dorm = loc_1,
      loc_frat = loc_3,
      loc_home = loc_8,
      loc_else = loc_7,
      loc_prty = loc_4,
      loc_frnd = loc_2,
      loc_tlg = loc_5
    )
  return(dataframe)
}

# Function to recode protective behavioral strategies based on survey responses
recode_pbs <- function(df, column_name) {
  col_sym <- ensym(column_name)  # Convert column_name to a symbol
  
  # Expand the comma-separated values into multiple rows
  df <- df %>%
    mutate(across(all_of(as_string(col_sym)), ~strsplit(as.character(.), ","))) %>%
    unnest(c(!!col_sym)) %>%
    mutate(!!col_sym := as.integer(!!col_sym))
  
  # Create binary columns for each unique strategy
  df <- df %>%
    pivot_wider(
      names_from = !!col_sym, 
      values_from = !!col_sym,
      names_prefix = "pbs_",  # Change prefix to pbs for protective behavioral strategies
      values_fn = length, 
      values_fill = list(value = 0)  # Fill missing cases with 0
    )
  
  return(df)
}

### Rename binary variables created from multi-select "protective behavioral strategies" variable ###
rename_pbs <- function(dataframe) {
  dataframe <- dataframe %>% 
    rename(
      pbs_pred = pbs_1,
      pbs_coun = pbs_2,
      pbs_frnd = pbs_3,
      pbs_alt = pbs_4,
      pbs_typ = pbs_5,
      pbs_oth = pbs_6,
      pbs_none = `pbs_-9`,
      # Add more renames as needed for each strategy
    )
  return(dataframe)
}

