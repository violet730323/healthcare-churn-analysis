# ==============================================================================
# LACKEY CLINIC CHURN PREDICTION - Final CODE
# ==============================================================================
# Team: Team 2
# Date: December 2025
# ==============================================================================

# ==============================================================================
# 1. SETUP & LIBRARIES
# ==============================================================================



rm(list=ls())

# Check for all required packages
packages <- c(
  "tidyverse", "lubridate", "dplyr", "tools", "caret", "car",
  "lmtest", "ResourceSelection", "pROC", "brglm2",
  "zipcodeR", "geosphere", "lubridate", "smotefamily", 'remotes',
  'glmnet', 'themis'
)

# Check for any missing packages
install_if_missing <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg)
  }
}

# Install missing packages
invisible(lapply(packages, install_if_missing))


# Should be able to call all libraries
remotes::install_github("vcastro/CCS")
library(tidyverse)
library(lubridate)
library(dplyr)
library(tools)
library(caret)
library(car)
library(lmtest)
library(ResourceSelection)
library(pROC)
library(brglm2)
library(zipcodeR)
library(geosphere)
library(lubridate)
library(smotefamily)
library(CCS)


# for reproducibility
set.seed(123) 




# Set options for cleaner output
options(scipen = 999)



# ==============================================================================
# 1. LOAD DATA
# ==============================================================================
# Csv files must be in same folder as R file

people <- read_csv("people_updated.csv", show_col_types = FALSE)
encounters <- read_csv("encounters.csv", show_col_types = FALSE)
enrollment <- read_csv("enrollment.csv", show_col_types = FALSE)
noshow <- read_csv("noshow.csv", show_col_types = FALSE)
observations <- read_csv("observations.csv", show_col_types = FALSE)
diagnosis <- read_csv("diagnosis.csv", show_col_types = FALSE)
orders <- read_csv("orders.csv", show_col_types = FALSE)
dental <- read_csv("dental.csv", show_col_types = FALSE)      # Recommend ignoring
residence <- read_csv("residence.csv", show_col_types = FALSE)

# ==============================================================================
# 1B. SET REFERENCE DATE FOR REPRODUCIBILITY
# ==============================================================================
# CRITICAL: Use fixed date (when data was received), NOT Sys.Date()


reference_date <- as.Date("2025-09-30")  # End of September 2025




# ==============================================================================
# Data Clean: People
# ==============================================================================

##Clean People dataset to not include the null values. This is simply to make it easier to work with. Filtering criteria can be changed later.

cleaned_people <- people %>%
  mutate(across(where(is.character), str_to_lower)) %>%
  filter(
    complete.cases(.),
    str_to_lower(Language) != "unknown",
    str_to_lower(Race) != "not provided",
    str_to_lower(Language) != "null",
    str_to_lower(Race) != "null",
    Employment != "null",
    str_to_lower(Employment) != "unknown",
    Education != "null",
    Education != "unknown",
    Housing != "null",
    Housing != "unknown",
    Gender != "null",
    Status != "null",
    Status != "unknown",
    Status != "not provided"
  ) %>%
  mutate(
    Language = case_when(
      Language == "english" ~ "english",
      Language == "spanish" ~ "spanish",
      TRUE ~ "other"
    ),
    Employment = str_replace_all(Employment, "]", ")")  
  )



##Cleaned Data (make sure all mrns are distinct)
cleaned_people <-  cleaned_people %>%
  distinct(mrn_pseudo, .keep_all = TRUE)





# ==================Status# ==============================================================================
# 2. DEFINE TWO MEASURES OF CHURN
# ==============================================================================


# ==================
# FIRST MEASURE
# ==================

# APPROACH: Encounter-Based Churn (no encounter in > 6 months)
# Find date column dynamically
enc_date_col <- names(encounters)[grepl("date|datetime|time", names(encounters), ignore.case = TRUE)]



if (length(enc_date_col) > 0 && inherits(encounters[[enc_date_col[1]]], c("Date", "POSIXct", "POSIXt"))) {
  enc_date_col <- enc_date_col[1]
  churn_encounter <- encounters %>%
    group_by(mrn_pseudo) %>%
    summarize(
      last_encounter = max(.data[[enc_date_col]], na.rm = TRUE),
      days_since = as.numeric(reference_date - last_encounter),
      .groups = "drop"
    ) %>%
    mutate(churned = if_else(days_since > 182, 1, 0))
}

# ==================
# SECOND MEASURE
# ==================

# APPROACH: Distinguish between Bad Churn and Good Churn
# Encounter-Based Churn (no encounter in > 6 months) as well as bad enrollment status (not enrolled ini Medicaid, Medicare, etc)

# Identify those who have bad churn
enrollment_bad_churn <- enrollment %>% 
  filter(!(enr_status == 'Approved Financials' | enr_status == 'Expired -  Income' | 
             enr_status == 'Expired -  Insurance' | enr_status == 'Expired -  Medicaid' | 
             enr_status == 'Expired -  Medicare' | enr_status == 'Expired -  Not in US enough' | enr_status == 'Expired -  Pregnant' | 
             enr_status == 'Ineligible -  Income' | enr_status == 'Ineligible -  Insurance' | 
             enr_status == 'Ineligible -  Medicaid' | enr_status == 'Ineligible -  Medicare' |
             enr_status == 'Ineligible -  Not in the US enough')) %>% 
  filter(!(enr_unenr_reason == 'Death' | enr_unenr_reason == 'Income' | enr_unenr_reason == 'Medicaid' |
             enr_unenr_reason == 'Medicare'))



## Store the IDs
bad_id_enroll <- unique(enrollment_bad_churn$mrn_pseudo)

cat(sprintf("Patients with potential churn issues problems: %s (%.1f%% of all patients)\n",
            format(n_distinct(enrollment_bad_churn$mrn_pseudo), big.mark = ","),
            (n_distinct(enrollment_bad_churn$mrn_pseudo) / nrow(cleaned_people)) * 100))

# ==============================================================================
# 3. CREATE PATIENT-LEVEL SUMMARIES
# ==============================================================================

# Enrollment summary (use if() instead of if_else() to avoid warnings)
if ("enr_start" %in% names(enrollment_bad_churn) && "enr_exp" %in% names(enrollment_bad_churn)) {
  enrollment_summary <- enrollment %>%
    filter(enr_start <= enr_exp | is.na(enr_start) | is.na(enr_exp)) %>%
    group_by(mrn_pseudo) %>%
    summarize(
      total_enrollments = n(),
      first_enrollment = if(all(is.na(enr_start))) as.Date(NA_character_) else min(enr_start, na.rm = TRUE),
      last_enrollment_end = if(all(is.na(enr_exp))) as.Date(NA_character_) else max(enr_exp, na.rm = TRUE),
      total_enrollment_days = sum(as.numeric(enr_exp - enr_start), na.rm = TRUE),
      currently_enrolled = if(all(is.na(enr_exp))) FALSE else max(enr_exp, na.rm = TRUE) >= reference_date,
      .groups = "drop"
    )
}

# Encounters summary (handles different date column names)
enc_date_col <- names(encounters)[grepl("date|datetime|time", names(encounters), ignore.case = TRUE)]

if (length(enc_date_col) > 0 && inherits(encounters[[enc_date_col[1]]], c("Date", "POSIXct", "POSIXt"))) {
  enc_date_col <- enc_date_col[1]
  
  # Check for telehealth column (handle different naming conventions)
  if ("telehealth" %in% names(encounters)) {
    # telehealth = "Yes"/"No"
    encounters_summary <- encounters %>%
      group_by(mrn_pseudo) %>%
      summarize(
        total_encounters = n(),
        telehealth_count = sum(telehealth == "Yes", na.rm = TRUE),
        telehealth_pct = mean(telehealth == "Yes", na.rm = TRUE) * 100,
        first_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else min(.data[[enc_date_col]], na.rm = TRUE),
        last_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else max(.data[[enc_date_col]], na.rm = TRUE),
        days_since_last_visit = if(all(is.na(.data[[enc_date_col]]))) NA_real_ else as.numeric(reference_date - max(.data[[enc_date_col]], na.rm = TRUE)),
        .groups = "drop"
      )
  } else if ("enc_is_teleh" %in% names(encounters)) {
    # enc_is_teleh = 0/1
    encounters_summary <- encounters %>%
      group_by(mrn_pseudo) %>%
      summarize(
        total_encounters = n(),
        telehealth_count = sum(enc_is_teleh == 1, na.rm = TRUE),
        telehealth_pct = mean(enc_is_teleh == 1, na.rm = TRUE) * 100,
        first_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else min(.data[[enc_date_col]], na.rm = TRUE),
        last_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else max(.data[[enc_date_col]], na.rm = TRUE),
        days_since_last_visit = if(all(is.na(.data[[enc_date_col]]))) NA_real_ else as.numeric(reference_date - max(.data[[enc_date_col]], na.rm = TRUE)),
        .groups = "drop"
      )
  } else {
    # No telehealth column
    encounters_summary <- encounters %>%
      group_by(mrn_pseudo) %>%
      summarize(
        total_encounters = n(),
        first_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else min(.data[[enc_date_col]], na.rm = TRUE),
        last_visit = if(all(is.na(.data[[enc_date_col]]))) as.Date(NA_character_) else max(.data[[enc_date_col]], na.rm = TRUE),
        days_since_last_visit = if(all(is.na(.data[[enc_date_col]]))) NA_real_ else as.numeric(reference_date - max(.data[[enc_date_col]], na.rm = TRUE)),
        .groups = "drop"
      )
  }
}

# No-shows summary
noshow_summary <- noshow %>%
  group_by(mrn_pseudo) %>%
  summarize(
    total_noshows = n(),
    .groups = "drop"
  )

# ==============================================================================
# 4. BUILD MASTER DATASET (ONE ROW PER PATIENT)
# ==============================================================================
master <- people %>%
  # Join summaries
  left_join(enrollment_summary, by = "mrn_pseudo") %>%
  left_join(encounters_summary, by = "mrn_pseudo") %>%
  left_join(noshow_summary, by = "mrn_pseudo") %>%
  # Create binary flags for sparse data
  mutate(
    has_observations = mrn_pseudo %in% observations$mrn_pseudo,
    has_diagnosis = mrn_pseudo %in% diagnosis$mrn_pseudo,
    has_noshows = !is.na(total_noshows)
  )

# Save raw version for missing data analysis
master_raw <- master

# Add chosen churn definition
master <- master %>% left_join(churn_encounter, by = "mrn_pseudo")

# ==============================================================================
# 5. MISSING DATA ANALYSIS & HANDLING
# ==============================================================================

cat("\n=== MISSING DATA SUMMARY (RAW DATA) ===\n")

# Calculate % missing for key variables (use RAW data)
missing_summary <- master_raw %>%
  summarize(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = (n_missing / nrow(master_raw)) * 100) %>%
  filter(pct_missing > 0) %>%
  arrange(desc(pct_missing))

cat(sprintf("Variables with missing data: %d\n", nrow(missing_summary)))
if (nrow(missing_summary) > 0) {
  cat(sprintf("Most missing: %s (%.1f%%)\n\n",
              missing_summary$variable[1],
              missing_summary$pct_missing[1]))
}

# STRATEGY: Domain-specific defaults
# For Lackey data, most NAs are structural (patient never had that event)

master <- master %>%
  mutate(
    # Counts: NA means zero occurrences
    total_encounters = replace_na(total_encounters, 0),
    total_noshows = replace_na(total_noshows, 0),
    total_enrollments = replace_na(total_enrollments, 0),
    
    # Rates/percentages: NA means 0% (or could keep as NA)
    telehealth_pct = replace_na(telehealth_pct, 0),
    
    # Time features: NA means "never happened" - use sentinel value
    days_since_last_visit = replace_na(days_since_last_visit, 9999),
    
    # Create indicators for structural missingness (informative!)
    never_visited = is.na(first_visit),
    never_enrolled = is.na(first_enrollment)
  )

cat("✓ Applied domain-specific missing data handling\n")
cat("  • Counts → 0 (no events recorded)\n")
cat("  • Rates → 0% (no activity)\n")
cat("  • Time features → 9999 days (sentinel value for 'never')\n")
cat("  • Created 'never_*' indicators\n\n")

# Validation: Check remaining NAs
remaining_na <- master %>%
  summarize(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  filter(value > 0)

if (nrow(remaining_na) > 0) {
  cat(sprintf("Note: %d variables still have NAs (dates, demographics)\n",
              nrow(remaining_na)))
  cat("     This is expected - will derive features from dates in Part 6\n")
  cat("     Time features (days_since_last_visit) already handled with sentinel value\n\n")
} else {
  cat("✓ No remaining NAs in dataset\n\n")
}


# ==============================================================================
# 6. FEATURE ENGINEERING
# ==============================================================================

## Build a distance metric

#Coordinates for lackey clinic
lackey_lat  <- 37.24631
lackey_lon <- -76.56082


lackey_coord <- c(lackey_lon, lackey_lat)

#GeoCode the zipcode
zip_data <- geocode_zip(residence$r_zip)

#Some zip codes are NA, so manually input the coordinates.
zip_data <- zip_data %>%
  mutate(
    lat = case_when(
      zipcode == "23090" ~ 37.3401,
      zipcode == "23127" ~ 37.3728,
      zipcode == "23183" ~ 37.3423,
      zipcode == "23694" ~ 37.2327,
      zipcode == "23186" ~ 37.2724,
      zipcode == "23501" ~ 36.8518,
      zipcode == "23506" ~ 36.8500,
      zipcode == "23609" ~ 36.9786,
      zipcode == "23668" ~ 37.0227,
      TRUE ~ lat
    ),
    lng = case_when(
      zipcode == "23090" ~ -76.7520,
      zipcode == "23127" ~ -76.7778,
      zipcode == "23183" ~ -76.5219,
      zipcode == "23694" ~ -76.5474,
      zipcode == '23186' ~ -76.7142,
      zipcode == "23501" ~ -76.2801,
      zipcode == "23506" ~ -76.2900,
      zipcode == "23609" ~ -76.4284,
      zipcode == "23668" ~ -76.3373,
      TRUE ~ lng
    )
  )


#Calc distnace from lackey in miles
zip_data$distance_miles <- distHaversine(
  lackey_coord,
  zip_data[, c("lng", "lat")]
) / 1609.34



residence <- residence %>%
  mutate(r_zip = as.character(r_zip)) %>%       # convert numeric ZIP to character
  left_join(
    zip_data %>% select(zipcode, distance_miles),
    by = c("r_zip" = "zipcode")
  )


#Merge with Master Dataset
residence_last <- residence %>%
  group_by(mrn_pseudo) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Join with master and rename the column
master <- master %>%
  left_join(
    residence_last %>% select(mrn_pseudo, distance_miles),
    by = "mrn_pseudo"
  ) %>% 
  rename(lackey_dist_miles = distance_miles)



#####################################################
###Characterize Disease Type
#####################################################

diagnosis_clean <- diagnosis %>%
  mutate(
    # 1)  Uniformly lowercase
    dia_dxdescr_clean = tolower(dia_dxdescr),
    
    # 2) Remove the spaces before and after
    dia_dxdescr_clean = str_trim(dia_dxdescr_clean),
    
    # 3) Compressing consecutive spaces into a single space
    dia_dxdescr_clean = str_squish(dia_dxdescr_clean)
  ) # <-- Added missing closing parenthesis here




diagnosis_std <- diagnosis_clean %>%
  mutate(
    # 1)  Prefix markers: family history / medical history / screening
    is_history   = str_detect(dia_dxdescr_clean, "^history of |^family history of "),
    is_screening = str_detect(dia_dxdescr_clean, "^screening for "),
    
    # 2) Remove these prefixes to get the "core diagnostic text".
    dx_core = dia_dxdescr_clean %>%
      str_remove("^history of ") %>%
      str_remove("^family history of ") %>%
      str_remove("^screening for "),
    
    # 
    dx_std = case_when(
      # GERD 
      str_detect(dx_core, "gastroesophageal reflux|gerd|acid reflux") ~ "gerd",
      
      # Diabetes
      str_detect(dx_core, "diabetes mellitus \\(dm\\)|\\bdiabetes\\b") ~ "diabetes (unspecified)",
      
      # Tobacco use
      str_detect(dx_core, "tobacco use|smoker|smoking") ~ "tobacco use",
      
      # Depression
      str_detect(dx_core, "depression|major depressive|dysthymia") ~ "depression",
      
      # Sebaceous cyst
      str_detect(dx_core, "sebaceous cyst") ~ "sebaceous cyst",
      
      # Lumbar degenerative disc disease
      str_detect(dx_core, "lumbar degenerative disc disease") ~ "lumbar degenerative disc disease",
      
      # Dental pain
      str_detect(dx_core, "pain, dental|dental pain") ~ "dental pain",
      
      # Allergies
      str_detect(dx_core, "allergies|allergic rhinitis") ~ "allergies",
      
      # Healthcare maintenance 
      str_detect(dx_core, "healthcare maintenance|health maintenance") ~ "healthcare maintenance",
      
      
      
      TRUE ~ NA_character_
    )
  )



diagnosis_labeled <- diagnosis_std %>%
  mutate(
    # Give each record a "Diagnostic Type" label in order of priority
    dx_type = case_when(
      is_history   ~ "history",              # history
      is_screening ~ "screening",           #  # Screening/examination
      
      # ----  chronic ----
      dx_std %in% c(
        "hypertension",
        "type 2 diabetes",
        "type 1 diabetes",
        "diabetes (unspecified)",
        "hyperlipidemia",
        "asthma",
        "copd",
        "chronic kidney disease",
        "gerd",
        "depression",
        "anxiety",
        "chronic pain",
        "lumbar degenerative disc disease",
        "cataract",
        "glaucoma",
        "thyroid disorder",
        "obesity / overweight",
        "tobacco use",
        "alcohol use disorder"
      ) ~ "chronic",
      
      # ----  acute----
      dx_std %in% c(
        "dental pain",
        "back pain",
        "joint pain",
        "sebaceous cyst",
        "allergies"
      ) ~ "acute",
      
      # ----  preventive ----
      dx_std %in% c(
        "healthcare maintenance",
        "annual exam",
        "preventive care"
      ) ~ "preventive",
      
      TRUE ~ NA_character_
    ),
    
    #  0/1 index
    is_chronic   = as.integer(dx_type == "chronic"),
    is_acute     = as.integer(dx_type == "acute"),
    is_preventive = as.integer(dx_type == "preventive")
  )


patient_dx_features <- diagnosis_labeled %>%
  group_by(mrn_pseudo) %>%
  summarise(
    # Total number of diagnostic articles
    n_dx = n(),
    
    # Chronic / Acute / Preventive Diagnosis
    n_chronic_dx    = sum(dx_type == "chronic",    na.rm = TRUE),
    n_acute_dx      = sum(dx_type == "acute",      na.rm = TRUE),
    n_preventive_dx = sum(dx_type == "preventive", na.rm = TRUE)
  ) %>%
  # Presence of chronic/acute illness (0/1)
  mutate(
    has_chronic = as.integer(n_chronic_dx > 0),
    has_acute   = as.integer(n_acute_dx > 0)
  ) %>%
  ungroup()




###Join into the Master Dataset
master <- master %>% 
  left_join(
    patient_dx_features %>%
      select(mrn_pseudo, n_dx, n_chronic_dx, n_acute_dx, n_preventive_dx, has_chronic, has_acute),
    by = "mrn_pseudo"
  ) 


## Treat NA's as 0, as it indicates that the patient does not have it.
master <- master %>%
  mutate(across(c(n_dx, n_chronic_dx, n_acute_dx, n_preventive_dx, has_acute, has_chronic),
                ~ ifelse(is.na(.), 0, .)))



##############
##Find the Top 5 most common diagnosis
diagnosis_n <- diagnosis %>%
  count(dia_dxdescr) %>% arrange(desc(n))
top_5_diagnosis <- diagnosis_n %>% 
  slice_max(n, n=5) %>% 
  pull(dia_dxdescr)

##Find the mrns of those who have the top 5 diagnosis
mrn_top_5 <- diagnosis %>% 
  filter(dia_dxdescr %in% top_5_diagnosis) %>% 
  distinct(mrn_pseudo) %>% 
  pull(mrn_pseudo)


master <- master %>%
  mutate(
    # Transformations (for skewed distributions)
    log_encounters = log(total_encounters + 1),  # +1 handles zeros
    
    # Ratios & Rates (handle division by zero)
    noshow_rate = if_else(total_encounters > 0,
                          total_noshows / (total_noshows + total_encounters),
                          NA_real_),
    
    # Visit frequency (encounters per year of enrollment)
    visit_frequency = if_else(total_enrollment_days > 0,
                              (total_encounters / total_enrollment_days) * 365,
                              NA_real_),
    
    # Temporal features
    days_since_exp = if_else(!is.na(last_enrollment_end),
                             as.numeric(reference_date - last_enrollment_end),
                             NA_real_),
    
    tenure_days = if_else(!is.na(first_enrollment) & !is.na(last_enrollment_end),
                          as.numeric(last_enrollment_end - first_enrollment),
                          NA_real_)
  ) %>% 
  mutate(custom_churn = if_else(mrn_pseudo %in% bad_id_enroll & churned == 1, 1, 0)) %>% 
  
  mutate(no_show_binary = if_else(total_noshows >= 2, 1, 0)) %>% 
  
  mutate(has_diagnosis = if_else(has_diagnosis == TRUE, 1, 0)) %>% 
  
  mutate(problem_diagnosis = if_else(mrn_pseudo %in% mrn_top_5, 1, 0)) %>% 
  
  mutate(telehealth_binary = if_else(telehealth_count > 10, 1, 0)) %>% 
  
  mutate(English_Binary = if_else(Language == "English", 1, 0)) %>% 
  
  mutate(EnrollmentBinary = if_else(total_enrollment_days > 300, 1, 0)) %>% 
  
  mutate(Expired_binary = if_else(days_since_exp > 365, 1, 0))

# ==============================================================================
# CATEGORICAL VARIABLE HANDLING EXAMPLES
# ==============================================================================

# Strategy: Intelligent grouping
master <- master %>%
  mutate(
    language_group = case_when(
      Language == "English" ~ "English",
      Language == "Spanish" ~ "Spanish",
      Language %in% c("Vietnamese", "Chinese", "Korean") ~ "Asian Languages",
      is.na(Language) ~ "Unknown",
      TRUE ~ "Other"
    )
  )



# ==============================================================================
# 7. DATA QUALITY CHECKS
# ==============================================================================

cat("\n=== FINAL DATA QUALITY CHECKS ===\n")

# Check for missing outcome for churned variable
if ("churned" %in% names(master)) {
  cat(sprintf("Missing churn labels: %d (%.1f%%)\n",
              sum(is.na(master$churned)),
              mean(is.na(master$churned)) * 100))

  # Check class balance
  master %>%
    filter(!is.na(churned)) %>%
    count(churned) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    print()
} else {
  cat("⚠️  No churn variable found - remember to add it!\n")
}


# Check for missing outcome for custom_churn variable
if ("custom_churn" %in% names(master)) {
  cat(sprintf("Missing churn labels: %d (%.1f%%)\n",
              sum(is.na(master$custom_churn)),
              mean(is.na(master$custom_churn)) * 100))
  
  # Check class balance
  master %>%
    filter(!is.na(custom_churn)) %>%
    count(custom_churn) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    print()
} else {
  cat("⚠️  No churn variable found - remember to add it!\n")
}

## Filter out those who were never enrolled, as that would not be considered churn
master <- master %>% filter(never_enrolled == FALSE)

## For simplicity, drop na values in churn column. This is mostly for calculation purposes, and can be changed for the actual model
master <- master %>% drop_na(churned)


## Create new tibble that contains only the information about patient id and churn
master_patient <- master %>% 
  group_by(mrn_pseudo) %>% 
  summarise(churned = max(churned, na.rm = TRUE))


# Complete cases comparison
cat(sprintf("\nComplete cases (RAW): %s / %s (%.1f%%)\n",
            format(sum(complete.cases(master_raw)), big.mark = ","),
            format(nrow(master_raw), big.mark = ","),
            (sum(complete.cases(master_raw)) / nrow(master_raw)) * 100))

cat(sprintf("Complete cases (CLEANED): %s / %s (%.1f%%)\n",
            format(sum(complete.cases(master)), big.mark = ","),
            format(nrow(master), big.mark = ","),
            (sum(complete.cases(master)) / nrow(master)) * 100))


#######################################
# FINAL FIX FOR EMPLOYMENT
#######################################


## Get rid of the inconsistent parenthesis, so there are no duplicate entries
master$Employment <- gsub("\\[", "(", master$Employment)
master$Employment <- gsub("\\]", ")", master$Employment)
master$Employment <- factor(master$Employment)


master <- na.omit(master)
master$Employment <- factor(trimws(master$Employment))


#########################################################
## 8. EDA
#########################################################

## No-show rate by churn status
master %>%
  filter(!is.na(churned), !is.na(noshow_rate)) %>%
  ggplot(aes(x = factor(churned), y = noshow_rate)) +
  geom_boxplot() +
  labs(title = "No-Show Rate by Churn Status",
       x = "Churned", y = "No-Show Rate")

## Encounters by churn status
master %>%
  filter(!is.na(churned)) %>%
  ggplot(aes(x = total_encounters, fill = factor(churned))) +
  geom_histogram(position = "dodge", bins = 30) +
  scale_x_log10() +  # Log scale helps with skewed data
  labs(title = "Encounter Distribution by Churn Status")



cat("\n✨ Script complete! Master dataset ready for modeling.\n")
cat(sprintf("📅 Remember: All time calculations use reference_date = %s\n", reference_date))
#########################################################
## 9. PREPARATION FOR THE MODEL
#########################################################


#############################
# 9a. Cross Validation Setup
#############################
##For down sampling, use repeated cv to make sure that there is enough variety in the folds chosen, and so that all data can potentiall be seen
train_control_down <- trainControl(method = "repeatedcv", number = 10, 
                                   repeats = 3, sampling='down',
                              classProbs = TRUE, 
                              savePredictions = "final",
                              summaryFunction = twoClassSummary) 

##Up sampling
train_control_up <- trainControl(method = "cv", number = 10, sampling='up',
                                   classProbs = TRUE, 
                                 savePredictions = "final",
                                   summaryFunction = twoClassSummary)  
##Smote sampling
train_control_smote <- trainControl(method = "cv", number = 10, sampling='smote',
                                    classProbs = TRUE, 
                                    savePredictions = "final",
                                    summaryFunction = twoClassSummary)

##Base model
train_control <- trainControl(method = "cv", number = 10,
                                   classProbs = TRUE, 
                              savePredictions = "final",
                                   summaryFunction = twoClassSummary) 



############################
# 9b. Train/Test Split
############################
##For custom definition
divideData <- createDataPartition(master$custom_churn, p = 0.7, list = FALSE)
## For basic definition
divideData_og <- createDataPartition(master$churned, p=0.7, list = FALSE)


# Create training (70%) and test (30%) sets

# Our custom definition
train <- master[divideData, ]
test  <- master[-divideData, ]


# Encounter only churn definition
train_og <- master[divideData_og, ]
test_og  <- master[-divideData_og, ]

# --- Factor the Data ---
train$custom_churn <- factor(train$custom_churn, 
                             levels = c("0", "1"), 
                             labels = c("No_Churn", "Churn"))

test$custom_churn <- factor(test$custom_churn, 
                            levels = c("0", "1"), 
                            labels = c("No_Churn", "Churn"))

train_og$churned <- factor(train_og$churned, 
                           levels = c(0,1), 
                           labels = c("No_Churn","Churn"))
test_og$churned <- factor(test_og$churned, 
                           levels = c(0,1), 
                           labels = c("No_Churn","Churn"))




####################################################################
# 10. LOGISTIC REGRESSION MODEL
####################################################################

############################
# 10a. Custom Definition
############################

simple_model <- train(custom_churn ~ visit_frequency + 
                        has_chronic + has_acute + no_show_binary + 
                        Employment + telehealth_count + language_group +
                        lackey_dist_miles + EnrollmentBinary + tenure_days + total_encounters,
                      data=train,
                      method = 'glm',
                      family = 'binomial',
                      trControl = train_control,
                      metric = 'ROC')

simple_model_down <- train(custom_churn ~ visit_frequency + 
                             has_chronic + has_acute + no_show_binary + 
                              Employment + language_group + telehealth_count + 
                             lackey_dist_miles + EnrollmentBinary + tenure_days + total_encounters, 
                      data=train,
                      method = 'glmnet',
                      family = 'binomial',
                      preProcess = c("center", "scale", "nzv", "corr"),
                      trControl = train_control_down,
                      metric = 'ROC')


# Get Best model for down sampling
best_down <- simple_model_down$bestTune

down_row <- simple_model_down$results[
  simple_model_down$results$alpha == best_down$alpha &
    simple_model_down$results$lambda == best_down$lambda,
]


simple_model_up <- train(custom_churn ~ visit_frequency + 
                           has_chronic + has_acute + no_show_binary + 
                           Employment + telehealth_count + language_group +
                           lackey_dist_miles + EnrollmentBinary + tenure_days + total_encounters,
                           data=train,
                           method = 'glm',
                           family = 'binomial',
                           trControl = train_control_up,
                           metric = 'ROC')


simple_model_smote <- train(custom_churn ~ visit_frequency + 
                              has_chronic + has_acute + no_show_binary + 
                              Employment + telehealth_count + language_group +
                              lackey_dist_miles + EnrollmentBinary + tenure_days + total_encounters,
                         data=train,
                         method = 'glm',
                         family = 'binomial',
                         trControl = train_control_smote,
                         metric = 'ROC')



##Compare models 
## Uncomment if you wish to see CV comparison
# cv_comparison <- data.frame(
#   Method = c("Baseline", "Down-sample", "Up-sample", "Smote"),
#   ROC   = c(simple_model$results$ROC,
#             down_row$ROC,
#             simple_model_up$results$ROC,
#             simple_model_smote$results$ROC),
#   Sens  = c(simple_model$results$Sens,
#             down_row$Sens,
#             simple_model_up$results$Sens,
#             simple_model_smote$results$Sens),
#   Spec  = c(simple_model$results$Spec,
#             down_row$Spec,
#             simple_model_up$results$Spec,
#             simple_model_smote$results$Spec)
# )
# 
# 
# print(cv_comparison)

## Overall, all models perform similarly, so use down-sampling, as least computationally expensive

####################################
##Find the best Threshold
###################################


# Get the probabilities
probs <- predict(simple_model_down, newdata=test, type = 'prob')[,"Churn"]

# Vector of thresholds to test
thresholds <- seq(.05, .95, by = 0.01)

# Store results
results <- data.frame(threshold = thresholds, sensitivity = NA, specificity = NA, weighted_score = NA)

reference <- factor(test$custom_churn,
                    labels = c("No_Churn", "Churn"))


for (i in seq_along(thresholds)) {
  thr <- thresholds[i]
  
  # create predictions at this threshold
  pred <- factor(ifelse(probs >= thr, "Churn", "No_Churn"),
                 levels = c("No_Churn", "Churn"))
  
  # If only one class is predicted → sens/spec undefined
  if (length(unique(pred)) < 2) {
    next
  }
  
  cm <- confusionMatrix(pred, reference, positive = "Churn")
  
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  acc <- cm$overall['Accuracy']
  
  results$sensitivity[i] <- sens
  results$specificity[i] <- spec
  results$weighted_score[i] <-  .6 * sens + .4 * spec
  # results$weighted_score[i] <-  acc
}

# Find threshold with max weighted score
best_thr <- results$threshold[which.max(results$weighted_score)]




################################
# Calc the matrix
################################

##Use the threshold and test on the test data
final_pred <- ifelse(probs >= best_thr, "Churn", "No_Churn")
final_pred <- factor(final_pred, levels = c("No_Churn", "Churn"))
reference <- factor(test$custom_churn, levels = c("No_Churn", "Churn"))
cm_final <- confusionMatrix(final_pred, reference, positive = "Churn")
cm_final

# Threshold for maxing accuracy:
acc_pred <- ifelse(probs >= .81, "Churn", "No_Churn")
acc_pred <- factor(acc_pred, levels = c("No_Churn", "Churn"))
acc_reference <- factor(test$custom_churn, levels = c("No_Churn", "Churn"))
acc_cm_final <- confusionMatrix(acc_pred, acc_reference, positive = "Churn")
acc_cm_final



# Find ROC

#Extract and display key metrics
cat("\nModel Performance Metrics (Multiple Logistic Model)\n")
cat("---------------------------------------------------\n")
cat("  ROC (AUC):", max(simple_model_down$results$ROC), "\n")
cat("Accuracy:  ", round(cm_final$overall["Accuracy"], 4), "\n")
cat("Sensitivity:  ", round(cm_final$byClass["Sensitivity"], 4), "\n")
cat("Specificity:  ", round(cm_final$byClass["Specificity"], 4), "\n")



# Compare to Naive Baseline:
naive_accuracy <- max(prop.table(table(test$custom_churn)))
cat("\n=== NAIVE BASELINE ===\n")
cat("If we predict 'No Churn' for everyone:\n")
cat("Baseline Accuracy:", naive_accuracy, "%\n")
cat("Our models must beat this to add value.\n")


glm_coefs <- coef(simple_model_down$finalModel, s = simple_model_down$bestTune$lambda)

# 2. Calculate the Odds Ratios (OR) by exponentiating the coefficients
glm_odds <- exp(glm_coefs)

cat("\nTop Predictors of Churn (by absolute coefficient):\n")
cat("--------------------------------------------------\n")

# 3. Create a clean data frame for viewing and sorting
coef_df <- tibble(
  Term = rownames(glm_coefs),
  Coefficient = round(as.numeric(glm_coefs), 4),
  OddsRatio = round(as.numeric(glm_odds), 4)
) %>%
  # Remove the intercept term for clarity
  filter(Term != "(Intercept)") %>%
  # Arrange by the absolute magnitude of the coefficient (largest impact)
  arrange(desc(abs(Coefficient)))

# 4. Print the top 8 most influential predictors
print(head(coef_df, 8))

############################
# 10b. Base Definition
############################

## Same as the custom model

simple_model_og <- train(churned ~ visit_frequency + 
                           has_chronic + has_acute + no_show_binary + 
                           Employment + language_group + telehealth_count + 
                           lackey_dist_miles + EnrollmentBinary + tenure_days + total_encounters,
                         data=train_og,
                         method = 'glm',
                         family = 'binomial', 
                         trControl = train_control,
                         metric='ROC')

simple_model_og_down <- train(churned ~ visit_frequency + 
                                has_chronic + has_acute + no_show_binary + 
                                Employment + language_group + telehealth_count + 
                                lackey_dist_miles + EnrollmentBinary + tenure_days + total_encounters,
                         data=train_og,
                         method = 'glm',
                         family = 'binomial', 
                         trControl = train_control_down,
                         metric='ROC')

simple_model_og_up <- train(churned ~ visit_frequency + 
                              has_chronic + has_acute + no_show_binary + 
                              Employment + language_group + telehealth_count + 
                              lackey_dist_miles + EnrollmentBinary + tenure_days + total_encounters,
                              data=train_og,
                              method = 'glm',
                              family = 'binomial', 
                              trControl = train_control_up,
                              metric='ROC')

simple_model_og_smote <- train(churned ~ visit_frequency + 
                                 has_chronic + has_acute + no_show_binary + 
                                 Employment + language_group + telehealth_count + 
                                 lackey_dist_miles + EnrollmentBinary + tenure_days + total_encounters,
                            data=train_og,
                            method = 'glmnet',
                            family = 'binomial', 
                            trControl = train_control_smote,
                            metric='ROC',
                            preProcess = c("center","scale"))



best_smote <- simple_model_og_smote$bestTune
smote_row <- simple_model_og_smote$results[
  simple_model_og_smote$results$alpha == best_smote$alpha &
    simple_model_og_smote$results$lambda == best_smote$lambda,
]


##Compare models
## Uncomment if you wish to see CV comparison
# cv_comparison <- data.frame(
#   Method = c("Baseline", "Downsample", "Upsample", "SMOTE"),
#   ROC   = c(simple_model_og$results$ROC,
#             simple_model_og_down$results$ROC,
#             simple_model_og_up$results$ROC,
#             smote_row$ROC),
#   Sens  = c(simple_model_og$results$Sens,
#             simple_model_og_down$results$Sens,
#             simple_model_og_up$results$Sens,
#             smote_row$Sens),
#   Spec  = c(simple_model_og$results$Spec,
#             simple_model_og_down$results$Spec,
#             simple_model_og_up$results$Spec,
#             smote_row$Spec)
# )
# 
# print(cv_comparison)





##############################################################
#Find the threshold that maximizes specificity and sensitivity
##############################################################


probs_og <- predict(simple_model_og_smote, newdata=test_og, type = 'prob')[,"Churn"]

thresholds <- seq(.05, .95, by = 0.01)

reference <- factor(test_og$churned, levels = c("No_Churn", "Churn"))

# Store results
results <- data.frame(threshold = thresholds, sensitivity = NA, specificity = NA, weighted_score = NA)



for (i in seq_along(thresholds)) {
  thr <- thresholds[i]
  
  # create predictions at this threshold
  pred <- factor(ifelse(probs_og >= thr, "Churn", "No_Churn"),
                 levels = c("No_Churn", "Churn"))
  
  # If only one class is predicted → sens/spec undefined
  if (length(unique(pred)) < 2) {
    next
  }
  
  cm <- confusionMatrix(pred, reference, positive = "Churn")
  
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  acc <- cm$overall["Accuracy"]
  
  results$sensitivity[i] <- sens
  results$specificity[i] <- spec
  results$weighted_score[i] <- 0.6 * sens + .4 * spec
  results$weighted_score[i] <- acc
  
  }

# Find threshold with max weighted score
best_thr <- results$threshold[which.max(results$weighted_score)]
final_pred <- ifelse(probs_og >= best_thr, "Churn", "No_Churn")
final_pred <- factor(final_pred, levels = c("No_Churn", "Churn"))


cm_final_og <- confusionMatrix(final_pred, reference, positive = "Churn")
cm_final_og



# Extract and display key metrics
cat("\nModel Performance Metrics (Multiple Logistic Model)\n")
cat("---------------------------------------------------\n")
cat("Accuracy:     ", round(cm_final_og$overall["Accuracy"], 4), "\n")
cat(" ROC (AUC):", max(simple_model_og_smote$results$ROC), "\n")
cat("Sensitivity:  ", round(cm_final_og$byClass["Sensitivity"], 4), "\n")
cat("Specificity:  ", round(cm_final_og$byClass["Specificity"], 4), "\n")


naive_accuracy_og <- max(prop.table(table(test_og$churned)))
cat("\n=== NAIVE BASELINE ===\n")
cat("If we predict 'No Churn' for everyone:\n")
cat("Baseline Accuracy:", naive_accuracy_og, "%\n")
cat("Our models must beat this to add value.\n")



glm_coefs_og <- coef(simple_model_og_smote$finalModel, s = simple_model_og_smote$bestTune$lambda)

# 2. Calculate the Odds Ratios (OR) by exponentiating the coefficients
glm_odds_og <- exp(glm_coefs_og)

cat("\nTop Predictors of Churn (by absolute coefficient):\n")
cat("--------------------------------------------------\n")

# 3. Create a clean data frame for viewing and sorting
coef_df_og <- tibble(
  Term = rownames(glm_coefs_og),
  Coefficient = round(as.numeric(glm_coefs_og), 4),
  OddsRatio = round(as.numeric(glm_odds_og), 4)
) %>%
  # Remove the intercept term for clarity
  filter(Term != "(Intercept)") %>%
  # Arrange by the absolute magnitude of the coefficient (largest impact)
  arrange(desc(abs(Coefficient)))

# 4. Print the top 8 most influential predictors
print(head(coef_df_og, 8))

#########################################################################
# 11. KNN MODEL
#########################################################################


############################
# 11a. Original Churn Model
############################


###For KNN, it's basically the same process
train <- master[divideData, ]
test  <- master[-divideData, ]


# Original Encounter Definition
train_og <- master[divideData_og, ]
test_og  <- master[-divideData_og, ]
##Make sure churned is a factor
train_og$churned <- as.factor(train_og$churned)
test_og$churned  <- as.factor(test_og$churned)
train_og$churned <- factor(train_og$churned, levels = c("0","1"))
test_og$churned  <- factor(test_og$churned, levels = c("0","1"))


train_og$churned <- factor(train_og$churned,
                           levels = c("0","1"),
                           labels = c("No_Churn","Churn"))

test_og$churned <- factor(test_og$churned,
                          levels = c("0","1"),
                          labels = c("No_Churn","Churn"))


##Train the KNN model
knnchurned_acc <- train(churned ~ visit_frequency +  has_chronic + has_acute + no_show_binary + 
                           Employment + telehealth_count +
                          lackey_dist_miles + EnrollmentBinary + tenure_days + total_encounters, 
                        data = train_og, 
                        method='knn', 
                        tuneGrid = data.frame(k = c(5, 7, 9, 11, 15)),
                        trControl = train_control,
                        preProcess = c("center","scale", "nzv"),
                        metric='ROC')


## Test on testing model
knn_predictions <- predict(knnchurned_acc, test_og)

KNN_Base_Churn <- confusionMatrix(knn_predictions, test_og$churned, positive = "Churn")


best_knn <- knnchurned_acc$results[knnchurned_acc$results$k == knnchurned_acc$bestTune$k, ]

cat("\n=== K-NEAREST NEIGHBORS (Cross-Validated) ===\n")
cat("Best k:", knnchurned_acc$bestTune$k, "\n")
cat("Cross-Validated Performance (from train):\n")
cat("  ROC (AUC):", round(best_knn$ROC, 3), "\n")
cat("  Sensitivity:", round(best_knn$Sens, 3), "\n")
cat("  Specificity:", round(best_knn$Spec, 3), "\n")

cat("\n=== K-NEAREST NEIGHBORS (Test Set) ===\n")
cat("Test Set Performance (from confusionMatrix):\n")
cat("  Accuracy:", round(KNN_Base_Churn$overall['Accuracy'], 3), "\n")
cat("  Sensitivity:", round(KNN_Base_Churn$byClass['Sensitivity'], 3), "\n")
cat("  Specificity:", round(KNN_Base_Churn$byClass['Specificity'], 3), "\n")
KNN_Base_Churn

############################
# 11b. Custom Churn Model
############################

## Same as Base KNN model
train_clean <- train %>%
  select(custom_churn, Housing, visit_frequency, problem_diagnosis, no_show_binary, 
         Gender, Employment, has_acute, has_chronic, age_range, EnrollmentBinary, tenure_days, language_group,
         total_encounters, lackey_dist_miles, telehealth_count) %>%
  na.omit()

test_clean <- test %>%
  select(custom_churn, Housing, visit_frequency, problem_diagnosis, 
         no_show_binary, Gender, Employment, has_acute, has_chronic, age_range, 
         EnrollmentBinary, tenure_days, total_encounters, lackey_dist_miles, telehealth_count, language_group) %>%
  na.omit()

train_clean$custom_churn <- as.factor(train_clean$custom_churn)
test_clean$custom_churn  <- as.factor(test_clean$custom_churn)


target_levels_names <- c("No_Churn", "Churn")

# 2. Convert and label the target variable in the training data
train_clean$custom_churn <- factor(train_clean$custom_churn, 
                                   levels = c("0", "1"), 
                                   labels = target_levels_names)

# 3. Convert and label the target variable in the testing data
test_clean$custom_churn <- factor(test_clean$custom_churn, 
                                  levels = c("0", "1"), 
                                  labels = target_levels_names)


# Train the model
knnchurned_acc_cust <- train(custom_churn ~ visit_frequency + 
                               has_chronic + has_acute + no_show_binary + 
                               Employment + telehealth_count + 
                                EnrollmentBinary + tenure_days + total_encounters,
                             data = train_clean,
                             method='knn', 
                             preProcess = c("center","scale", "nzv"),
                             trControl = train_control,
                             tuneGrid = data.frame(k = c(5, 7, 9, 11, 15)),
                             metric = "ROC")



preds <- predict(knnchurned_acc_cust, test_clean)

# 2. Get the clean reference factor 
reference <- test_clean$custom_churn 

# 3. Create the final prediction factor, ensuring levels match the reference
# We use levels(reference) to guarantee the levels and order are identical, 
# preventing any subscript or overlap errors.
final_preds <- factor(preds, levels = levels(reference))

# 4. Create and view the Confusion Matrix
# 'positive = "Churn"' is correct, as it is a valid level name.
KNN_custom_churn <- confusionMatrix(data = final_preds, 
                                    reference = reference, 
                                    positive = "Churn")

best_knn_og <- knnchurned_acc_cust$results[knnchurned_acc_cust$results$k == knnchurned_acc_cust$bestTune$k, ]

cat("\n=== K-NEAREST NEIGHBORS (Cross-Validated) ===\n")
cat("Best k:", knnchurned_acc_cust$bestTune$k, "\n")
cat("Cross-Validated Performance (from train):\n")
cat("  ROC (AUC):", round(best_knn_og$ROC, 3), "\n")
cat("  Sensitivity:", round(best_knn_og$Sens, 3), "\n")
cat("  Specificity:", round(best_knn_og$Spec, 3), "\n")

cat("\n=== K-NEAREST NEIGHBORS (Test Set) ===\n")
cat("Test Set Performance (from confusionMatrix):\n")
cat("  Accuracy:", round(KNN_custom_churn$overall['Accuracy'], 3), "\n")
cat("  Sensitivity:", round(KNN_custom_churn$byClass['Sensitivity'], 3), "\n")
cat("  Specificity:", round(KNN_custom_churn$byClass['Specificity'], 3), "\n")


KNN_custom_churn



################################################
# 12. COMPARE MODEL PERFORMANCES
################################################


## Store the accuracy, sensitivity, and specificity for all 4 models

metrics_og <- data.frame(
  Accuracy = cm_final_og$overall["Accuracy"],
  Sensitivity = cm_final_og$byClass["Sensitivity"],
  Specificity = cm_final_og$byClass["Specificity"]
)


metrics <- data.frame(
  Accuracy = cm_final$overall["Accuracy"],
  Sensitivity = cm_final$byClass["Sensitivity"],
  Specificity = cm_final$byClass["Specificity"]
)

metrics_acc <- data.frame(
  Accuracy = acc_cm_final$overall["Accuracy"],
  Sensitivity = acc_cm_final$byClass["Sensitivity"],
  Specificity = acc_cm_final$byClass["Specificity"]
)

metrics_KNN_og <- data.frame(
  Accuracy = KNN_custom_churn$overall["Accuracy"],
  Sensitivity = KNN_custom_churn$byClass["Sensitivity"],
  Specificity = KNN_custom_churn$byClass["Specificity"]
)

metrics_KNN <- data.frame(
  Accuracy = KNN_Base_Churn$overall["Accuracy"],
  Sensitivity = KNN_Base_Churn$byClass["Sensitivity"],
  Specificity = KNN_Base_Churn$byClass["Specificity"]
)


## View accuracy, sensitivity, and specificity for all models
comparison <- rbind(Base_Model = metrics_og, Custom_Model = metrics, Custom_Model_Max_Acc = metrics_acc, Base_KNN = metrics_KNN, Custom_KNN = metrics_KNN_og)

# uncomment to see comparison
# comparison




################################################
# 13. COMPARE TO NAIVE BASELINE
################################################

###################
# 13a. Base Churn
###################

# Naive Baseline
naive_accuracy_og <- max(prop.table(table(test_og$churned)))
cat("\n=== NAIVE BASELINE FOR BASE CHURN ===\n")
cat("If we predict 'Churn' for everyone:\n")
cat("Baseline Accuracy:", naive_accuracy_og, "%\n")
cat("Our models must beat this to add value.\n")

# Logistic Model
log_acc_og <- round(cm_final_og$overall["Accuracy"], 4)
cat("\n=== LOGISTIC MODEL FOR BASE CHURN ===\n")
cat("Accuracy:", log_acc_og, "%\n")
cat("Model Performs ", (log_acc_og - naive_accuracy_og) * 100, "% better than the Naive Baseline")

# KNN Model
KNN_acc_og <- round(KNN_Base_Churn$overall["Accuracy"], 4)
cat("\n=== LOGISTIC MODEL FOR BASE CHURN ===\n")
cat("Accuracy:", KNN_acc_og, "%\n")
cat("Model Performs ", (KNN_acc_og - naive_accuracy_og) * 100, "% better than the Naive Baseline")



###################
# 13b. Custom Churn
###################

# Naive Baseline
naive_accuracy <- max(prop.table(table(test$custom_churn)))
cat("\n=== NAIVE BASELINE FOR CUSTOM CHURN ===\n")
cat("If we predict 'No Churn' for everyone:\n")
cat("Baseline Accuracy:", naive_accuracy, "%\n")
cat("Our models must beat this to add value.\n")

# Logistic Model
log_acc <- round(cm_final_og$overall["Accuracy"], 4)
log_max_acc <- round(acc_cm_final$overall["Accuracy"], 4)
cat("\n=== LOGISTIC MODEL FOR CUSTOM CHURN ===\n")
cat("Accuracy:", log_acc, "%\n")
cat("Model Performs ", (log_acc - naive_accuracy) * 100, "% worse than the Naive Baseline")
cat("However, when prioritizing accuracy")
cat("\n=== LOGISTIC MODEL (MAX ACCURACY) FOR CUSTOM CHURN ===\n")
cat("Accuracy:", log_max_acc, "%\n")
cat("Model Performs ", (log_max_acc - naive_accuracy) * 100, "% better than the Naive Baseline")

# KNN Model
KNN_acc <- round(KNN_custom_churn$overall["Accuracy"], 4)
cat("\n=== LOGISTIC MODEL FOR CUSTOM CHURN ===\n")
cat("Accuracy:", KNN_acc, "%\n")
cat("Model Performs ", (KNN_acc - naive_accuracy) * 100, "% better than the Naive Baseline")



################################################
# 14. ROC CURVE
################################################

library(pROC) 

head(simple_model_down$pred)

# --- 1. Define ROC objects using cross-validation predictions ---

# Model 1: GLMNET (Down-sampled)
roc_glmnet_down <- roc(simple_model_down$pred$obs, simple_model_down$pred$Churn)

# Model 2: GLMNET (SMOTE) - Assuming this is your second GLMNET model
roc_glmnet_smote <- roc(simple_model_og_smote$pred$obs, simple_model_og_smote$pred$Churn)

# Model 3: KNN (Base)
# Filter for the predictions corresponding to the optimal 'k'
knn_base_pred_data <- knnchurned_acc$pred[knnchurned_acc$pred$k == knnchurned_acc$bestTune$k, ]
roc_knn_base <- roc(knn_base_pred_data$obs, knn_base_pred_data$Churn)

# Model 4: KNN (Custom)
# Filter for the predictions corresponding to the optimal 'k'
knn_cust_pred_data <- knnchurned_acc_cust$pred[knnchurned_acc_cust$pred$k == knnchurned_acc_cust$bestTune$k, ]
roc_knn_cust <- roc(knn_cust_pred_data$obs, knn_cust_pred_data$Churn)

# Plot the first curve (GLMNET Down-sampled)
# Use 'print.auc=TRUE' to show the AUC for the first model on the plot
plot(roc_glmnet_down, col = "#E41A1C", lwd = 2, main = "Cross-Validated ROC Curve Comparison", print.auc = TRUE)

# Add the subsequent curves using 'add = TRUE'
plot(roc_glmnet_smote, col = "#377EB8", lwd = 2, add = TRUE)
plot(roc_knn_base, col = "#4DAF4A", lwd = 2, add = TRUE)
plot(roc_knn_cust, col = "#984EA3", lwd = 2, add = TRUE)

# Add a comprehensive legend with AUC scores
legend("bottomright",
       legend = c(
         paste("GLMNET (Down-s):", round(auc(roc_glmnet_down), 3)),
         paste("GLMNET (SMOTE):", round(auc(roc_glmnet_smote), 3)),
         paste("KNN (Base):", round(auc(roc_knn_base), 3)),
         paste("KNN (Custom):", round(auc(roc_knn_cust), 3))
       ),
       col = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
       lwd = 2)


################################################
# 15. KEY INSIGHTS (GRAPHICAL)
################################################

## Churners by distance
master$custom_churn_labeled <- factor(master$custom_churn, 
                                      levels = c(0, 1), 
                                      labels = c("Non-Churners", "Churners"))
ggplot(master, aes(x = custom_churn_labeled, y = lackey_dist_miles, group = custom_churn_labeled, fill = custom_churn_labeled)) +
  geom_boxplot() +
  labs(
    x = "Customer Churn", # Sets the X-axis label
    y = "Distance from Lackey (miles)", # Sets the Y-axis label
    title = "Distance Comparison by Customer Churn Status") +
  theme_minimal() + guides(fill = "none")




## Miles from Lackey Clinic and Chronic vs. Acute Conditions

#Assuming 'master' is your DataFrame
# 1. Create a new categorical column to define the four distinct groups
master <- master %>%
  mutate(
    condition_group = case_when(
      has_chronic == 1 & has_acute == 1 ~ "Both Chronic and Acute",
      has_chronic == 1 & has_acute == 0 ~ "Only Chronic",
      has_chronic == 0 & has_acute == 1 ~ "Only Acute",
      has_chronic == 0 & has_acute == 0 ~ "Neither",
      TRUE ~ "Unclassified" # Catch-all for NAs or unexpected values
    )
  )

## 2. Calculate the median and mean distance for each group
distance_summary <- master %>%
  group_by(condition_group) %>%
  summarise(
    Median_Distance_miles = median(lackey_dist_miles, na.rm = TRUE),
    Average_Distance_miles = mean(lackey_dist_miles, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ## Sort by average distance for easier interpretation
  arrange(desc(Average_Distance_miles))


## 2. Create the necessary grouping variables
master <- master %>%
  # A. Create the labeled churn column (if not already done)
  mutate(
    custom_churn_labeled = factor(custom_churn, 
                                  levels = c(0, 1), 
                                  labels = c("Non-Churners", "Churners")),
    # B. Create a binary variable for having ANY condition (acute OR chronic)
    has_any_condition = case_when(
      has_acute == 1 | has_chronic == 1 ~ "Has Condition",
      TRUE ~ "No Condition"
    )
  )

## 3. Calculate the percentage of customers who 'Has Condition' by Churn Group
percentage_data <- master %>%
  group_by(custom_churn_labeled, has_any_condition) %>%
  summarise(
    Count = n(),
    .groups = 'drop_last'
  ) %>%
  ## Calculate the percentage within each churn group
  mutate(
    Percentage = Count / sum(Count) * 100
  ) %>%
  filter(has_any_condition == "Has Condition") # Keep only the 'Has Condition' bar

# 4. Generate the Bar Chart showing the percentage
ggplot(percentage_data, aes(x = custom_churn_labeled, y = Percentage, fill = custom_churn_labeled)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            vjust = -0.5, 
            size = 4) + # Add percentage labels above the bars
  labs(
    title = "Percentage of Customers with Acute or Chronic Conditions by Churn Status",
    x = "Customer Churn Status",
    y = "Has Acute or Chronic (%)"
  ) +
  scale_y_continuous(limits = c(0, max(percentage_data$Percentage) * 1.1)) + # Adjust y-axis limit
  theme_minimal() +
  guides(fill = "none") # Remove the legend

## Ensure churn is labeled for clarity
master <- master %>%
  mutate(
    custom_churn_labeled = factor(custom_churn, 
                                  levels = c(0, 1), 
                                  labels = c("Non-Churners", "Churners"))
  )

## Calculate the percentage of each condition within each churn group
percentage_summary <- master %>%
  group_by(custom_churn_labeled) %>%
  summarise(
    Perc_Has_Chronic = mean(has_chronic == 1) * 100,
    Perc_Has_Acute = mean(has_acute == 1) * 100,
    .groups = 'drop'
  )

# Convert to Long Format for Plotting
percentage_plot_data <- percentage_summary %>%
  pivot_longer(
    cols = starts_with("Perc_"),
    names_to = "Condition_Type",
    values_to = "Percentage"
  ) %>%
  mutate(
    # Clean up the Condition_Type names for the X-axis labels
    Condition_Type = str_replace_all(Condition_Type, c("Perc_Has_" = "", "_Chronic" = "Chronic", "_Acute" = "Acute"))
  )

## Reversed Grouped Bar Chart for condition type and churn prevelance
ggplot(percentage_plot_data, 
       aes(x = Condition_Type, # X-axis: Acute vs. Chronic
           y = Percentage, 
           fill = custom_churn_labeled)) + # Fill/Grouping: Churners vs. Non-Churners
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           width = 0.7) + # CORRECTED: Removed ++
  geom_text(aes(label = paste0(round(Percentage, 1), "%"), 
                group = custom_churn_labeled), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            size = 4) + 
  labs(
    title = "Condition Prevalence by Churn Status",
    x = "Condition Type", 
    y = "Percentage (%) of Group",
    fill = "Customer Churn Status" 
  ) + # CORRECTED: Removed ++
  scale_y_continuous(limits = c(0, max(percentage_plot_data$Percentage) * 1.1)) + 
  theme_minimal()
