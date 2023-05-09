# ==================GetPackages ================================================
GetPackages <- function() {
  
  return(c(
    "here",
    "readxl",
    "lubridate",
    "data.table",
    "tidyverse",
    "odbc",
    "sqldf",
    "openxlsx",
    "tidytext",
    "Hmisc",
    "dplyr",
    "fs",
    "filesstrings",
    "stringi",
    "zoo",
    "tictoc",
    "gestalt"
  ))
}

#===================Utility Functions===========================================
# Function to clean up phone data ----------------------------------------------
FormatPhone <- function(phone, invalid = NA){
  phone <- gsub("[[:alpha:]]", "", phone)          # remove letters
  phone <- gsub("[[:punct:]]", "", phone)          # remove punctuation
  phone <- trimws(phone)                           # trim white space from ends
  phone <- gsub(' ','', phone)                     #remove spaces in the middle
  phone[!nchar(phone) %in% c(7, 10)] <- invalid    # keep only 7 or 10 digit numbers
  phone[nchar(phone) %in% 7] <- gsub("(^\\d{3})(\\d{4}$)", 
                                     "\\1-\\2", 
                                     phone[nchar(phone) %in% 7])
  phone[nchar(phone) %in% 10] <- gsub("(^\\d{3})(\\d{3})(\\d{4}$)", 
                                      "(\\1) \\2-\\3",
                                      phone[nchar(phone) %in% 10])
  return(phone)
}

# Function to recode yes/no variables ------------------------------------------
RecodeYesNo <- function(var) {
  recode(
    as.character(stringr::str_to_upper(var)),
    "NO" = "NO",
    "YES" = "YES",
    "I DON'T KNOW" = "UNKNOWN",
    .default = NA_character_)
}

# Function to clean texts ------------------------------------------------------
CleanString <- function(string) {
  string <- stringr::str_to_upper(stringr::str_trim(
    stringr::str_replace_all(string, "[[:punct:]]|[[:space:]]", ""),
    side = c("both")))
  return(as.character(string))
}

# Function to set blanks to NAs ------------------------------------------------
EmptyAsNA <- function(x){ifelse(as.character(x)!="", x, NA)}

# Function to set NAs to blanks ------------------------------------------------
NAAsBlank <- function(x){ifelse(is.na(x), "", x)}

NAToBlank <- function(dataset) {
  dataset <- dataset %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::mutate_all(NAAsBlank) %>% 
    dplyr::mutate_all(EmptyAsNA) %>% 
    dplyr::mutate_all(as.character)
  return(dataset)
}

# Function to set fields to empty strings --------------------------------------
SetToBlank <- function(x){ x = ''}

# Function to replace suffixes .x and .y after join ----------------------------
add_suffixes <- compose(
  function(x) gsub("\\.x", "\\.pat", x),
  function(x) gsub("\\.y", "\\.enc", x)
) 

#===================GetYesNoVariables===========================================
GetYesNoVariables <- function() {
  
  YESNO_VARIABLES <- c(
    "symptom_onset_date",
    "test_date",
    "fever_yesno",
    "fever_temp",
    "sym_yesno",
    "sym_headache",
    "sym_congestion",
    "sym_cough",
    "sym_nausea",
    "sym_vomiting",
    "sym_diarrhea",
    "sym_abdominal",
    "sym_other",
    "hospitalized",
    "icu",
    "death_date")
  
  return(YESNO_VARIABLES)
}

#===================RaceDisagg==================================================
# Disaggregate race field
RaceDisagg <- function(df_pat_1, VAR_MAP) {
  
  df_pat_1$race <- coalesce(
    df_pat_1$`Race(s) 1`, 
    df_pat_1$`Race(s) 2`)
  
  df_pat_1$race_unknown <- grepl('Unknown', df_pat_1$race, ignore.case = TRUE)
  df_pat_1$race_declined <- grepl('Declined', df_pat_1$race, ignore.case = TRUE)
  
  df_pat_1$race_other <- case_when(
    (grepl("Multiracial", df_pat_1$race, ignore.case = TRUE)
     & grepl("Other", df_pat_1$race, ignore.case = TRUE)) ~ '',
    grepl("Multiracial", df_pat_1$race, ignore.case = TRUE) ~ "MULTIRACIAL",
    grepl("Other", df_pat_1$race, ignore.case = TRUE) ~ "OTHER",
    TRUE ~ '')
  
  return(df_pat_1)
}

#===================TransformYNLREM=============================================
# Transform the yes/no, language, race, ethnicity, marriage fields
TransformYNLREM <- function(df_input_2, YESNO_VARIABLES, WORKING_FOLDER) {
  
  # Load a language mapping file ----------------------------------------------- 
  LANGUAGE_MAPPING <- 
    readxl::read_excel(
      paste0(WORKING_FOLDER, "/language_mapping_20230331.xlsx")) 
  
  df_input_3 <- df_input_2 %>%
    dplyr::mutate_at(c(YESNO_VARIABLES), RecodeYesNo) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(
      entered_by = sub("# ", "", entered_by),
      fever_temp =
        ifelse(
          (as.numeric(fever_temp) >= 99.0 &as.numeric(fever_temp) <= 112.0),
          round(as.numeric(fever_temp), digits = 1), NA) # must be 1 decimal place
      ) 
  
  # Recode language & the other fields -----------------------------------------
  language.list <- as.list(LANGUAGE_MAPPING$`Reference Code - New Value for Output`)
  names(language.list) <- LANGUAGE_MAPPING$`Original Value`
  
  # Take only one language per entry
  df_input_3 <- df_input_3 %>% 
    dplyr::mutate(
      language = case_when(grepl(',', language) ~ strsplit(language, ',')[[1]][1],
                           TRUE ~ language))
  
  df_input_3 <- df_input_3 %>%
    dplyr::mutate(
      language = dplyr::recode(language, !!!language.list),
      
      # Recode translator needed field --- if language is English then set translator to blank
      translator = 
        case_when(language == "ENGLISH" & translator == "YES" ~ NA_character_,
                  TRUE ~ translator),
      
      ethnicity = recode(ethnicity,
                         `Non-Hispanic, Latino/a, Latinx` = "NOT_HISPANIC_OR_LATINO",
                         `Hispanic, Latino/a, Latinx` = "HISPANIC_OR_LATINO",
                         `Unknown` = "UNKNOWN",
                         .missing = ""),
      
      under_pregnancy = 
        recode(under_pregnancy, `Neither of these` = "NEITHER",
               Pregnant = "PREGNANT",
               `Had a baby in the past 6 months` = "POSTPARTUM",
               `I don't know` = "UNKNOWN",
               .missing = ""))
  
  # recode marriage fields -----------------------------------------------------
  # recode the invalid apostrophe
  df_input_3$marital_status <- gsub("[^0-9A-Za-z.,-^#^*///' ]", "'" , df_input_3$marital_status, ignore.case = TRUE)
  
  df_input_3 <- df_input_3 %>%
    dplyr::mutate(marital_status =
                    case_when(marital_status == "I don'''t know" ~ "UNKNOWN",
                              marital_status == "I don't know" ~ "UNKNOWN",
                              marital_status == "Yes" ~ "YES",
                              marital_status == "No" ~ "NO",
                              TRUE ~ marital_status)) %>%
    
    dplyr::select(-c(enrolled_school_yn, enrolled_school_name))
  
  return(df_input_3)
}

#===================HandleUncompletes===========================================
# Flag and export uncompleted encounter records
HandleUncompletes <- function(df_input_3, UNC_ENC_FILE_PATH) {
  
  # Load a file that contains the latest status of encounters ------------------
  latest_status <- readxl::read_xlsx(path = paste0(WORKING_FOLDER, "/", "latest_status_20230331.xlsx"),
                                                    sheet = "LatestStatus") %>%
    dplyr::filter(!is.na(enc_id)) %>%
    dplyr::mutate_all(as.character)
  
  # Compare to the investigation status in the new REDCap file -----------------
  compare_status <- df_input_3 %>% 
    dplyr::mutate(old_status = status) %>%
    dplyr::select(enc_id, old_status) %>%
    dplyr::mutate_all(as.character) %>% 
    dplyr::left_join(y = latest_status, by = c("enc_id"))
  
  # Frag incomplete encounter records ------------------------------------------
  unc_ids <- compare_status %>% 
    dplyr::filter(
      new_status %in% c("Uncompleted", "Status Unknown") &
        toupper(old_status) == "not_completed",
      .preserve = TRUE)
  
  # Export a list if at least 1 case was found to be uncompleted ---------------
  if (nrow(unc_ids) > 0) {
    unc_ids <- dplyr::mutate_all(unc_ids, as.character) %>% 
      dplyr::mutate_all(as.character) %>% 
      dplyr::mutate_all(NAAsBlank) %>%
      dplyr::mutate_all(EmptyAsNA)  %>% 
      dplyr::mutate_all(NAAsBlank) 
    
    # Export a list of the uncompleted records
    openxlsx::write.xlsx(unc_ids, file = paste0(UNC_ENC_FILE_PATH))
    
    # Remove these rejected records from the roster ----------------------------
    df_input_3 <- df_input_3 %>%
      dplyr::filter(enc_id %in% unc_ids$unc_id)
  }
  
  return(df_input_3)
}


#===================TransformSH=================================================
# Transform hospital and symptom fields
TransformSH <- function(df_input_3, VAR_MAP, today_date) {
  
  # Create a list to rename midcol names to outputcol names --------------------
  RENAME_VARIABLE <- VAR_MAP %>% dplyr::filter(rename_variable != "N/A")
  
  rename.variable <- as.list(RENAME_VARIABLE$midcol)
  names(rename.variable) <- RENAME_VARIABLE$outputcol
  
  # Load a hospital list and transform the hospital field ----------------------
  hospital_list <- readr::read_csv(paste0(WORKING_FOLDER, "/",
                                          "hospital_list_20230331.csv", col_types = cols())) %>% 
    dplyr::mutate_all(as.character)
  
  df_input_3 <- df_input_3 %>%
    dplyr::mutate(dplyr::across(.cols = c(hospital_name, lab_name), ~ na.fill(.x, '')))
  
  df_input_4 <- df_input_3 %>% 
    left_join(hospital_list, by = c("hospitalized_id" = "hospital_list_id"))

  
  # Transform symptom fields ---------------------------------------------------
  df_input_4 <- dplyr::mutate(
    df_input_4,
    across(
      c(birth_date,
        enc_date,
        symptom_onset_date),
      as.Date, "%m/%d/%Y"),
    
    symptom_onset_date = case_when(
      symptom_onset_date > as.Date(today_date) ~ as.Date(NA),
      symptom_onset_date >= birth_date ~ as.Date(symptom_onset_date, "%m/%d/%Y"),
      is.na(birth_date) ~ as.Date(symptom_onset_date, "%m/%d/%Y"),
      as.character(birth_date) == "" ~ as.Date(symptom_onset_date, "%m/%d/%Y"),
      TRUE ~ as.Date(NA)),
    
    enc_date =
      case_when(
        enc_date > as.Date(today_date) ~ as.Date(NA),
        TRUE ~ as.Date(enc_date, "%m/%d/%Y")),
    
    across(c(
      birth_date,
      enc_date,
      symptom_onset_date),
      format, "%m/%d/%Y"),
    
    note = case_when(!is.na(note) ~ glue::glue("No additional comments"))) %>%
    
    dplyr::select(all_of(RENAME_VARIABLE$midcol)) %>%
    dplyr::rename(!!!rename.variable) %>% 
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_all(EmptyAsNA)
  
  return(df_input_4)
}

#===================CheckNAColumns============================================== 
CheckNAColumns <- function(df, verbose) {
  n = 1
  nacols <- c()
  for (c in colSums(is.na(df))) {
    if (c == nrow(df)) {
      nacols <- append(nacols, names(df)[n])
    }
    n <- n+1
  }
  ALWAYS_NACOLS <- c("health_status",
                     "living_status",
                     "employment_status")
  
  USUALLY_NACOLS <- c("death_date",
                      "icu")
  k = 1
  extra_nacols <- c()
  for (x in nacols) {
    if (!x %in% ALWAYS_NACOLS & !x %in% USUALLY_NACOLS) {
      extra_nacols <- append(extra_nacols, nacols[k])
    }
    k <- k+1
  }
  
  # if more than 30% of the file is NA, there might be a bug and need double-check
  if (length(nacols)/ncol(df) >= .3 ) {
    print('Detected unusual full-NA columns:')
    print(extra_nacols)
    print(paste0(((length(nacols)*100)/ncol(df)), '% of columns are missing data.'))
  } else {
    if (verbose) {
      print('No unusual full-na columns detected.')
    }
  }
}

#===================ValidateRoster==============================================
ValidateRoster <- function() {
  
  df_input_6 <- df_input_5 %>%
    dplyr::left_join(df_pat_1, by = c("pat_id"), suffix = c(".fin", ".pat"),
                     keep = TRUE) %>%
    dplyr::left_join(df_enc_1, by = c("enc_id"), suffix = c(".fin", ".enc"),
                     keep = TRUE)
  
  df_input_6 <- df_input_6 %>% dplyr::mutate(
    
    language_discrepancy = case_when(
      tolower(language.fin) != tolower(language.pat)
      & !tolower(language.fin) %in% c('unknown', 'no') ~ 1,
      TRUE ~ 0),
    
    translator_discrepancy = case_when(
      tolower(translator.fin) != tolower(translator.pat)
      & tolower(language.fin) != "english" & language.fin != 'No' ~ 1,
      TRUE ~ 0),
    
    symptoms_discrepancy = case_when(
      tolower(symptoms.fin) != 'no'
      & tolower(symptoms.fin) != tolower(symptoms.enc) ~ 1,
      TRUE ~ 0),
    
    alt_contact_discrepancy = case_when(
      tolower(alt_contact.fin) != 'no'
      & tolower(alt_contact.pat) == "yes" 
      & tolower(alt_contact.fin) != "yes" ~ 1,
      TRUE ~ 0),
    
    fever_temp_discrepancy = case_when(
      as.numeric(fever_temp.fin) != 0 
      & (as.numeric(fever_temp.enc) < 99 | as.numeric(fever_temp.enc) > 112) ~ 1,
      TRUE ~ 0)
  )
  
  df_input_6$other_discrepancy <- 0
  df_input_6$other_discrepancy_type <- ''
  df_input_6$has_discrepancy <- FALSE
  
  for (n in 1:nrow(df_input_6)) {
    df_input_6[n, ] <- ValidateEntry(df_input_6[n, ])
  }
  
  # Filter the flagged issues --------------------------------------------------
  df_input_6 <- df_input_6[df_input_6$has_discrepancy, ]
  
  # Only try this if we have data, otherwise it crashes
  if (ncol(df_input_6) > 0 & nrow(df_input_6) > 0) {
    # Transform for increased readability
    df_input_6[df_input_6 == 0] <- ''
    df_input_6[df_input_6 == TRUE] <- 'Discrepancy detected'
    
    # Take only the relevant information
    error_ids <- df_input_6[c(
      "pat_id",
      "first_name",
      "last_name",
      "language_discrepancy",
      "translator_discrepancy",
      "symptoms_discrepancy",
      "alt_contact_discrepancy",
      "fever_temp_discrepancy",
      "other_discrepancy",
      "other_discrepancy_type")]
  }
  
  return(error_ids)
}


#===================ValidateEntry===============================================
# Check the entries for contradictions
ValidateEntry <- function(df_input_6) {
  
  other_cols_1 <- c(
    "cough.fin",
    "headeche.fin",
    "congestion.fin",
    "nausea.fin",
    "vomitting.fin",
    "abdominal_pain.fin")
  
  other_cols_2 <- c(
    "cough.enc",
    "headeche.enc",
    "congestion.enc",
    "nausea.enc",
    "vomitting.enc",
    "abdominal_pain.enc")
  
  # Sweep through everything else looking for mismatches -----------------------
  for (n in 1:length(other_cols_1)) {
    if (tolower(df_input_6[other_cols_1[n]]) != 'no'
        & tolower(df_input_6[other_cols_1[n]]) != tolower(df_input_6[other_cols_2[n]])) {
      df_input_6$other_discrepancy <- 1
      df_input_6$other_discrepancy_type <- paste0(df_input_6$other_discrepancy_type, other_cols_1[n])
    } 
  }
  
  # Flag to keep if we had any issues
  # Select the sixteen discrepancy check columns, ignore the text at the end
  if (sum(df_input_6[, c("language_discrepancy",
                         "translator_discrepancy",
                         "symptoms_discrepancy",
                         "alt_contact_discrepancy",
                         "fever_temp_discrepancy",
                         "other_discrepancy")]) > 0) {
    df_input_6$has_discrepancy <- TRUE
  } 
  
  return(df_input_6)
}

#===================ExportOutputs===============================================
# export all output files
ExportOutputs <- function(df_input_5, error_ids, test, verbose, INPUT_FOLDER,
                          OUTPUT_FOLDER, WORKING_FOLDER, today_date, today_datetime) {
  
  if (test) {
    test_append <- 'TEST_'
    test_dir <- '/Test'
  } else {
    test_append <- ''
    test_dir <- ''
  }
  
  # ============================================================================
  # EXPORT ERROR RECORDS
  # ============================================================================
  if (verbose) {
    print('exporting error_ids...')
  }
  # Export only if any errors --------------------------------------------------
  if (ncol(error_ids) > 0 & nrow(error_ids > 0)) {
    
    print('Detected discrepancies in the roster. Writing error log...')
    
    readr::write_csv( 
      error_ids,
      file.path(paste0(OUTPUT_FOLDER, "/", test_dir, "/", today_date, "/"),
                glue::glue("{test_append}error_ids_{today_datetime}.csv")), na = "")
  } else {
    print('No errors detected')
  }
  
  # ============================================================================
  # EXPORT ROSTER
  # ============================================================================
  if (verbose) {
    print('exporting df_input_5...')
  }
  
  # Split roster every 2k records
  split_df_input_5 <- split(df_input_5, rep(1:ceiling(nrow(df_input_5)/2000), each=2000))
  
  # Write the sliced file without zipping it
  num_smallimp <- 1
  for (n in split_df_input_5) {
    
    readr::write_csv(n, 
                     file = file.path(
                       paste0(OUTPUT_FOLDER, "/", test_dir, "/", today_date, "/",
                              num_smallimp, "_", test_append, "roster_", today_datetime, ".csv")),
                     na = "")
    
    num_smallimp <- num_smallimp + 1
  }
  
  # ============================================================================
  # SAVE BACK UP FILES
  # ============================================================================
  # save error_ids and roster files ============================================
  saveRDS(error_ids, paste0(WORKING_FOLDER, test_dir, "/", test_append, 'error_ids.rds'))
  saveRDS(df_input_5, paste0(WORKING_FOLDER, test_dir, "/", test_append, 'df_input_5.rds'))
  
  # save the last date/time of modification ====================================
  saveRDS(str_remove_all(today(), '-'), paste0(WORKING_FOLDER, test_dir, "/", test_append, 'last_input_dt.rds'))
}
