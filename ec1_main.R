RunTransformation <- function(test = FALSE, save = TRUE, rerun = FALSE, escape = FALSE, verbose = FALSE) {
  
  if (verbose) {
    print('starting the transformation...')
  }
  
  # ============================================================================
  # 1. SET UP ENVIRONMENT
  # ============================================================================
  # SOURCE FUNCTIONS SCRIPT ====================================================
  setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Documents/GitHub/Epi_Cleaning"))
  source('ec1_functions.R')
  
  # LOAD R PACKAGES ============================================================
  # Get a list of packages to load from the functions script
  packages <- GetPackages()
  
  # This is a function to check if a package exists, otherwise install it
  PkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE)) {
      install.packages(x, dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
  
  lapply(packages,PkgTest)
  lapply(packages, require, character.only = TRUE)
  
  # ============================================================================
  # 1. SET UP ENVIRONMENT
  # ============================================================================
  # SET UP WORKING DIRECTORY & FOLDER PATHS ====================================
  # -- As data is stored outside of GitHub, do not use here()
  INPUT_FOLDER <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Documents/Data/Epi_Cleaning/input")
  OUTPUT_FOLDER <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Documents/Data/Epi_Cleaning/output")
  WORKING_FOLDER <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Documents/Data/Epi_Cleaning/working")
  
  # SET UP TEST ARGUMENTS ======================================================
  if (test) {
    test_append <- 'TEST_'
    test_dir <- '/Test'
    INPUT_FOLDER <- paste0(INPUT_FOLDER, '/Archived')
  } else {
    test_append <- ''
    test_dir <- ''
  }
  
  # CREATE TIME STAMPS =========================================================
  # today's time stamp (date only) ---------------------------------------------
  today_date <- lubridate::today()
  
  # today's time stamp (date & time only) --------------------------------------
  today_datetime <- substring(gsub("\\s", "_", gsub(":|-", "", Sys.time())), 3, 13)
  
  # adhoc date stamp------------------------------------------------------------
  output_files <- list.files(file.path(OUTPUT_FOLDER)) %>% 
    as.data.frame(col.names = "filename") %>% 
    dplyr::filter( . != "Test") %>% 
    dplyr::arrange(desc(.))
  
  if(!rerun & !test) {
    adhoc_date <- output_files[1,]
  } else {
    adhoc_date <- output_files[2,]
  }
  
  # CREATE OUTPUT FOLDERS ======================================================
  # For output files -----------------------------------------------------------
  # This should be done with two steps, otherwise R thorws an error
  dir.create(paste0(OUTPUT_FOLDER, "/",test_dir), showWarnings = FALSE)
  dir.create(paste0(OUTPUT_FOLDER, '/', test_dir, "/", today_date), showWarnings = FALSE)
  
  # For working files ----------------------------------------------------------
  dir.create(paste0(WORKING_FOLDER, "/", test_dir), showWarnings = FALSE)
  
  # ============================================================================
  # 2. LOAD INPUT FILES
  # ============================================================================
  # IMPORT DATA SET A (PATIENTS) ===============================================
  # List all files in the input folder -----------------------------------------
  patients_files <- list.files(file.path(INPUT_FOLDER), 
                           pattern = "patients_covid",
                           full.names = T)
  
  # Sort by modified date-time and pick the latest modified/created file -------
  patients_files.df <- purrr::map_dfr(patients_files,file.info) %>%
    tibble::rownames_to_column("filename") %>%
    dplyr::arrange(desc(mtime))
  
  # First record at the top should be the latest file modified/created -- slice_head() selects the first row 
  patients_filename <- dplyr::slice_head(patients_files.df)$filename
  
  # Check the date and time last modified
  file_last_dt <- gsub("\\s", "_", gsub(":|-", "", file.info(file.path(patients_filename))$mtime))
  
  # Stop the further steps if --------------------------------------------------
  # -- there hasn't been a new input file created/modified since the last run
  
  # Get the latest transformation run time
  last_input_dt <- readRDS(paste0(WORKING_FOLDER, "/last_input_dt.rds"))
  
  if (substr(file_last_dt, 1, 8) == last_input_dt) {
    # If we're not ignoring the warning for test/rerun/3rd run
    if (!test & !escape & !rerun) {
      stop(
        stringr::str_glue(
          "There is no new export since the last one saved on {last_input_dt}.\nDo you want to pursue with this exported file?"),
        call. = F)
    } else {
      if (verbose) {
        print(glue::glue('There is no new export since the last one saved on {last_input_dt}.\nThe run will continue anyway.'))
      }
    }
  }
  
  if (verbose) {
    print('patient filename:')
    print(patients_filename)
  }
  
  # Set a column type for each of the columns and load the data set ------------
  # -- otherwise R sets an incorrect column type if there are any invalid values
  df_patients <- readxl::read_excel(patients_filename, 
                                              col_types = c('text', 'date', 'date', 'text', 'text', #1
                                                            'text', 'text', 'text', 'text', 'text', #2
                                                            'text', 'text', 'text', 'text', 'text', #3
                                                            'text', 'text', 'text', 'text', 'text', #4
                                                            'text', 'numeric', 'numeric', 'numeric', 'numeric' #5
                                                            ))
  
  # Flag if we detect IDs that don't have 35 digits (invalid IDs) --------------
  errorRecords <- df_patients[nchar(df_patients$`Patient ID`) != 9, ]
  # Remove invalid IDs from the df
  df_patients <- df_patients[nchar(df_patients$`Patient ID`) == 9, ]
  
  # IMPORT DATA SET B (ENCOUNTERS) =============================================
  # List all files in the input folder -----------------------------------------
  encounters_files <- list.files(file.path(INPUT_FOLDER), 
                               pattern = "encounters",
                               full.names = T)
  
  # Sort by modified date-time and pick the latest modified/created file -------
  encounters_files.df <- purrr::map_dfr(encounters_files,file.info) %>%
    tibble::rownames_to_column("filename") %>%
    dplyr::arrange(desc(mtime))
  
  # First record at the top should be the latest file modified/created -- slice_head() selects the first row 
  encounters_filename <- dplyr::slice_head(encounters_files.df)$filename
  
  if (verbose) {
    print('encounter filename:')
    print(encounters_filename)
  }
  
  # Set a column type for each of the columns and load the data set ------------
  df_encounters <- readxl::read_excel(patients_filename, 
                                    col_types = c('text', 'date', 'date', 'text', 'text', #1
                                                  'text', 'text', 'text', 'text', 'text', #2
                                                  'numeric', 'numeric', 'numeric', 'text', 'text' #3
                                    ))
  
  # ============================================================================
  # 3. CREATE VARIABLE LISTS
  # ============================================================================
  # Load a variable mapping file ===============================================
  VAR_MAP <-readxl::read_excel(
    path = paste0(WORKING_FOLDER, "/", "variable_mapping_20230331.xlsx"),
    sheet = "MAP")
  
  # Rename column names in the variable_mapping file for ease of use -----------
  names(VAR_MAP) <-
    c("entity_name",
      "field_name",
      "inputcol",
      "midcol",
      "outputcol",
      "note",
      "variable_list",
      "rename_variable")
  
  # Create variable lists ======================================================
  VAR_LIST <- VAR_MAP %>% dplyr::filter(variable_list != "N/A") %>% 
    dplyr::select(midcol)
  
  # Load a template for the output file ========================================
  ROSTER_TEMPLATE <- readr::read_csv(paste0(WORKING_FOLDER, "/", "roster_header_template_20230331.csv", col_types = cols()))
  
  # Create Yes/No variable list ================================================
  YESNO_VARIABLES <- GetYesNoVariables()
  
  if (verbose) {
    print('Files loaded...')
  }
  
  # ============================================================================
  # 4. RENAME FIELD NAMES IN VARIABLE MAPPING FILE
  # ============================================================================
  # Load a field mapping file ==================================================
  FIELD_MAPPING <- VAR_MAP %>% dplyr::filter(field_name != "N/A")
  
  # Drop unnecessary variables for patients data set ===========================
  labelsPat <- dplyr::filter(FIELD_MAPPING, !inputcol %in% c(
    "SUFFIX"	,
    "MAIDEN NAME"))
  
  # Create a list to Rename columns to midcol names for ease of use ------------
  renameVarPatients <- as.list(labelsPat$`inputcol`)
  names(renameVarPatients) <- labelsPat$midcol
  
  # Drop unnecessary variables for encounters data set =========================
  labelsEnc <- dplyr::filter(VARIABLE_MAPPING, !inputcol %in% c(
    "REASONCODE",
    "REASONDESCRIPTION"))
  
  # ============================================================================
  # 5. RENAME FIELD NAMES ON PATIENTS DATA SET
  # ============================================================================
  # Subset fields to just those needed =========================================
  df_pat_1 <- df_patients %>% 
    dplyr::select(all_of(labelsPat$inputcol)) %>% 
    dplyr::mutate_all(as.character)
  
  # Rename columns to midcol names for ease of use==============================
  df_pat_1 <- df_pat_1 %>% dplyr::rename(!!!renameVarPatients)
  
  # ============================================================================
  # 6. RENAME FIELD NAMES ON ENCOUNTERS DATA SET
  # ============================================================================
  #  Subset fields to just those needed ========================================
  df_enc_1 <- df_encouters %>%
    dplyr::select(all_of(labelsEnc$inputcol)) %>%
    dplyr::mutate_all(as.character)
  
  # Rename columns to midcol names for ease of use==============================
  df_enc_1 <- df_enc_1 %>%
    dplyr::rename(
      enc_id = `Id` ,
      start_time = `START TIME` ,
      stop_time = `STOP TIME`)
  
  # ============================================================================
  # 7. REPLACE OLD PATIENT IDs WITH NEW PATIENT IDS
  # ============================================================================
  if (verbose) {
    print(paste("rows before updating patient ids =", nrow(df_pat_1)))
  }
  
  updated_pat_id <- readxl::read_xlsx(path = paste0(WORKING_FOLDER, "/", "updated_patids_20230331.xlsx"),
                                       sheet = "UpdatedPatIds") %>%
    dplyr::filter(!is.na(new_id)) %>%
    dplyr::filter(ignore != "Yes")
  
  df_pat_1 <- dplyr::left_join(df_pat_1, updated_pat_id, by = c("pat_id" = "new_id"), keep = TRUE) %>% 
    filter(!is.na(current_id)) %>%
    dplyr::mutate(current_id = case_when(
      FALSE ~ pat_id,
      TRUE ~ current_id)) %>%
    dplyr::mutate(pat_id = case_when(
      pat_id != current_id ~ current_id,
      TRUE ~ pat_id)) %>% 
    dplyr::select(-c("current_id", "new_id"))
  
  if (verbose) {
    print(paste("rows after updating patient ids =", nrow(df_pat_1)))
  }
  
  # ============================================================================
  # 8. TRANSFORM PATIENTS DATA
  # ============================================================================
  # Disaggregate race data
  df_pat_1 <- RaceDisagg(df_pat_1, VAR_MAP)
  
  # ============================================================================
  # 9. MERGE PATIENTS DATA & ENCOUNTERS DATA
  # ============================================================================
  # Merge patient data and encoutner data ======================================
  df_input_1a <- dplyr::inner_join(df_pat_1, df_enc_1, by = c("pat_id" = "enc_id"))
  
  # Extract records without encounter data =====================================
  df_input_1b <- dplyr::anti_join(df_pat_1, df_enc_1, by = c("pat_id" = "enc_id"))
  
  # Append records without enc data to records with enc data ===================
  df_input_2 <- bind_rows(df_input_1a, df_input_1b) %>%
    # subset columns to just those we need
  dplyr::select(all_of(VAR_LIST$midcol)) %>%
    # reformat dates for output
  dplyr::mutate(
    birth_date = format(as.Date(birth_date, "%Y-%m-%d"), "%m/%d/%Y"),
    enc_date = format(as.Date(enc_date, "%Y-%m-%d"), "%m/%d/%Y"),
    death_date	= format(as.Date(death_date, "%Y-%m-%d"), "%m/%d/%Y"))
  
  # Standardize all the variables ==============================================
  df_input_2 <- df_input_2 %>%
    dplyr::mutate_all(as.character) %>% 
    dplyr::mutate_all(EmptyAsNA) %>%
    dplyr::distinct()
  
  # ============================================================================
  # 10. TRANSFORM INPUT DATA - 1
  # ============================================================================
  # Transform yes/no, language, race, ethnicity, marriage fields
  df_input_3 <- TransformYNLREM(df_input_2, YESNO_VARIABLES, WORKING_FOLDER)
  
  # ============================================================================
  # 11. FLAG & EXPORT UNCOMPLETED RECORDS
  # ============================================================================
  # Set a path for uncompleted encounter records--------------------------------
  UNC_ENC_FILE_PATH <- file.path(OUTPUT_FOLDER,"/", today_date, test_dir, "/", glue::glue("uncompletedenc_ids_{today_datetime}.xlsx"))
  
  # Export records if any ------------------------------------------------------
  df_input_3 <- HandleUncompletes(df_input_3, REJECTED_RECORD_FILE_PATH)
  
  # ============================================================================
  # 12. RANSFORM INPUT DATA - 2
  # ============================================================================
  # Transform hospital and symptom fields
  df_input_4 <- TransformSH(df_input_3, VAR_MAP, today_date)
  
  # ============================================================================
  # 13. TRANSFORM HOUSEHOLD MEMBERS FIELD
  # ============================================================================
  # Convert NA to Blanks for non-job data ======================================
  df_input_4 <- NAToBlank(df_input_4)
  
  # Clean household members fields =============================================
  # -- Separate comma-separated values into multiple rows
  sub_df_input_1_a <- df_input_4 %>% 
    dplyr::filter(grepl(",", household_members_name)) %>% 
    tidytext::unnest_tokens(input = household_members_name, 
                            output = "household_members_name", 
                            token = "regex", pattern = ",") %>% 
    dplyr::filter(!duplicated(pat_id))
  
  sub_df_input_1_b <- df_input_4 %>% 
    dplyr::select(pat_id, enc_id, first_name, last_name, birth_date, gender, household_members_name) %>% 
    filter(grepl(",", household_members_name)) %>% 
    tidytext::unnest_tokens(input = household_members_name, 
                            output = "household_members_name", 
                            token = "regex", pattern = ",")  %>% 
    filter(duplicated(pat_id))
  
  sub_df_input_2 <- plyr::rbind.fill(sub_df_input_1_a, sub_df_input_1_b)
  
  # Append the two sub data frames ---------------------------------------------
  df_input_5 <- dplyr::bind_rows(df_input_4, sub_df_input_2) %>% 
    dplyr::filter(!grepl(",", household_members_name))
  
  # Clean alternate contact phone number field =================================
  # -- The phone numbers should be in the format (###) ###-####
  df_input_5 <- df_input_5 %>% 
    dplyr::mutate(alt_phone_number =
                    FormatPhone(alt_phone_number))
  
  if (verbose) {
    print('data cleaning complete...')
  }
  
  # ============================================================================
  # 14. VALIDATE ROSTER FILE
  # ============================================================================
  if (verbose) {
    print('validating df_input_5')
  }
  
  # Check if there are any unusual NA columns ==================================
  CheckNAColumns(df_input_5, verbose)
  
  # Check if patient ID is not null ============================================
  if (any(is.na(df_input_5$pat_id))) {
    print("There are missing patient IDs in the roster. Please check the output.")
  }
  
  # Validate all the fields and flag errors ====================================
  error_ids <- ValidateRoster(df_input_5, df_pat_1, df_enc_1)
  
  # ============================================================================
  # 23. EXPORT DATA
  # ============================================================================
  if (verbose) {
    print('exporting data...')
  }
  
  if (save) {
    ExportOutputs(df_input_5, error_ids, test, verbose, INPUT_FOLDER, OUTPUT_FOLDER,
                  WORKING_FOLDER, today_date, today_datetime)
  } else {
    print('Skipping export step.')
  }
  
  # ============================================================================
  # 24. MOVE INPUT FILES TO ARCHIVED FOLDER
  # ============================================================================
  # Move the input files to Archived folder ====================================
  if (!test) {
    # clear the patient input files --------------------------------------------
    filesstrings::file.move(patients_filename,
                            paste0(WORKING_FOLDER, "/archived"))
  }

  if (!test) {
    # clear the encounter input files ------------------------------------------
    filesstrings::file.move(encounters_filename,
                            paste0(WORKING_FOLDER, "/archived"))
  }
}