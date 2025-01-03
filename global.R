#------------------------------------------------------------#
# ----- Dependencies -----
#------------------------------------------------------------#
# Define the list of required packages
required_packages <- c(
  "shiny",
  "bs4Dash",
  "googlesheets4",
  "googledrive",
  "hawkinR",
  "shinyWidgets",
  "waiter",
  "tidyverse",
  "base64enc",
  "jsonlite",
  "gargle",
  "igniteR",
  "waiter",
  "DT",
  "shinyjs"
)

# Function to install and load packages
install_and_load <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    message(paste("Installing package:", package))
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# Loop through each package and ensure it is installed and loaded
lapply(required_packages, install_and_load)

#------------------------------------------------------------#
# ----- Create Serial Date Time -----
#------------------------------------------------------------#
dateTime <- function() {
  # Get the current system time
  current_time <- Sys.time()
  
  # Convert to yyyymmddhhmmss format
  formatted_time <- format(current_time, "%Y%m%d%H%M%S")
  
  options(scipen = 999)
  
  return(as.numeric(formatted_time))
}

#------------------------------------------------------------#
# ----- Convert Inches to Feet-Inches -----
#------------------------------------------------------------#
feet_inches <- function(inches) {
  feet <- inches %/% 12      # Calculate the number of feet
  remaining_inches <- inches %% 12  # Calculate the remaining inches
  paste(feet, "'", remaining_inches, '"', sep = "")
}

#------------------------------------------------------------#
# ----- Store Vars in File -----
#------------------------------------------------------------#
saveVars <- function() {
  save(cmjData, sjData, mrData, fiveTenFiveData, laneAgilityData, fortyData, vertData, threeQuarterData, anthroData, broadData, teams, roster, acctManager, sportList, posList, classList, file = "backup_data.RData")
  drive_upload("backup_data.RData", path = as_id(Sys.getenv("BACKUP_FOLDER_ID")), type = "application/x-gzip", overwrite = TRUE)
}

#------------------------------------------------------------#
# ----- Update Data Variable -----
#------------------------------------------------------------#
updateVar <- function(newdata, varName) {
  # Join new data to existing data
  df <- rbind(get(varName), newdata)
  # Assign to previous variable
  assign(varName, df)
}

#------------------------------------------------------------#
# ----- Env Variables -----
#------------------------------------------------------------#

## Hawkin Dynamics
hdToken <- Sys.getenv("HD_TOKEN")

## Google Sheet
gsheetId <- Sys.getenv("GSHEET_ID")

# Google Drive Folder ID
folderId <- Sys.getenv("BACKUP_FOLDER_ID")

#------------------------------------------------------------#
#----- Google Sheets -----
#------------------------------------------------------------#

## Google Authentication
googleAuth <- function() {
  
  base64_json <- Sys.getenv("GCP_SERVICE_ACCOUNT_BASE64")
  
  decoded_json <- rawToChar(jsonlite::base64_dec(base64_json))
  
  # Write decoded_json to a temporary file
  temp_file <- tempfile(fileext = ".json")
  writeLines(decoded_json, temp_file)
  
  # Generate a token with service account credentials and required scopes
  token <- gargle::credentials_service_account(
    path = temp_file,
    scopes = c(
      "https://www.googleapis.com/auth/spreadsheets",
      "https://www.googleapis.com/auth/drive"
    )
  )
  
  # Authenticate with googlesheets4
  googlesheets4::gs4_auth(token = token)
  
  # Authenticate with googledrive
  googledrive::drive_auth(token = token)
  
  # Clean up temporary file
  unlink(temp_file)
}

## Get Sheet Data
get_gsheet <- function(sheet, range = NULL) {
  googlesheets4::read_sheet(ss = gsheetId, sheet = sheet, range = range)
}

## Update Sheet Data
update_gsheet <- function(sheet, data) {
  googlesheets4::sheet_append(ss = get("gsheetId"), data, sheet)
}

#------------------------------------------------------------#
#----- Sign In -----
#------------------------------------------------------------#

## App Authentication -----
safe_sign_in <- function(email, password) {
  tryCatch(
    {
      # Attempt to sign in
      login <- igniteR::sign_in_password(email, password)
      
      # Login to Google
      googleAuth()
      
      # Get User Status
      user <- googlesheets4::read_sheet(ss= gsheetId, sheet= "account_manager") %>%
        dplyr::filter(UUID == login$localId)
      
      # Return success result as a list
      list(
        success = TRUE,
        name = user$Name,
        role = user$Role,
        localId = login$localId,
        idToken = login$idToken,
        email = login$email,
        refreshToken = login$refreshToken,
        expiresIn = login$expiresIn
        
      )
    },
    error = function(e) {
      # Catch and handle errors triggered by stop in sign_in_password
      list(success = FALSE, error = paste("Sign-in failed:", conditionMessage(e)))
    }
  )
}

## Update Force Plate Sheets -----
updateForcePlates <- function() {
  lastSyncTime <- get_gsheet("Last Sync Time") %>%
    pull(lastHawkinSync)
  
  tryCatch({
    # Call Newest Tests
    newTests <- get_tests(sync = TRUE, from = lastSyncTime)
    
    #--------------------------------------------------#
    # Clean and Store CMJ
    #--------------------------------------------------#
    if(nrow(newTests) > 0) {
      if(any(str_detect(newTests$testType_name, "Countermovement Jump"))) {
        cmj_clean <- newTests %>%
          filter(str_detect(testType_name, "Countermovement Jump")) %>%
          transmute(
            testId = id,
            timestamp = timestamp,
            date = format(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"),
            type = testType_name,
            tags = testType_tags_name,
            name = athlete_name,
            athleteId = athlete_id,
            teams = sapply(athlete_teams, function(x) if (is.null(x)) NA else paste(x, collapse = ",")),
            groups = sapply(athlete_groups, function(x) if (is.null(x)) NA else paste(x, collapse = ",")), 
            active = athlete_active, 
            email = athlete_email, 
            position = athlete_position,
            class = athlete_class,
            sport = athlete_sport,
            jump_height_in = jumpHeight * 39.3701,
            l_r_peak_landing_force = lrPeakLandingForce,
            l_r_peak_propulsive_force = lrPeakPropulsiveForce,
            avg_prop_velocity = avgPropulsiveVelocity
          )
        
        update_gsheet("CMJ", cmj_clean)
        #updateVar(cmj_clean, "cmjData")
      }
      
      #--------------------------------------------------#
      # Clean and Store SJ
      #--------------------------------------------------#
      if(any(str_detect(newTests$testType_name, "Squat Jump"))) {
        sj_clean <- if(!any(str_detect(newTests$testType_name, "Squat Jump")))
          newTests %>%
          filter(str_detect(testType_name, "Squat Jump")) %>%
          mutate(jump_height_in = jumpHeight * 39.3701,
                 date = format(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")) %>%
          rowwise() %>%
          mutate(
            recent_cmj = cmj_clean %>%
              filter(athleteId == athleteId, date <= date) %>%  # Match athlete and filter by date
              arrange(desc(date)) %>%  # Sort by the most recent date
              slice(1) %>%
              pull(jump_height_in),  # Get the most recent cmj jump height
            eur = recent_cmj / jump_height_in  # Calculate the eur ratio
          ) %>%
          ungroup() %>%
          transmute(
            testId = id,
            timestamp = timestamp,
            date = date,
            type = testType_name,
            tags = testType_tags_name,
            name = athlete_name,
            athleteId = athlete_id,
            teams = sapply(athlete_teams, function(x) if (is.null(x)) NA else paste(x, collapse = ",")),
            groups = sapply(athlete_groups, function(x) if (is.null(x)) NA else paste(x, collapse = ",")), 
            active = athlete_active, 
            email = athlete_email, 
            position = athlete_position,
            class = athlete_class,
            sport = athlete_sport,
            jump_height_in = jump_height_in,
            eur = eur
          )
        
        update_gsheet("Squat Jump", sj_clean)
        #updateVar(sj_clean, "sjData")
      }
      
      #--------------------------------------------------#
      # Clean and Store MR
      #--------------------------------------------------#
      if(any(str_detect(newTests$testType_name, "Multi Rebound"))) {
        multiReb_clean <- newTests %>%
          filter(str_detect(testType_name, "Multi Rebound")) %>%
          transmute(
            testId = id,
            timestamp = timestamp,
            date = format(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"),
            type = testType_name,
            tags = testType_tags_name,
            name = athlete_name,
            athleteId = athlete_id,
            teams = sapply(athlete_teams, function(x) if (is.null(x)) NA else paste(x, collapse = ",")),
            groups = sapply(athlete_groups, function(x) if (is.null(x)) NA else paste(x, collapse = ",")), 
            active = athlete_active, 
            email = athlete_email, 
            position = athlete_position,
            class = athlete_class,
            sport = athlete_sport,
            peakRSI = peakRsi,
            top3_avgRSI = top3AvgMRsi,
            top5_avgRSI = top5AvgMRsi
          )
        
        update_gsheet("Multi Rebound", multiReb_clean)
        #updateVar(multiReb_clean, "mrData")
      }
      
      #--------------------------------------------------#
      # Update Last Sync Time
      #--------------------------------------------------#
      lastSyncTime <- data.frame(
        lastHawkinSync = c(round(as.numeric(Sys.time()), 0))
      )
      write_sheet(lastSyncTime, ss = gsheetId, sheet = "Last Sync Time")
      
      # Save Data
      #saveVars()
    }
  }, error = function(e) {
    print(e)
  })
  
}

#------------------------------------------------------------#
#----- Roster Management -----
#------------------------------------------------------------#
add_athlete <- function(df, teamsDF, groupsDF = NULL) {
  
  # Convert Team names to Ids
  teams <- replace_names_with_ids(df$teams, teamsDF)
  
  # Check to Convert Groups
  if(!is.null(groupsDF)) {
    # Convert Group names to Ids
    groups <- replace_names_with_ids(df$groups, groupsDF)
  } else {
    groups <- df$groups
  }
  # Create athlete object
  df <- data.frame(
    name = df$name,
    image = ifelse("image" %in% colnames(df), df$image, NULL),
    active = df$active,
    teams = teams,
    groups = groups,
    email = df$email,
    position = df$position,
    class = df$class,
    sport = df$sport,
    updated = df$updated
  )
  
  # Add athlete to Hawkin Dynamics
  hd_athlete <- hawkinR::create_athletes(
    athleteData = df
  )
  
  # Return the athlete object
  hd_athlete
}

update_athlete <- function(df) {
  # Create athlete object
  df <- data.frame(
    id = df$id,
    name = df$name,
    image = ifelse("image" %in% colnames(df), df$image, NULL),
    active = df$active,
    teams = df$teams,
    groups = df$groups,
    email = df$email,
    position = df$position,
    class = df$class,
    sport = df$sport,
    updated = df$updated
  )
  
  # Add athlete to Hawkin Dynamics
  hd_athlete <- hawkinR::update_athletes(
    athleteData = df
  )
  
  # Return the athlete object
  hd_athlete
}

syncRosters <- function() {
  # 1. Get Hawkin Roster and Clean
  hdRoster <- hawkinR::get_athletes() %>%
    mutate(
      teams = sapply(teams, function(x) if (is.null(x)) NA else paste(x, collapse = ",")),
      groups = sapply(groups, function(x) if (is.null(x)) NA else paste(x, collapse = ","))
    )
  
  # 3. Compare Rosters
  ## New Athletes in Hawkin Roster
  newHawkinDiff <- hdRoster %>%
    anti_join(get("roster"), by = c("id", "updated"))
  
  ## Updates Atheltes in Saved Roster
  newRosterDiff <- get("roster") %>%
    anti_join(hdRoster, by = c("id", "updated"))
  
  # 4. If Updated in Roster Var -> Update Hawkin Roster
  if (nrow(newRosterDiff) > 0) {
    
    # Add New Athletes to Hawkin Roster
    update_athlete(newRosterDiff)
    
    # Reload HD Roster after Update
    hdRoster <- hawkinR::get_athletes() %>%
      mutate(
        teams = sapply(teams, function(x) if (is.null(x)) NA else paste(x, collapse = ",")),
        groups = sapply(groups, function(x) if (is.null(x)) NA else paste(x, collapse = ","))
      )
    
    # Check Again for Hawkin Roster Differences
    newHawkinDiff <- hdRoster %>%
      anti_join(newRosterDiff, by = c("id", "updated"))
  }
  
  # 5. If Updated/New in Hawkin -> Update Google Roster
  if (nrow(newHawkinDiff) > 0) {
    updateVar(newHawkinDiff, "roster")
  }
  
  saveVars()
}

replace_names_with_ids <- function(teamValues, label_df) {
  # Iterate over each row in the input vector
  sapply(teamValues, function(item) {
    # Split the string into individual proper names
    names_split <- strsplit(item, ",\\s*")[[1]]
    
    # Replace each proper name with its corresponding ID
    ids <- sapply(names_split, function(name) {
      # Match the name to the proper_name column and get the corresponding ID
      matched_id <- mapping_df$id[match(name, mapping_df$proper_name)]
      if (is.na(matched_id)) {
        # Show error alert
        stop(show_alert(
          title = "Error",
          text = paste("Failed to upload bulk add athletes. Provided teams not found in the roster: ", name),
          type = "error",
          btn_labels = NULL,
          closeOnClickOutside = TRUE,
          showCloseButton = TRUE
        ))
      } else {
        matched_id
      }
    })
    
    # Recombine the IDs into a comma-separated string
    paste(ids, collapse = ", ")
  })
}

#------------------------------------------------------------#
#----- Test Data -----
#------------------------------------------------------------#
get_btg_data <- function() {
  # List files in the folder (replace "Folder_ID" with your actual folder ID)
  #folder_id <- Sys.getenv("BACKUP_FOLDER_ID")
  
  # Download all .RData files in the folder
  #file <- "backup_env_vars.RData"
  #drive_download(file = file, path = file, overwrite = TRUE)
  #load(file)
  
  # Download all .RData files in the folder
  #file <- "backup_data.RData"
  #drive_download(file = file, path = file, overwrite = TRUE)
  #load(file)
  
  assign("cmjData", get_gsheet("CMJ"))
  assign("sjData", get_gsheet("Squat Jump"))
  assign("mrData", get_gsheet("Multi Rebound"))
  assign("laneAgilityData", get_gsheet("Pro Lane Agility"))
  assign("fiveTenFiveData", get_gsheet("5-10-5 Agility"))
  assign("threeQuarterData", get_gsheet("3/4 Court Sprint"))
  assign("anthroData", get_gsheet("Anthropometrics"))
  assign("broadData", get_gsheet("Broad Jump"))
  assign("fortyData", get_gsheet("40 Yard Sprint"))
  assign("vertData", get_gsheet("Vertical Jump"))
}

#------------------------------------------------------------#
#----- Filter Test Dates -----
#------------------------------------------------------------#
filter_dates <- function(athleteName) {
  ath <- athleteName
  
  # Get Test Dates for Selected Athlete
  cmjDates <- cmjData %>% filter(name == ath) %>% pull(date)
  sjDates <- sjData %>% filter(name == ath) %>% pull(date)
  mrDates <- mrData %>% filter(name == ath) %>% pull(date)
  laneAgilityDates <- laneAgilityData %>% filter(name == ath) %>% pull(date)
  fiveTenFiveDates <- fiveTenFiveData %>% filter(name == ath) %>% pull(date)
  threeQDates <- threeQuarterData %>% filter(name == ath) %>% pull(date)
  anthroDates <- anthroData %>% filter(name == ath) %>% pull(date)
  broadDates <- broadData %>% filter(name == ath) %>% pull(date)
  fortyDates <- fortyData %>% filter(name == ath) %>% pull(date)
  vertDates <- vertData %>% filter(name == ath) %>% pull(date)
  
  # Get Unique Dates
  uniqueDates <- unique(c(cmjDates, sjDates, mrDates, laneAgilityDates, fiveTenFiveDates, threeQDates, anthroDates, broadDates, fortyDates, vertDates))
  
  return(uniqueDates)
}

filterCMJ <- function(athleteName, date) {
  tryCatch({
    # Check if the dataset exists
    if (!exists("cmjData")) {
      stop("Dataset 'cmjData' does not exist")
    }
    
    # Get the dataset and filter it
    df <- get("cmjData") %>%
      filter(name == athleteName, date == date)
    
    # Check if the filter returns no rows
    if (nrow(df) == 0) {
      stop("No matching records found for the specified athlete and date")
    }
    
    # Perform summarization
    result <- df %>%
      summarise(
        avg_jh = mean(jump_height_in, na.rm = TRUE),  # Handle NA values
        max_jh = max(jump_height_in, na.rm = TRUE),  # Handle NA values
        avg_landing_asymm = mean(l_r_peak_landing_force, na.rm = TRUE),  # Handle NA values
        max_landing_asymm = l_r_peak_landing_force[which.max(abs(l_r_peak_landing_force))], # Max absolute landing asymmetry
        avg_prop_asymm = mean(l_r_peak_propulsive_force, na.rm = TRUE),  # Handle NA values
        max_prop_asymm = l_r_peak_propulsive_force[which.max(abs(l_r_peak_propulsive_force))], # Max absolute propulsive asymmetry
        avg_prop_velo = mean(avg_prop_velocity, na.rm = TRUE),  # Handle NA values
        max_prop_velo = max(avg_prop_velocity, na.rm = TRUE)   # Handle NA values
      )
    
    return(result)
  }, error = function(e) {
    # Print the error message
    message("Error: ", e$message)
    # Optionally return an empty tibble with the expected structure
    return(tibble(
      avg_jh = numeric(),
      max_jh = numeric(),
      avg_landing_asymm = numeric(),
      max_landing_asymm = numeric(),
      avg_prop_asymm = numeric(),
      max_prop_asymm = numeric(),
      avg_prop_velo = numeric(),
      max_prop_velo = numeric()
    ))
  })
}

filterSJ <- function(athleteName, date) {
  tryCatch({
    # Check if the dataset exists
    if (!exists("sjData")) {
      stop("Dataset 'sjData' does not exist")
    }
    
    # Get the dataset and filter it
    df <- get("sjData") %>%
      filter(name == athleteName, date == date)
    
    # Check if the filter returns no rows
    if (nrow(df) == 0) {
      stop("No matching records found for the specified athlete and date")
    }
    
    # Perform summarization
    result <- df %>%
      summarise(
        avg_jh = mean(jump_height_in, na.rm = TRUE), # Handle NA values
        max_jh = max(jump_height_in, na.rm = TRUE), # Handle NA values
        avg_eur = mean(eur, na.rm = TRUE),          # Handle NA values
        max_eur = max(eur, na.rm = TRUE)           # Handle NA values
      )
    
    return(result)
  }, error = function(e) {
    # Print the error message
    message("Error: ", e$message)
    # Optionally return an empty tibble with the expected structure
    return(tibble(
      avg_jh = numeric(),
      max_jh = numeric(),
      avg_eur = numeric(),
      max_eur = numeric()
    ))
  })
}

filterMR <- function(athleteName, date) {
  tryCatch({
    # Check if the dataset exists
    if (!exists("mrData")) {
      stop("Dataset 'mrData' does not exist")
    }
    
    # Get the dataset and filter it
    df <- get("mrData") %>%
      filter(name == athleteName, date == date)
    
    # Check if the filter returns no rows
    if (nrow(df) == 0) {
      stop("No matching records found for the specified athlete and date")
    }
    
    # Perform summarization
    result <- df %>%
      summarise(
        avg_peak_rsi = mean(peakRSI, na.rm = TRUE),  # Handle NA values
        max_peak_rsi = max(peakRSI, na.rm = TRUE),  # Handle NA values
        avg_top3_rsi = mean(top3_avgRSI, na.rm = TRUE),  # Handle NA values
        max_top3_rsi = max(top3_avgRSI, na.rm = TRUE),  # Handle NA values
        avg_top5_rsi = mean(top5_avgRSI, na.rm = TRUE),  # Handle NA values
        max_top5_rsi = max(top5_avgRSI, na.rm = TRUE)   # Handle NA values
      )
    
    return(result)
  }, error = function(e) {
    # Print the error message
    message("Error: ", e$message)
    # Optionally return an empty tibble with the expected structure
    return(tibble(
      avg_peak_rsi = numeric(),
      max_peak_rsi = numeric(),
      avg_top3_rsi = numeric(),
      max_top3_rsi = numeric(),
      avg_top5_rsi = numeric(),
      max_top5_rsi = numeric()
    ))
  })
}

filterLaneAgility <- function(athleteName, date) {
  tryCatch({
    # Check if the dataset exists
    if (!exists("laneAgilityData")) {
      stop("Dataset 'laneAgilityData' does not exist")
    }
    
    # Get the dataset and filter it
    df <- get("laneAgilityData") %>%
      filter(name == athleteName, date == date)
    
    # Check if the filter returns no rows
    if (nrow(df) == 0) {
      stop("No matching records found for the specified athlete and date")
    }
    
    # Perform summarization
    result <- df %>%
      summarise(
        avg_time = mean(time, na.rm = TRUE),  # Handle NA values
        min_time = min(time, na.rm = TRUE)   # Handle NA values
      )
    
    return(result)
  }, error = function(e) {
    # Print the error message
    message("Error: ", e$message)
    # Optionally return an empty tibble with the expected structure
    return(tibble(
      avg_time = numeric(),
      min_time = numeric()
    ))
  })
}

filterFiveTenFive <- function(athleteName, date) {
  tryCatch({
    # Check if the dataset exists
    if (!exists("fiveTenFiveData")) {
      stop("Dataset 'fiveTenFiveData' does not exist")
    }
    
    # Get the dataset and filter it
    df <- get("fiveTenFiveData") %>%
      filter(name == athleteName, date == date)
    
    # Check if the filter returns no rows
    if (nrow(df) == 0) {
      stop("No matching records found for the specified athlete and date")
    }
    
    # Perform grouping and summarization
    result <- df %>%
      group_by(direction) %>%
      summarise(
        avg_time = mean(time, na.rm = TRUE), # Handle NA values
        min_time = min(time, na.rm = TRUE)  # Handle NA values
      )
    
    return(result)
  }, error = function(e) {
    # Print the error message
    message("Error: ", e$message)
    # Optionally return an empty data frame with the expected structure
    return(tibble(
      direction = character(),
      avg_time = numeric(),
      min_time = numeric()
    ))
  })
}

filterThreeQuarter <- function(athleteName, date) {
  tryCatch({
    # Check if the dataset exists
    if (!exists("threeQuarterData")) {
      stop("Dataset 'threeQuarterData' does not exist")
    }
    
    # Get the dataset and filter it
    df <- get("threeQuarterData") %>%
      filter(name == athleteName, date == date)
    
    # Check if the filter returns no rows
    if (nrow(df) == 0) {
      stop("No matching records found for the specified athlete and date")
    }
    
    # Perform summarization
    result <- df %>%
      summarise(
        avg_time = mean(time, na.rm = TRUE),  # Handle NA values
        min_time = min(time, na.rm = TRUE)   # Handle NA values
      )
    
    return(result)
  }, error = function(e) {
    # Print the error message
    message("Error: ", e$message)
    # Optionally return an empty tibble with the expected structure
    return(tibble(
      avg_time = numeric(),
      min_time = numeric()
    ))
  })
}

filterAnthro <- function(athleteName, date) {
  tryCatch({
    # Check if the dataset exists
    if (!exists("anthroData")) {
      stop("Dataset 'anthroData' does not exist")
    }
    
    # Get the dataset and filter it
    df <- get("anthroData") %>%
      filter(name == athleteName, date == date)
    
    # Check if the filter returns no rows
    if (nrow(df) == 0) {
      stop("No matching records found for the specified athlete and date")
    }
    
    # Perform summarization
    result <- df %>%
      summarise(
        height = mean(height, na.rm = TRUE),     # Handle NA values
        wingspan = mean(wingspan, na.rm = TRUE), # Handle NA values
        reach = mean(reach, na.rm = TRUE)        # Handle NA values
      )
    
    return(result)
  }, error = function(e) {
    # Print the error message
    message("Error: ", e$message)
    # Optionally return an empty tibble with the expected structure
    return(tibble(
      height = numeric(),
      wingspan = numeric(),
      reach = numeric()
    ))
  })
}

filterBroad <- function(athleteName, date) {
  tryCatch({
    # Check if the dataset exists
    if (!exists("broadData")) {
      stop("Dataset 'broadData' does not exist")
    }
    
    # Get the dataset and filter it
    df <- get("broadData") %>%
      filter(name == athleteName, date == date)
    
    # Check if the filter returns no rows
    if (nrow(df) == 0) {
      stop("No matching records found for the specified athlete and date")
    }
    
    # Perform summarization
    result <- df %>%
      summarise(
        dist_in = mean(distance_in, na.rm = TRUE),                   # Handle NA values
        dist_ft_in = paste0(mean(distance_in, na.rm = TRUE) %/% 12, # Convert to feet and inches
                            "'", 
                            round(mean(distance_in, na.rm = TRUE) %% 12), 
                            "\"")
      )
    
    return(result)
  }, error = function(e) {
    # Print the error message
    message("Error: ", e$message)
    # Optionally return an empty tibble with the expected structure
    return(tibble(
      dist_in = numeric(),
      dist_ft_in = character()
    ))
  })
}

filterForty <- function(athleteName, date) {
  tryCatch({
    # Check if the dataset exists
    if (!exists("fortyData")) {
      stop("Dataset 'fortyData' does not exist")
    }
    
    # Get the dataset and filter it
    df <- get("fortyData") %>%
      filter(name == athleteName, date == date)
    
    # Check if the filter returns no rows
    if (nrow(df) == 0) {
      stop("No matching records found for the specified athlete and date")
    }
    
    # Perform summarization
    result <- df %>%
      summarise(
        mean_split10 = mean(time_10y, na.rm = TRUE),  # Handle NA values
        min_split10 = min(time_10y, na.rm = TRUE),   # Handle NA values
        mean_split20 = mean(time_20y, na.rm = TRUE),  # Handle NA values
        min_split20 = min(time_20y, na.rm = TRUE),   # Handle NA values
        mean_split40 = mean(time_40y, na.rm = TRUE),  # Handle NA values
        min_split40 = min(time_40y, na.rm = TRUE)   # Handle NA values
      )
    
    return(result)
  }, error = function(e) {
    # Print the error message
    message("Error: ", e$message)
    # Optionally return an empty tibble with the expected structure
    return(tibble(
      mean_split10 = numeric(),
      min_split10 = numeric(),
      mean_split20 = numeric(),
      min_split20 = numeric(),
      mean_split40 = numeric(),
      min_split40 = numeric()
    ))
  })
}

filterVert <- function(athleteName, date) {
  tryCatch({
    # Check if the dataset exists
    if (!exists("vertData")) {
      stop("Dataset 'vertData' does not exist")
    }
    
    # Get the dataset and filter it
    df <- get("vertData") %>%
      filter(name == athleteName, date == date)
    
    # Check if the filter returns no rows
    if (nrow(df) == 0) {
      stop("No matching records found for the specified athlete and date")
    }
    
    # Perform summarization
    result <- df %>%
      summarise(
        height_in = mean(height_in, na.rm = TRUE),              # Handle NA values
        height_ft_in = paste0(
          mean(height_in, na.rm = TRUE) %/% 12, "'",            # Convert to feet
          round(mean(height_in, na.rm = TRUE) %% 12), "\""      # Convert to inches
        )
      )
    
    return(result)
  }, error = function(e) {
    # Print the error message
    message("Error: ", e$message)
    # Optionally return an empty tibble with the expected structure
    return(tibble(
      height_in = numeric(),
      height_ft_in = character()
    ))
  })
}

