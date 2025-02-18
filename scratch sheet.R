# Reorder your source calls:
source("global.R")

hawkinR::get_access(Sys.getenv("HD_TOKEN"))

## Update Force Plate Sheets -----
updateForcePlates <- function(lastSync) {
  lastSyncTime <-lastSync %>% 
    select(lastHawkinSync) %>%
    pull()
  
  tryCatch({
    # Call Newest Tests
    newTests <- get_tests(sync = TRUE, from = 1735688657)
    
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
        
        update_gsheet("Countermovement Jump", cmj_clean)
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
        lastHawkinSync = c(round(as.numeric(Sys.time()), 0)),
        lastRosterSync = lastSync$lastRosterSync
      )
      write_sheet(lastSyncTime, ss = gsheetId, sheet = "Last Sync Time")
      
    }
  }, error = function(e) {
    print(e)
  })
  
}

roster <- roster %>% mutate(updated = as.numeric(Sys.time()))