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
#----- Update Force Plate Data -----
#------------------------------------------------------------#
updateForcePlates <- function() {
  # Get Last Sync Time
  googlesheets4::read_sheet(gsheetId, sheet = "Last Sync Time") %>%
    pull(lastHawkinSync) -> lastSyncTime
  
  tryCatch({
    # Call Newest Tests
    newTests <- get_tests(sync = TRUE, from = lastSyncTime)
    
    #--------------------------------------------------#
    # Clean and Store CMJ
    #--------------------------------------------------#
      
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
            jump_height_in = jump_height_m * 39.3701,
            l_r_peak_landing_force = l_r_peak_landing_force_percent,
            l_r_peak_propulsive_force = l_r_peak_propulsive_force_percent,
            avg_prop_velocity = avg_propulsive_velocity_m_s
          )
        
        googlesheets4::write_sheet(cmj_clean, ss = gsheetId,sheet =  "CMJ")
      }
    
    #--------------------------------------------------#
    # Clean and Store SJ
    #--------------------------------------------------#
    if(any(str_detect(newTests$testType_name, "Squat Jump"))) {
      sj_clean <- if(!any(str_detect(newTests$testType_name, "Squat Jump")))
        newTests %>%
        filter(str_detect(testType_name, "Squat Jump")) %>%
        mutate(jump_height_in = jump_height_m * 39.3701,
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
      
      googlesheets4::write_sheet(sj_clean, ss = gsheetId, sheet =  "Squat Jump")
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
          peakRSI = peak_rsi,
          top3_avgRSI = top_3_jumps_avg_rsi,
          top5_avgRSI = top_5_jumps_avg_rsi
        )
      
      googlesheets4::write_sheet(multiReb_clean, ss = gsheetId, sheet =  "Multi Rebound")
    }
    
    #--------------------------------------------------#
    # Update Last Sync Time
    #--------------------------------------------------#
    lastSyncTime <- data.frame(
      lastHawkinSync = round(as.numeric(Sys.time()),0)
    )
    
    googlesheets4::write_sheet(lastSyncTime, ss = gsheetId, sheet =  "Last Sync Time")
  }, error = function(e) {
    print(e)
  })
  
}


#------------------------------------------------------------#
#----- Admin UI in Sidebar -----
#------------------------------------------------------------#
output$sidebarUI <- renderUI({
  if (isTruthy(creds$loggedIn)) {
    sidebarMenu(
      id = "tabs",  # Use sidebarMenu ID for tab navigation
      tags$div(
        style = "height: 100%; display: flex; flex-direction: column;",
        # Sidebar Menu Items
        tags$div(
          style = "flex-grow: 1;",
          # Sidebar Menu Items
          tags$div(
            style = "flex-grow: 1;",
            menuItem("Home", tabName = "home", icon = icon("home"), selected = TRUE),
            menuItem("Roster", tabName = "roster", icon = icon("users")),
            menuItem("Tests", tabName = "tests", icon = icon("stopwatch")),
            if (!is.null(creds$role) && creds$role == "Admin") {
              menuItem("Manage Users", tabName = "admin", icon = icon("gear"))
            },
            # Logout Button
            tags$div(
              style = "padding: 10px; border-top: 1px solid #ccc; display: flex; justify-content: center; align-items: center; width: 100%;",
              actionBttn(
                inputId = "logout_btn", 
                label = "Logout",
                style = "unite",
                color = "danger",
                size = "md"
              )
            )
          )
        )
      )
    )
  } else {
    sidebarMenu(
      id = "tabs",
      tags$div(
        style = "height: 100%; display: flex; flex-direction: column;",
        # Sidebar Menu Items
        tags$div(
          style = "flex-grow: 1;",
          # Sidebar Menu Items
          tags$div(
            style = "flex-grow: 1;",
            menuItem("Login", tabName = "home", selected = TRUE, icon = icon("right-to-bracket"))  # Default tab for not logged-in users
          )
        )
      )
    )
  }
})

#------------------------------------------------------------#
#----- Update Force Plate Data -----
#------------------------------------------------------------#
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

#------------------------------------------------------------#
#----- Google Sheets Data -----
#------------------------------------------------------------#
## Get Sheet Data
get_gsheet <- function(sheet, range = NULL) {
  googlesheets4::read_sheet(ss = gsheetId, sheet = sheet, range = range)
}

## Update Sheet Data
update_gsheet <- function(sheet, data) {
  googlesheets4::sheet_append(ss = gsheetId, data, sheet)
}

## Update Athlete Data
syncRosters <- function() {
  # 1. Get Hawkin Roster and Clean
  hdRoster <- hawkinR::get_athletes() %>%
    mutate(
      teams = sapply(teams, function(x) if (is.null(x)) NA else paste(x, collapse = ",")),
      groups = sapply(groups, function(x) if (is.null(x)) NA else paste(x, collapse = ","))
    )
  
  # 2. Get Google Roster and Clean
  gRoster <- get_gsheet(sheet = "Roster")
  
  # 3. Compare Rosters
  ## New Athletes in Hawkin Roster
  newHawkinDiff <- hdRoster %>%
    anti_join(gRoster, by = c("id", "updated"))
  
  ## Updates Atheltes in Google Roster
  newGoogleDiff <- gRoster %>%
    anti_join(hdRoster, by = c("id", "updated"))
  
  # 4. If Updated in Google -> Update Hawkin Roster
  if (nrow(newGoogleDiff) > 0) {
    
    # Add New Athletes to Hawkin Roster
    update_athlete(newGoogleDiff)
    
    # Reload HD Roster after Update
    hdRoster <- hawkinR::get_athletes() %>%
      mutate(
        teams = sapply(teams, function(x) if (is.null(x)) NA else paste(x, collapse = ",")),
        groups = sapply(groups, function(x) if (is.null(x)) NA else paste(x, collapse = ","))
      )
    
    # Check Again for Hawkin Roster Differences
    newHawkinDiff <- hdRoster %>%
      anti_join(gRoster, by = c("id", "updated"))
  }
  
  # 5. If Updated/New in Hawkin -> Update Google Roster
  if (nrow(newHawkinDiff) > 0) {
    googlesheets4::write_sheet(hdRoster, ss = gsheetId, "Roster")
  }
}

#------------------------------------------------------------#
##-----| Admin Page -----
#------------------------------------------------------------#
adminScreen <- reactiveVal("choices")

#------------------------------------------------------------#
###----- Admin Page Logic -----
#------------------------------------------------------------#

# Return to Admin Choices
observeEvent(input$admin_return, {
  adminScreen("choices")
})

# Return to Admin Choices
observeEvent(input$backToAdminSelect, {
  adminScreen("choices")
})

# Return to Home
observeEvent(input$backToHome, {
  updateTabItems(session, "tabs", "home")
})
#--------------------------------------------------#
####----- Add Athlete -----
#--------------------------------------------------#

# Go to Add Athlete Page
observeEvent(input$admin_addAthlete, {
  adminScreen("addAthlete")
})

# Submit New Athlete Action
observeEvent(input$addAthleteSubmit, {
  
  tryCatch({
    # Create New Athlete Data Frame
    newAthlete <- data.frame(
      name = input$addAthleteName,
      email = input$addAthleteEmail,
      active = input$addAthleteActive,
      teams = paste(input$addAthleteTeams, collapse = ","),
      groups = paste(input$addAthleteGroups, collapse = ","),
      position = paste(input$addAthletePos, collapse = ","),
      class = paste(input$addAthleteClass, collapse = ","),
      sport = paste(input$addAthleteSport, collapse = ",")
    )
    
    # Upload new Athlete
    add_athlete(newAthlete)
    
    syncRosters()
    
    # Add new sport to sportList
    if(!any(str_detect(input$addAthleteSport, sportList))) {
      googlesheets4::sheet_append(data.frame(sport = c(input$addAthleteSport)), 
                                  ss = gsheetId, 
                                  sheet = "sportList")
    }
    
    # Add new position to posList
    if(!any(str_detect(input$addAthletePos, posList))) {
      googlesheets4::sheet_append(data.frame(sport = c(input$addAthletePos)), 
                                  ss = gsheetId, 
                                  sheet = "posList")
    }
    
    # Add new class to classList
    if(!any(str_detect(input$addAthleteClass, classList))) {
      googlesheets4::sheet_append(data.frame(sport = c(input$addAthleteClass)), 
                                  ss = gsheetId, 
                                  sheet = "classList")
    }
  } , error = function(e) {
    # Show error alert
    show_alert(
      title = "Error",
      text = paste("Failed to add athlete:", conditionMessage(e)),
      type = "error",
      btn_labels = "Back to Admin",
      btn_colors = "light-blue",
      closeOnClickOutside = TRUE,
      showCloseButton = TRUE
    )
  })
})

#--------------------------------------------------#
####----- Bulk Athlete -----
#--------------------------------------------------#

# Go to Bulk Athletes Page
observeEvent(input$admin_bulkAtheletes, {
  adminScreen("bulkAthletes")
})

# Download Bulk Athlete Template
output$bulkTemplateDownload <- downloadHandler(
  filename = function() {
    paste("bulk_athlete_template", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(
      data.frame(
        name = "First Last",
        image = "https://www.baseball-reference.com/req/202412180/images/headshots/9/957d4da0_sabr.jpg",
        active = "TRUE",
        teams = "team1,team2,team3",
        groups = "group1,group2,group3",
        email = "email@emailer.com",
        position = "guard, forward, center",
        class = "freshman",
        sport = "sport1,sport2,sport3"
      )
    )
  }
)

# Submit Bulk Athlete Action
observeEvent(input$bulkSubmit, {
  file <- input$bulkSubmit
  ext <- tools::file_ext(file$datapath)
  
  req(file)
  validate(need(ext == "csv", "Please upload a csv file"))
  
  tryCatch({
    # Read in the uploaded CSV file
    bulkData <- read.csv(input$bulkUpload$datapath)
    
    # Add athletes to the roster
    add_athlete(bulkData)
    
    syncRosters()
    
    # Add new sport to sportList
    if(!any(str_detect(input$addAthleteSport, sportList))) {
      googlesheets4::sheet_append(data.frame(sport = c(input$addAthleteSport)), 
                                  ss = gsheetId, 
                                  sheet = "sportList")
    }
    
    # Add new position to posList
    if(!any(str_detect(input$addAthletePos, posList))) {
      googlesheets4::sheet_append(data.frame(sport = c(input$addAthletePos)), 
                                  ss = gsheetId, 
                                  sheet = "posList")
    }
    
    # Add new class to classList
    if(!any(str_detect(input$addAthleteClass, classList))) {
      googlesheets4::sheet_append(data.frame(sport = c(input$addAthleteClass)), 
                                  ss = gsheetId, 
                                  sheet = "classList")
    }
    
    # Show error alert
    show_alert(
      title = "Success!",
      text = paste(nrow(bulkData),"athletes added successfully."),
      type = "success",
      actionBttn(
        inputId = "admin_return",
        label = "Return to Admin",
        icon = icon("person-circle-plus"),
        style = "pill",
        color = "primary",
        block = TRUE,
        size = "lg"
      ),
      closeOnClickOutside = TRUE,
      showCloseButton = TRUE
    )
  }, error = function(e) {
    # Show error alert
    show_alert(
      title = "Error",
      text = paste("Failed to bulk add athletes:", conditionMessage(e)),
      type = "error",
      btn_labels = "Back to Admin",
      btn_colors = "light-blue",
      closeOnClickOutside = TRUE,
      showCloseButton = TRUE
    )
  })
})

#--------------------------------------------------#
####----- Add Coach -----
#--------------------------------------------------#

observeEvent(input$admin_addCoach, {
  adminScreen("addCoach")
})

#--------------------------------------------------#
####----- Edit Coach -----
#--------------------------------------------------#

observeEvent(input$admin_editCoach, {
  adminScreen("editCoach")
})

#--------------------------------------------------#
####----- Delete Coach -----
#--------------------------------------------------#

observeEvent(input$admin_deleteCoach, {
  adminScreen("deleteCoach")
})

#------------------------------------------------------------#
###----- Admin Page UI -----
#------------------------------------------------------------#

output$adminScreen <- renderUI({
  req(creds$role == "Admin")
  
  acntManager <- reactive({
    get_gsheet(sheet = "account_manager")
  })
  
  fluidPage(
    title = "Admin Page",
    fluidRow(
      actionLink("backToHome", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
    ),
    if (adminScreen() == "choices") {
      #--------------------------------------------------#
      ####----- Choices Page UI -----
      #--------------------------------------------------#
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          h1("Admin Options"),
          # Buttons with consistent spacing
          div(
            actionBttn(
              inputId = "admin_addAthlete",
              label = "Add Athlete",
              icon = icon("person-circle-plus"),
              style = "pill",
              color = "primary",
              block = TRUE,
              size = "lg"
            ),
            style = "margin-bottom: 15px; width: 100%; text-align: center;"
          ),
          div(
            actionBttn(
              inputId = "admin_bulkAtheletes",
              label = "Add Multiple Athletes",
              icon = icon("users-viewfinder"),
              style = "pill",
              color = "primary",
              block = TRUE,
              size = "lg"
            ),
            style = "margin-bottom: 15px; width: 100%; text-align: center;"
          ),
          div(
            actionBttn(
              inputId = "admin_addCoach",
              label = "Add Coach",
              icon = icon("user-plus"),
              style = "pill",
              color = "primary",
              block = TRUE,
              size = "lg"
            ),
            style = "margin-bottom: 15px; width: 100%; text-align: center;"
          ),
          div(
            actionBttn(
              inputId = "admin_editCoach",
              label = "Edit User",
              icon = icon("user-pen"),
              style = "pill",
              color = "primary",
              block = TRUE,
              size = "lg"
            ),
            style = "margin-bottom: 15px; width: 100%; text-align: center;"
          ),
          div(
            actionBttn(
              inputId = "admin_deleteCoach",
              label = "Delete Coach",
              icon = icon("user-xmark"),
              style = "pill",
              color = "danger",
              block = TRUE,
              size = "lg"
            ),
            style = "margin-bottom: 15px; width: 100%; text-align: center;"
          )
        ),
        column(width = 3)
      )
    } else if (adminScreen() == "addAthlete") {
      #--------------------------------------------------#
      ####----- Add Athlete Page UI -----
      #--------------------------------------------------#
      
      fluidRow(
        fluidRow(
          actionLink("backToAdmin", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;")
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("Add Athlete"),
            # name
            textInput("addAthleteName", "Name", placeholder = "First Last"),
            # email
            textInput("addAthleteEmail", "Email", placeholder = "email@email.com"),
            # active
            checkboxInput("addAthleteActive", "Active", value = TRUE),
            # teams
            virtualSelectInput(
              inputId = "addAthleteTeams",
              label = "Teams:",
              multiple = TRUE,
              choices = setNames(teams()$proper_name, teams()$id),
              showValueAsTags = TRUE, 
              width = "100%",
              dropboxWrapper = "body"
            ),
            # groups
            virtualSelectInput(
              inputId = "addAthleteGroups",
              label = "Groups:", 
              choices = c("Group 1", "Group 2", "Group 3"),
              showValueAsTags = TRUE, 
              multiple = TRUE,
              width = "100%",
              dropboxWrapper = "body"
            ),
            # position
            virtualSelectInput(
              inputId = "addAthletePos",
              label = "Multiple :", 
              choices = posList,
              multiple = TRUE,
              allowNewOption = TRUE,
              showValueAsTags = TRUE, 
              width = "100%", 
              dropboxWrapper = "body"
            ),
            # class
            virtualSelectInput(
              inputId = "addAthleteClass",
              label = "Class :", 
              choices = classList,
              multiple = TRUE,
              allowNewOption = TRUE,
              showValueAsTags = TRUE, 
              width = "100%", 
              dropboxWrapper = "body"
            ),
            # sport
            virtualSelectInput(
              inputId = "addAthleteSport",
              label = "Sport :", 
              choices = sportList,
              multiple = TRUE,
              allowNewOption = TRUE,
              showValueAsTags = TRUE, 
              width = "100%", 
              dropboxWrapper = "body"
            ),
            fluidRow(
              column(
                width = 6,
                actionBttn(
                  inputId = "addAthleteSubmit",
                  label = "Submit",
                  style = "pill",
                  color = "primary",
                  size = "lg"
                )
              ),
              column(
                width = 6,
                actionBttn(
                  inputId = "admin_return",
                  label = "Return",
                  style = "pill",
                  color = "secondary",
                  size = "lg"
                )
              )
            )
          ),
          column(width = 3)
        )
      )
    } else if (adminScreen() == "bulkAthletes") {
      #--------------------------------------------------#
      ####----- Bulk Add Page UI -----
      #--------------------------------------------------#
      fluidRow(
        fluidRow(
          actionLink("backToAdmin", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;")
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("Bulk Athletes"),
            # Instructions
            fluidRow(
              box(
                title = "Instructions",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                tags$p("Download the csv template and fill in the athlete information. Then upload the completed csv file."),
                # Download CSV Template
                actionLink("bulkTemplateDownload", "Download CSV Template", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
                br(), br(),
                # Upload CSV File
                fileInput(
                  inputId = "bulkUpload", 
                  label = "Upload CSV File", 
                  multiple = FALSE, 
                  accept = ".csv",
                  buttonLabel = "Browse",
                  placeholder = "e.g. your_athletes.csv"
                ),
                br(), br(),
                # Submit Button
                actionBttn(
                  inputId = "bulkSubmit",
                  label = "Submit",
                  style = "pill",
                  color = "primary",
                  size = "lg"
                )
              )
            )
          ),
          column(width = 3)
        )
      )
    } else if (adminScreen() == "addCoach") {
      #--------------------------------------------------#
      ####----- Add Coach Page UI -----
      #--------------------------------------------------#
      fluidRow(
        fluidRow(
          actionLink("backToAdmin", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;")
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("Add Coach")
          ),
          column(width = 3)
        )
      )
    } else if (adminScreen() == "editCoach") {
      #--------------------------------------------------#
      ####----- Edit Coach Page UI -----
      #--------------------------------------------------#
      fluidRow(
        fluidRow(
          actionLink("backToAdmin", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;")
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("Edit Coach")
          ),
          column(width = 3)
        )
      )
    } else if (adminScreen() == "deleteCoach") {
      #--------------------------------------------------#
      ####----- Delete Coach Page UI -----
      #--------------------------------------------------#
      fluidRow(
        fluidRow(
          actionLink("backToAdmin", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;")
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("Delete Coach")
          ),
          column(width = 3)
        )
      )
    }
  )
})

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
        max_landing_asymm = l_r_peak_landing_force[which.max(abs(l_r_peak_landing_force), na.rm = TRUE)], # Max absolute landing asymmetry
        avg_prop_asymm = mean(l_r_peak_propulsive_force, na.rm = TRUE),  # Handle NA values
        max_prop_asymm = l_r_peak_propulsive_force[which.max(abs(l_r_peak_propulsive_force), na.rm = TRUE)], # Max absolute propulsive asymmetry
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