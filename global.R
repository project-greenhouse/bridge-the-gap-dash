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
# ----- Env Variables -----
#------------------------------------------------------------#

## Hawkin Dynamics
hdToken <- Sys.getenv("HD_TOKEN")

## Google Sheet
gsheetId <- Sys.getenv("GSHEET_ID")

#------------------------------------------------------------#
#----- Sign In -----
#------------------------------------------------------------#

## App Authentication
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
get_gsheet <- function(sheet, range) {
  googlesheets4::read_sheet(ss = gsheetId, sheet = sheet, range = range)
}

## Update Sheet Data
update_gsheet <- function(sheet, range, data) {
  googlesheets4::sheet_append(ss = gsheetId, data, sheet)
}

