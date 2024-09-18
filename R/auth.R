# Firebase API Configuration
projName <- "bridge-the-gap-dash"
projId <- "btg-dash-c1bf1"
projNumber <- "676161938393"

firebase_api_key <- "AIzaSyCxW_YJiA5belNxDBnksF-uD1yP9B2vSMc"
firebase_auth_url <- paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=", firebase_api_key)

# Function to authenticate with Firebase
authenticate_user <- function(email, password) {
  response <- POST(
    url = firebase_auth_url,
    body = toJSON(list(
      email = email,
      password = password,
      returnSecureToken = TRUE
    ), auto_unbox = TRUE),
    encode = "json",
    content_type_json()
  )

  if (status_code(response) == 200) {
    content <- as.data.frame(content(response, as = "parsed"))

    df <- content %>%
      select(localId, email, refreshToken) %>%
      mutate(expires = as.character(ceiling(as.numeric(Sys.time()) + 3600)))

    return(df)  # Return the authentication token
  } else {
    stop("Authentication failed. Please check your credentials.")
  }
}

#--------------------------------------------------------------------------------------------------#
# Test Authentication -----
#--------------------------------------------------------------------------------------------------#
#test <- authenticate_user("user@example.com", "user-password")
