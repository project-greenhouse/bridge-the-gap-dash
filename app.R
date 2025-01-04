# Reorder your source calls:
source("global.R")

#------------------------------------------------------------#
#----- UI -----
#------------------------------------------------------------#


#------------------------------------------------------------#
##----- Header -----
#------------------------------------------------------------#
header <- dashboardHeader(
  title = dashboardBrand(
    title = "Bridge The Gap",
    href = "https://www.btgphysicaltherapy.com/",
    image = "btg-logo-black-trans-950.png"
  ),
  titleWidth = "250px"
)

#------------------------------------------------------------#
##----- Footer -----
#------------------------------------------------------------#
footer <- dashboardFooter(
  tags$div(
    style = "display: flex; justify-content: center; align-items: center; height: 100%;",
    tags$p(
      "BTG Combine Testing App v1.0.0 | Powered by ",
      tags$a(
        href = "https://www.greenhouse-ps.com/",
        "Greenhouse Performance Solutions LLC"
      ),
      "."
    )
  )
)

#------------------------------------------------------------#
##----- Sidebar -----
#------------------------------------------------------------#
sidebar <- dashboardSidebar(
  width = 250,
  uiOutput("sidebarUI")
)

#------------------------------------------------------------#
##----- Body -----
#------------------------------------------------------------#
ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  header = header,
  sidebar = sidebar,
  footer = footer,
  
  dashboardBody(
    useWaiter(),  # Initialize waiter
    useShinyjs(),  # Initialize shinyjs
    # Define the loading screen for busy states
    waiter_on_busy(
      color = "white",  # Background color for loading screen
      html = tagList(
        tags$div(
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
          tags$img(
            src = "btg-logo-black-trans-950.png",
            style = "width: 150px; height: 150px; animation: spin 2s linear infinite;"
          ),
          tags$p(
            "Loading...",
            style = "font-size: 20px; color: #333; margin-top: 10px;"
          )
        ),
        tags$style(HTML("
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
      "))
      )
    ),
    tabItems(
      tabItem(tabName = "home", uiOutput("homePageUI")),
      #tabItem(tabName = "admin", uiOutput("adminScreen")),
      #tabItem(tabName = "roster", uiOutput("rosterScreen")),
      #tabItem(tabName = "reports", uiOutput("reportScreen")),
      tabItem(tabName = "tests", uiOutput("testScreen"))
    )
  )
)

#------------------------------------------------------------#
#----- Server -----
#------------------------------------------------------------#

server <- function(input, output, session) {
  
  #------------------------------------------------------------#
  ##-----| Auth Reactive Values -----
  #------------------------------------------------------------#
  
  # Reactive value to store login status
  creds <- reactiveValues(
    loggedIn = FALSE,
    email = NULL,
    role = "user",
    uuid = NULL,
    name = NULL
  )
  
  #------------------------------------------------------------#
  ##-----| Org Reactive Data -----
  #------------------------------------------------------------#
  rosterDF <- reactiveVal()
  teamsDF <- reactiveVal()
  classList <- reactiveVal()
  posList <- reactiveVal()
  sportList <- reactiveVal()
  
  #------------------------------------------------------------#
  ##-----| Testing Reactive Data -----
  #------------------------------------------------------------#
  cmjData <- reactiveVal()
  sjData <- reactiveVal()
  mrData <- reactiveVal()
  laneAgilityData <- reactiveVal()
  fiveTenFiveData <- reactiveVal()
  threeQuarterData <- reactiveVal()
  anthroData <- reactiveVal()
  broadData <- reactiveVal()
  fortyData <- reactiveVal()
  vertData <- reactiveVal()
  
  # Initialize values at session start
  session$onSessionEnded(function() {
    # Logged In
    creds$loggedIn <- FALSE
    # User Email
    creds$email <- NULL
    # User Status
    creds$role <- "user"
    # User UUID
    creds$uuid <- NULL
    # User Name
    creds$name <- NULL
  })
  
  #------------------------------------------------------------#
  ##-----| Sidebar Logic -----
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
            menuItem("Home", tabName = "home", icon = icon("home"), selected = TRUE),
            #menuItem("Roster", tabName = "roster", icon = icon("users")),
            menuItem("Tests", tabName = "tests", icon = icon("stopwatch")),
            # Admin Tab
            #if (!is.null(creds$role) && creds$role == "Admin") {
            #  menuItem("Manage Users", tabName = "admin", icon = icon("gear"))
            #},
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
    } else {
      sidebarMenu(
        id = "tabs",
        tags$div(
          style = "height: 100%; display: flex; flex-direction: column;",
          # Sidebar Menu Items
          tags$div(
            style = "flex-grow: 1;",
            menuItem("Login", tabName = "home", selected = TRUE, icon = icon("right-to-bracket"))
          )
        )
      )
    }
  })
  
  # Set the initial active tab when the app starts
  observe({
    if (isTruthy(creds$loggedIn)) {
      updateTabItems(session, "tabs", "tests")  # Default to the "home" tab
    } else {
      updateTabItems(session, "tabs", "home")  # Default to the "login" tab for unauthenticated users
    }
  })
  
  #------------------------------------------------------------#
  ##-----| Sign In Logic -----
  #------------------------------------------------------------#
  
  # On Login Button Click
  observeEvent(input$login_btn, {
    email <- input$email
    password <- input$password
    
    login <- safe_sign_in(email = email, password = password)
    
    # Example login logic
    if (isTRUE(login$success)) {
      
      # Update App Credentials
      creds$email <- login$email
      creds$name <- login$name
      creds$role <- login$role
      creds$uuid <- login$localId
      creds$loggedIn <- TRUE
      
      # Access Google Auth
      googleAuth()
      
      # Get Organizational Data
      rosterDF(get_gsheet(sheet = "Roster"))
      teamsDF(get_gsheet(sheet = "Teams"))
      classList(get_gsheet(sheet = "classList"))
      posList(get_gsheet(sheet = "posList"))
      sportList(get_gsheet(sheet = "sportList"))
      
      # Redirect to home after successful login
      updateTabItems(session, "tabs", "tests") 
      
      # Access Hawkin
      #hawkinR::get_access(hdToken)
      
      # Sync Roster Data
      # Add in future
      
      # Update Force Plate Data
      #updateForcePlates()
      
    } else {
      creds$loggedIn <- FALSE
      shinyjs::html("login-error", "Invalid email or password.")
    }
  })
  
  #------------------------------------------------------------#
  ##-----| Reset Password -----
  #------------------------------------------------------------#
  
  ### >Forgot Password Link -----
  observeEvent(input$forgot_password, {
    inputSweetAlert(
      inputId = "reset_btn",
      title = "Reset Password",
      text = "Enter your email:",
      type = "question",
      input = "email",
      inputPlaceholder = input$email,
      btn_labels = "Send Reset Email",
      btn_colors = "light-blue",
      closeOnClickOutside = FALSE,
      showCloseButton = TRUE
    )
  })
  
  ### >Reset Password Button -----
  observeEvent(input$reset_btn, {
    tryCatch(
      {
        # Attempt to reset password
        reset <- reset_password(email = input$reset_email)
        # Show success alert
        show_alert(
          title = "Success!",
          text = paste0("A password reset email has been sent to ", email),
          type = "success",
          btn_labels = "Back to Login",
          btn_colors = "light-blue",
          closeOnClickOutside = TRUE,
          showCloseButton = FALSE
        )
      },
      error = function(e) {
        error = list(
          # Catch and handle errors triggered by stop in sign_in_password
          show_alert(
            title = "Error",
            text = paste(conditionMessage(e)),
            type = "error",
            btn_labels = "Back to Login",
            btn_colors = "light-blue",
            closeOnClickOutside = TRUE,
            showCloseButton = FALSE
          )
        )
      }
    )
  })
  
  #------------------------------------------------------------#
  ##-----| Sign Out Logic -----
  #------------------------------------------------------------#
  # Add observer for logout button
  observeEvent(input$logout_btn, {
    creds$loggedIn <- FALSE
    updateTabItems(session, "tabs", "login")
  })
  
  #------------------------------------------------------------#
  ##-----| Home Page Logic -----
  #------------------------------------------------------------#
  observeEvent(input$jumbo_btn, {
    updateTabItems(session, "tabs", "tests")
  })
  
  #------------------------------------------------------------#
  ##-----| Home Page UI -----
  #------------------------------------------------------------#
  
  output$homePageUI <- renderUI({
    if (!isTruthy(creds$loggedIn)) {
      #--------------------------------------------------#
      ###> Login Page -----
      #--------------------------------------------------#
      # Login page without sidebar
      fluidPage(
        div(
          id = "login-panel",
          style = "background-color: black; display: flex; flex-direction: column; align-items: center; height: 100vh; padding: 0;",
          div(
            style = "width: 75%; text-align: center;",
            tags$img(
              src = "btg-wallpaper-white-trans.png",
              alt = "Logo",
              style = "width: 50%; height: auto; display: block; margin: 0 auto;"
            )
          ),
          div(
            style = "width: 100%; display: flex; justify-content: center; margin-top: 0;",
            box(
              title = "Login",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              width = 4,
              textInput("email", "Email", placeholder = 'email'), # = "info@btgphysicaltherapy.com"),
              passwordInput("password", "Password", placeholder = 'password'),# value = "BTG.2025!"),
              actionButton("login_btn", "Login", class = "btn-primary"),
              div(id = "login-error", style = "color: red; margin-top: 10px;"),
              actionLink("forgot_password", "Forgot Password?", style = "margin-top: 20px; color: #6397d0;")
            )
          )
        )
      )
    } else {
      #--------------------------------------------------#
      ###> Home Page -----
      #--------------------------------------------------#
      fluidPage(
        jumbotron(
          title = "Bridge The Gap Performance PT Testing Dashboard",
          lead = "Welcome to the BTG Performance PT testing dashboard. Where we connect collection to reporting to action.",
          status = "primary",
          background = "black",
          width = 12,
          btnName = NULL
        )
      )
    }
  })
  
  #------------------------------------------------------------#
  ##-----| Test Page UI -----
  #------------------------------------------------------------#
  
  
  #------------------------------------------------------------#
  ###> Testing Logic -----
  #------------------------------------------------------------#
  # Reactive Test Screen UI Variable
  test_screen <- reactiveVal()
  
  ####| Return to test choices -----
  observeEvent(input$test_return, {
    test_screen("selectTest")
    closeSweetAlert()
  })
  
  ####| Back to Test Selection -----
  observeEvent(input$backToTestSelect, {
    test_screen("selectTest")
    closeSweetAlert()
  })
  
  ####| Back to Home Selection -----
  observeEvent(input$backToHome, {
    updateTabItems(session, "tabs", "home")
  })
  
  #----------------------------------------#
  ####| Anthropometrics Tests -----
  #----------------------------------------#
  
  ##### Select Anthropometrics Test -----
  observeEvent(input$selectTest_anthro, {
    test_screen("anthroTab")
  })
  
  ##### Submit Anthropometrics Test -----
  observeEvent(input$anthroSubmitBtn, {
    athInfo <- rosterDF() %>% filter(name == input$anthroSelect)
    tryCatch({
      # Attempt to add anthropometrics
      df <- data.frame(
        timestamp = as.numeric(dateTime()),
        date = as.character(as.Date(Sys.time())),
        name = as.character(input$anthroSelect),
        athleteId = as.character(athInfo$id),
        teams = as.character(athInfo$teams),
        groups = as.character(athInfo$groups),
        active = as.character(athInfo$active),
        email = as.character(athInfo$email),
        position = as.character(athInfo$position),
        class = as.character(athInfo$class),
        sport = as.character(athInfo$sport),
        height_ft = as.integer(input$anthroHeight_ft),
        height_in = as.numeric(input$anthroHeight_in),
        wingspan = as.integer(input$anthroWing),
        reach = as.integer(input$anthroReach)
      )
      
      ## Update Sheet Data
      update_gsheet(sheet = "Anthropometrics", data = df)
      
      # Show success alert
      show_alert(
        title = "Success!",
        text = tagList(
          tags$p(paste0("Anthropometrics for ", input$anthroSelect, " added successfully.")),
          actionButton("anthr_retest", "Test Again", class = "btn-primary"),
          actionButton("test_return", "Back to Tests", class = "btn-success")
        ),
        type = "success",
        closeOnClickOutside = TRUE,
        btn_labels = NA,
        showCloseButton = TRUE,
        showConfirmButton = FALSE, # Disable default confirm button
        showCancelButton = FALSE   # Disable default cancel button
      )
    }, error = function(e) {
      # Show error alert
      show_alert(
        title = "Error",
        text = tagList(
          tags$p(paste("Failed to add anthropometrics:", conditionMessage(e))),
          actionButton("anthr_retest", "Test Again", class = "btn-primary"),
        ),
        type = "error",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE
      )
    })
  })
  
  ##### Retest Anthropometrics Test -----
  observeEvent(input$anthr_retest, {
    updateSelectizeInput(session, "anthroSelect", selected = NULL)
    updateNumericInput(session, "anthroHeight", value = 72)
    updateNumericInput(session, "anthroWing", value = 72)
    updateNumericInput(session, "anthroReach", value = 90)
    closeSweetAlert()
    test_screen("anthroTab")
  })
  
  #----------------------------------------#
  ####| 3/4 Court Tests -----
  #----------------------------------------#
  
  ##### Select 3/4 Court Sprint Test -----
  observeEvent(input$selectTest_3Qcourt, {
    test_screen("3_4CourtTab")
  })
  
  ##### Submit 3/4 Court Sprint Test -----
  observeEvent(input$threeQSubmitBtn, {
    athInfo <- rosterDF() %>% filter(name == input$threeQSelect)
    
    tryCatch({
      # Attempt to add 3/4 court sprint
      df <- data.frame(
        timestamp = as.numeric(dateTime()),
        date = as.character(as.Date(Sys.time())),
        name = as.character(input$threeQSelect),
        athleteId = as.character(athInfo$id),
        teams = as.character(athInfo$teams),
        groups = as.character(athInfo$groups),
        active = as.character(athInfo$active),
        email = as.character(athInfo$email),
        position = as.character(athInfo$position),
        class = as.character(athInfo$class),
        sport = as.character(athInfo$sport),
        surface = as.character(input$threeQSurface),
        time = as.numeric(input$threeQTime)
      )
      
      ## Update Sheet Data
      update_gsheet(sheet = "3/4 Quarter Court", data = df)

      # Show success alert
      show_alert(
        title = "Success!",
        text = tagList(
          tags$p(paste0("3/4 Court Sprint for ", input$threeQSelect, " added successfully.")),
          actionButton("3_4_retest", "Test Again", class = "btn-primary"),
          actionButton("test_return", "Back to Tests", class = "btn-success")
        ),
        type = "success",
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE,
        btn_labels = NA,
        showConfirmButton = FALSE, # Disable default confirm button
        showCancelButton = FALSE   # Disable default cancel button
      )
    }, error = function(e) {
      print(e)
      # Show error alert
      show_alert(
        title = "Error",
        text = tagList(
          tags$p(paste("Failed to add 3/4 court sprint:", conditionMessage(e))),
          actionButton("3_4_retest", "Test Again", class = "btn-primary"),
        ),
        type = "error",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE
      )
    })
  })
  
  ##### Retest 3/4 Court Sprint Test -----
  observeEvent(input$threeQ_retest, {
    updateSelectizeInput(session, "threeQSelect", selected = NULL)
    updateNumericInput(session, "threeQTime", value = 0.00)
    closeSweetAlert()
    test_screen("3_4CourtTab")
  })
  
  #----------------------------------------#
  ####| Vertical Tests -----
  #----------------------------------------#
  ##### Select Vertical Test -----
  observeEvent(input$selectTest_vert, {
    test_screen("vertTab")
  })
  
  ##### Submit Vertical Test -----
  observeEvent(input$vertSubmitBtn, {
    athInfo <- rosterDF() %>% filter(name == input$vertSelect)
    
    tryCatch({
      # Attempt to add vertical jump
      df <- data.frame(
        timestamp = as.numeric(dateTime()),
        date = as.character(as.Date(Sys.time())),
        name = as.character(input$vertSelect),
        athleteId = as.character(athInfo$id),
        teams = as.character(athInfo$teams),
        groups = as.character(athInfo$groups),
        active = as.character(athInfo$active),
        email = as.character(athInfo$email),
        position = as.character(athInfo$position),
        class = as.character(athInfo$class),
        sport = as.character(athInfo$sport),
        test = as.character(input$vertType),
        height_ft_in = as.character(paste0(input$vertHeightFeet,"'", input$vertHeightInch, '"')),
        height_in = as.integer((input$vertHeightFeet * 12) + input$vertHeightInch)
      )
      
      ## Update Sheet Data
      update_gsheet(sheet = "Vertical Jump", data = df)
      
      # Show success alert
      show_alert(
        title = "Success!",
        text = tagList(
          tags$p(paste0("Vertical Jump for ", input$vertSelect, " added successfully.")),
          actionButton("vert_retest", "Test Again", class = "btn-primary"),
          actionButton("test_return", "Back to Tests", class = "btn-success")
        ),
        type = "success",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE,
        showConfirmButton = FALSE, # Disable default confirm button
        showCancelButton = FALSE   # Disable default cancel button
      )
    }, error = function(e) {
      # Show error alert
      show_alert(
        title = "Error",
        text = tagList(
          tags$p(paste("Failed to add vertical jump:", conditionMessage(e))),
          actionButton("vert_retest", "Test Again", class = "btn-primary"),
        ),
        type = "error",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE
      )
    })
  })
  
  ##### Retest Vertical Test -----
  observeEvent(input$vert_retest, {
    updateSelectizeInput(session, "vertSelect", selected = NULL)
    updateNumericInput(session, "vertHeightFeet", value = 0)
    updateNumericInput(session, "vertHeightInch", value = 0)
    closeSweetAlert()
    test_screen("vertTab")
  })
  
  #----------------------------------------#
  ####| Broad Jump Tests -----
  #----------------------------------------#
  
  ##### Select Broad Jump Test -----
  observeEvent(input$selectTest_broad, {
    test_screen("broadTab")
  })
  
  ##### Submit Broad Jump Test -----
  observeEvent(input$broadSubmitBtn, {
    athInfo <- rosterDF() %>% filter(name == input$broadSelect)
    
    tryCatch({
      # Attempt to add broad jump
      df <- data.frame(
        timestamp = as.numeric(dateTime()),
        date = as.character(as.Date(Sys.time())),
        name = as.character(input$broadSelect),
        athleteId = as.character(athInfo$id),
        teams = as.character(athInfo$teams),
        groups = as.character(athInfo$groups),
        active = as.character(athInfo$active),
        email = as.character(athInfo$email),
        position = as.character(athInfo$position),
        class = as.character(athInfo$class),
        sport = as.character(athInfo$sport),
        distance_ft_in = as.character(paste0(input$broadDistFeet,"'", input$broadDistInch, '\"')),
        distance_in = as.integer((input$broadDistFeet * 12) + input$broadDistInch)
      )
      
      ## Update Sheet Data
      update_gsheet(sheet = "Broad Jump", data = df)
      
      # Show success alert
      show_alert(
        title = "Success!",
        text = tagList(
          tags$p(paste0("Broad Jump for ", input$broadSelect, " added successfully.")),
          actionButton("broad_retest", "Test Again", class = "btn-primary"),
          actionButton("test_return", "Back to Tests", class = "btn-success")
        ),
        type = "success",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE,
        showConfirmButton = FALSE, # Disable default confirm button
        showCancelButton = FALSE   # Disable default cancel button
      )
    }, error = function(e) {
      # Show error alert
      show_alert(
        title = "Error",
        text = tagList(
          tags$p(paste("Failed to add broad jump:", conditionMessage(e))),
          actionButton("broad_retest", "Test Again", class = "btn-primary"),
        ),
        type = "error",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE
      )
    })
  })
  
  ##### Retest Broad Jump Test -----
  observeEvent(input$broad_retest, {
    updateSelectizeInput(session, "broadSelect", selected = NULL)
    updateNumericInput(session, "broadDistFeet", value = 0)
    updateNumericInput(session, "broadDistInch", value = 0)
    closeSweetAlert()
    test_screen("broadTab")
  })
  
  #----------------------------------------#
  ####| 40 Yard Tests -----
  #----------------------------------------#
  
  ##### Select 40 Yard Test -----
  observeEvent(input$selectTest_40sprint, {
    test_screen("40sprintTab")
  })
  
  ##### Submit 40 Yard Test -----
  observeEvent(input$fortySubmitBtn, {
    athInfo <- rosterDF() %>% filter(name == input$fortySelect)
    
    tryCatch({
      # Attempt to add 40 yard sprint
      df <- data.frame(
        timestamp = as.numeric(dateTime()),
        date = as.character(as.Date(Sys.time())),
        name = as.character(input$fortySelect),
        athleteId = as.character(athInfo$id),
        teams = as.character(athInfo$teams),
        groups = as.character(athInfo$groups),
        active = as.character(athInfo$active),
        email = as.character(athInfo$email),
        position = as.character(athInfo$position),
        class = as.character(athInfo$class),
        sport = as.character(athInfo$sport),
        surface = as.character(input$fortySurface),
        time_10y = as.numeric(input$fortySplit10),
        time_20y = as.numeric(input$fortySplit20),
        time_40y = as.numeric(input$fortyTime)
      )
      
      ## Update Sheet Data
      update_gsheet(sheet = "40 Yard Dash", data = df)

      # Show success alert
      show_alert(
        title = "Success!",
        text = tagList(
          tags$p(paste0("40 Yard Sprint for ", input$fortySelect, " added successfully.")),
          actionButton("forty_retest", "Test Again", class = "btn-primary"),
          actionButton("test_return", "Back to Tests", class = "btn-success")
        ),
        type = "success",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE,
        showConfirmButton = FALSE, # Disable default confirm button
        showCancelButton = FALSE   # Disable default cancel button
      )
    }, error = function(e) {
      # Show error alert
      show_alert(
        title = "Error",
        text = tagList(
          tags$p(paste("Failed to add 40 yard sprint:", conditionMessage(e))),
          actionButton("forty_retest", "Test Again", class = "btn-primary"),
        ),
        type = "error",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE
      )
    })
  })
  
  ##### Retest 40 Yard Test -----
  observeEvent(input$forty_retest, {
    updateSelectizeInput(session, "fortySelect", selected = NULL)
    updateNumericInput(session, "fortySplit10", value = 0.00)
    updateNumericInput(session, "fortySplit20", value = 0.00)
    updateNumericInput(session, "fortyTime", value = 0.00)
    closeSweetAlert()
    test_screen("40sprintTab")
  })
  
  #----------------------------------------#
  ####| 5-10-5 Agility Tests -----
  #----------------------------------------#
  
  ##### Select 5-10-5 Agility Test -----
  observeEvent(input$selectTest_5105, {
    test_screen("5105agilityTab")
  })
  
  ##### Submit 5-10-5 Agility Test -----
  observeEvent(input$ftfSubmitBtn, {
    athInfo <- rosterDF() %>% filter(name == input$ftfSelect)
    
    tryCatch({
      # Attempt to add 5-10-5 agility
      df <- data.frame(
        timestamp = as.numeric(dateTime()),
        date = as.character(as.Date(Sys.time())),
        name = as.character(input$ftfSelect),
        athleteId = as.character(athInfo$id),
        teams = as.character(athInfo$teams),
        groups = as.character(athInfo$groups),
        active = as.character(athInfo$active),
        email = as.character(athInfo$email),
        position = as.character(athInfo$position),
        class = as.character(athInfo$class),
        sport = as.character(athInfo$sport),
        surface = as.character(input$ftfSurface),
        direction = as.character(input$ftfDirection),
        time = as.numeric(input$ftfTime)
      )
      
      ## Update Sheet Data
      update_gsheet(sheet = "5-10-5 Pro Agility", data = df)

      # Show success alert
      show_alert(
        title = "Success!",
        text = tagList(
          tags$p(paste0("5-10-5 Agility for ", input$ftfSelect, " added successfully.")),
          actionButton("agility_retest", "Test Again", class = "btn-primary"),
          actionButton("test_return", "Back to Tests", class = "btn-success")
        ),
        type = "success",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE,
        showConfirmButton = FALSE, # Disable default confirm button
        showCancelButton = FALSE   # Disable default cancel button
      )
    }, error = function(e) {
      # Show error alert
      show_alert(
        title = "Error",
        text = tagList(
          tags$p(paste("Failed to add 5-10-5 agility:", conditionMessage(e))),
          actionButton("agility_retest", "Test Again", class = "btn-primary"),
        ),
        type = "error",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE
      )
    })
  })
  
  ##### Retest 5-10-5 Agility Test -----
  observeEvent(input$agility_retest, {
    updateSelectizeInput(session, "ftfSelect", selected = NULL)
    updateNumericInput(session, "ftfTime", value = 0.00)
    closeSweetAlert()
    test_screen("5105agilityTab")
  })
  
  #----------------------------------------#
  #### Pro Agility Tests -----
  #----------------------------------------#
  
  ##### Select Pro Lane Agility Test -----
  observeEvent(input$selectTest_laneAgility, {
    test_screen("laneAgilityTab")
  })
  
  ##### Submit Pro Lane Agility Test -----
  observeEvent(input$laneAgilSubmitBtn, {
    athInfo <- rosterDF() %>% filter(name == input$laneAgilSelect)
    
    tryCatch({
      # Attempt to add pro lane agility
      df <- data.frame(
        timestamp = as.numeric(dateTime()),
        date = as.character(as.Date(Sys.time())),
        name = as.character(input$laneAgilSelect),
        athleteId = as.character(athInfo$id),
        teams = as.character(athInfo$teams),
        groups = as.character(athInfo$groups),
        active = as.character(athInfo$active),
        email = as.character(athInfo$email),
        position = as.character(athInfo$position),
        class = as.character(athInfo$class),
        sport = as.character(athInfo$sport),
        surface = as.character(input$laneAgilSurface),
        time = as.numeric(input$laneAgilTime)
      )
      
      ## Update Sheet Data
      update_gsheet(sheet = "Lane Agility", data = df)
      
      # Show success alert
      show_alert(
        title = "Success!",
        text = tagList(
          tags$p(paste0("Pro Lane Agility for ", input$laneAgilSelect, " added successfully.")),
          actionButton("lane_retest", "Test Again", class = "btn-primary"),
          actionButton("test_return", "Back to Tests", class = "btn-success")
        ),
        type = "success",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE,
        showConfirmButton = FALSE, # Disable default confirm button
        showCancelButton = FALSE   # Disable default cancel button
      )
    }, error = function(e) {
      # Show error alert
      show_alert(
        title = "Error",
        text = tagList(
          tags$p(paste("Failed to add pro lane agility:", conditionMessage(e))),
          actionButton("lane_retest", "Test Again", class = "btn-primary"),
        ),
        type = "error",
        btn_labels = NA,
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE
      )
    })
  })
  
  ##### Retest Pro Lane Agility Test -----
  observeEvent(input$lane_retest, {
    updateSelectizeInput(session, "laneAgilSelect", selected = NULL)
    updateNumericInput(session, "laneAgilTime", value = 0.00)
    closeSweetAlert()
    test_screen("laneAgilityTab")
  })
  
  #------------------------------------------------------------#
  ###----- Testing Page Output -----
  #------------------------------------------------------------#
  output$testScreen <- renderUI({
    screen <- test_screen()
    
    if(is.null(screen) || screen == "selectTest"){
      #--------------------------------------------------#
      ####----- Test Selections Page UI -----
      #--------------------------------------------------#
      fluidPage(
        title = "Test Selection Page",
        actionLink("backToHome", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
        fluidRow(
          column(width = 3),
          column(
            width = 4,
            h1("What are we testing?")
          ),
          column(width = 3)
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 4,
            # Buttons with consistent spacing
            div(
              actionBttn(
                inputId = "selectTest_anthro",
                label = "Anthropometrics",
                icon = icon("ruler-combined"),
                style = "unite",
                color = "primary",
                block = TRUE,
                size = "lg"
              ),
              style = "margin-bottom: 15px; width: 100%; text-align: center;"
            ),
            div(
              actionBttn(
                inputId = "selectTest_vert",
                label = "Vertical Jump",
                icon = icon("person-arrow-up-from-line"),
                style = "unite",
                color = "primary",
                block = TRUE,
                size = "lg"
              ),
              style = "margin-bottom: 15px; width: 100%; text-align: center;"
            ),
            div(
              actionBttn(
                inputId = "selectTest_broad",
                label = "Broad Jump",
                icon = icon("person-walking-arrow-right"),
                style = "unite",
                color = "primary",
                block = TRUE,
                size = "lg"
              ),
              style = "margin-bottom: 15px; width: 100%; text-align: center;"
            ),
            div(
              actionBttn(
                inputId = "selectTest_3Qcourt",
                label = "3/4 Court Sprint",
                icon = icon("person-walking-dashed-line-arrow-right"),
                style = "unite",
                color = "primary",
                block = TRUE,
                size = "lg"
              ),
              style = "margin-bottom: 15px; width: 100%; text-align: center;"
            ),
            div(
              actionBttn(
                inputId = "selectTest_40sprint",
                label = "40 Yard Sprint",
                icon = icon("person-walking-dashed-line-arrow-right"),
                style = "unite",
                color = "primary",
                block = TRUE,
                size = "lg"
              ),
              style = "margin-bottom: 15px; width: 100%; text-align: center;"
            ),
            div(
              actionBttn(
                inputId = "selectTest_5105",
                label = "5-10-5 Pro Agility",
                icon = icon("person-walking-arrow-loop-left"),
                style = "unite",
                color = "primary",
                block = TRUE,
                size = "lg"
              ),
              style = "margin-bottom: 15px; width: 100%; text-align: center;"
            ),
            div(
              actionBttn(
                inputId = "selectTest_laneAgility",
                label = "Pro Lane Agility",
                icon = icon("person-walking-arrow-loop-left"),
                style = "unite",
                color = "primary",
                block = TRUE,
                size = "lg"
              ),
              style = "margin-bottom: 15px; width: 100%; text-align: center;"
            )
          ),
          column(width = 3)
        )
      )
    } else if (screen == "anthroTab") {
      #--------------------------------------------------#
      ####----- Anthropometrics Page UI -----
      #--------------------------------------------------#
      fluidPage(
        fluidRow(
          actionLink("backToTestSelect", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("Anthropometrics"),
            selectizeInput(
              inputId = "anthroSelect",
              label = "Select Athlete:",
              choices = c(rosterDF()$name),
              size = 5,
            ),
            fluidRow(
              column(
                width = 6,
                numericInput(
                  inputId = "anthroHeight_ft",
                  label = "Height (ft)",
                  min = 1,
                  max = 8,
                  step = 1,
                  value = 5
                )
              ),
              column(
                width = 6,
                numericInput(
                  inputId = "anthroHeight_in",
                  label = "Height (in)",
                  min = 0.0,
                  max = 12.0,
                  step = 0.25,
                  value = 0.0
                )
              )
            ),
            numericInput(
              inputId = "anthroWing",
              label = "Wingspan (in)",
              min = 0,
              max = 100,
              step = 0.25,
              value = 72
            ),
            numericInput(
              inputId = "anthroReach",
              label = "standing Reach (in)",
              min = 0,
              max = 120,
              step = 0.25,
              value = 90
            ),
            actionBttn(
              inputId = "anthroSubmitBtn",
              label = "Submit",
              style = "pill",
              color = "primary",
              size = "lg"
            )
          ),
          column(width = 3)
        )
      )
    } else if(screen == "3_4CourtTab") {
      #--------------------------------------------------#
      ####----- 3/4 Court Sprint Page UI -----
      #--------------------------------------------------#
      fluidPage(
        fluidRow(
          actionLink("backToTestSelect", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("3/4 Court Sprint"),
            selectizeInput(
              inputId = "threeQSelect",
              label = "Select Athlete:",
              choices = c(rosterDF()$name),
              size = 5,
            ),
            radioGroupButtons(
              inputId = "threeQSurface",
              label = "Surface",
              choices = c("Court", "Turf"),
              individual = TRUE,
              size = "lg",
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-circle-o", 
                            style = "color: steelblue"))
            ),
            numericInput(
              inputId = "threeQTime",
              label = "Time (s)",
              value = 0.00,
              min = 0.00,
              max = 10.00,
              step = 0.10
            ),
            actionBttn(
              inputId = "threeQSubmitBtn",
              label = "Submit",
              style = "pill",
              color = "primary",
              size = "lg"
            )
          ),
          column(width = 3)
        )
      )
    } else if(screen == "vertTab") {
      #--------------------------------------------------#
      ####----- Vert Page UI -----
      #--------------------------------------------------#
      fluidPage(
        fluidRow(
          actionLink("backToTestSelect", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("Standing or Approach vertical Jump"),
            selectizeInput(
              inputId = "vertSelect",
              label = "Select Athlete:",
              choices = c(rosterDF()$name),
              size = 5,
            ),
            radioGroupButtons(
              inputId = "vertType",
              label = "Vertical Jump Test",
              choices = c("Standing", "Approach"),
              individual = TRUE,
              size = "lg",
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-circle-o", 
                            style = "color: steelblue"))
            ),
            numericInput(
              inputId = "vertHeightFeet",
              label = "Height (ft)",
              value = 0,
              min = 0,
              max = 15,
              step = 1
            ),
            numericInput(
              inputId = "vertHeightInch",
              label = "Distance (in)",
              value = 0.0,
              min = 0.0,
              max = 12.00,
              step = 0.25
            ),
            actionBttn(
              inputId = "vertSubmitBtn",
              label = "Submit",
              style = "pill",
              color = "primary",
              size = "lg"
            )
          ),
          column(width = 3)
        )
      )
    } else if(screen == "broadTab") {
      #--------------------------------------------------#
      ####-----Broad Jump Page UI -----
      #--------------------------------------------------#
      fluidPage(
        fluidRow(
          actionLink("backToTestSelect", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("Broad Jump"),
            selectizeInput(
              inputId = "broadSelect",
              label = "Select Athlete:",
              choices = c(rosterDF()$name),
              size = 5,
            ),
            numericInput(
              inputId = "broadDistFeet",
              label = "Distance (ft)",
              value = 0,
              min = 0,
              max = 15,
              step = 1
            ),
            numericInput(
              inputId = "broadDistInch",
              label = "Distance (in)",
              value = 0.0,
              min = 0.0,
              max = 12.00,
              step = 0.25
            ),
            actionBttn(
              inputId = "broadSubmitBtn",
              label = "Submit",
              style = "pill",
              color = "primary",
              size = "lg"
            )
          ),
          column(width = 3)
        )
      )
    } else if(screen == "40sprintTab") {
      #--------------------------------------------------#
      ####----- 40 Yard Page UI -----
      #--------------------------------------------------#
      fluidPage(
        fluidRow(
          actionLink("backToTestSelect", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("40 Yard Sprint"),
            selectizeInput(
              inputId = "fortySelect",
              label = "Select Athlete:",
              choices = c(rosterDF()$name),
              size = 5,
            ),
            radioGroupButtons(
              inputId = "fortySurface",
              label = "Surface",
              choices = c("Court", "Turf"),
              selected = "Turf",
              individual = TRUE,
              size = "lg",
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-circle-o", 
                            style = "color: steelblue"))
            ),
            numericInput(
              inputId = "fortySplit10",
              label = "10 Yard Time (s)",
              value = 0.00,
              min = 0.00,
              max = 10.00,
              step = 0.10
            ),
            numericInput(
              inputId = "fortySplit20",
              label = "20 Yard Time (s)",
              value = 0.00,
              min = 0.00,
              max = 10.00,
              step = 0.10
            ),
            numericInput(
              inputId = "fortyTime",
              label = "40 Yard Time (s)",
              value = 0.00,
              min = 0.00,
              max = 10.00,
              step = 0.10
            ),
            actionBttn(
              inputId = "fortySubmitBtn",
              label = "Submit",
              style = "pill",
              color = "primary",
              size = "lg"
            )
          ),
          column(width = 3)
        )
      )
    } else if(screen == "5105agilityTab") {
      #--------------------------------------------------#
      ####----- 5-10-5 Page UI -----
      #--------------------------------------------------#
      fluidPage(
        fluidRow(
          actionLink("backToTestSelect", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("5-10-5 Agility"),
            selectizeInput(
              inputId = "ftfSelect",
              label = "Select Athlete:",
              choices = c(rosterDF()$name),
              size = 5,
            ),
            radioGroupButtons(
              inputId = "ftfSurface",
              label = "Surface",
              choices = c("Court", "Turf"),
              selected = "Turf",
              individual = TRUE,
              size = "lg",
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-circle-o", 
                            style = "color: steelblue"))
            ),
            radioGroupButtons(
              inputId = "ftfDirection",
              label = "Starting Direction",
              choices = c("Left", "Right"),
              individual = TRUE,
              size = "lg",
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-circle-o", 
                            style = "color: steelblue"))
            ),
            numericInput(
              inputId = "ftfTime",
              label = "Time (s)",
              value = 0.00,
              min = 0.00,
              max = 10.00,
              step = 0.10
            ),
            actionBttn(
              inputId = "ftfSubmitBtn",
              label = "Submit",
              style = "pill",
              color = "primary",
              size = "lg"
            )
          ),
          column(width = 3)
        )
      )
    } else if(screen == "laneAgilityTab") {
      #--------------------------------------------------#
      ####----- Lane Agility Page UI -----
      #--------------------------------------------------#
      fluidPage(
        fluidRow(
          actionLink("backToTestSelect", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
        ),
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("Lane Agility Tests"),
            selectizeInput(
              inputId = "laneAgilSelect",
              label = "Select Athlete:",
              choices = c(rosterDF()$name),
              size = 5,
            ),
            radioGroupButtons(
              inputId = "laneAgilSurface",
              label = "Surface",
              choices = c("Court", "Turf"),
              selected = "Turf",
              individual = TRUE,
              size = "lg",
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-circle-o", 
                            style = "color: steelblue"))
            ),
            numericInput(
              inputId = "laneAgilTime",
              label = "Time (s)",
              value = 0.00,
              min = 0.00,
              max = 10.00,
              step = 0.10
            ),
            actionBttn(
              inputId = "laneAgilSubmitBtn",
              label = "Submit",
              style = "pill",
              color = "primary",
              size = "lg"
            )
          ),
          column(width = 3)
        )
      )
    }
  })
  
}

shinyApp(ui, server, enableBookmarking = "server")



