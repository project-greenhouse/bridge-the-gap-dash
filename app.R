# Reorder your source calls:
source("global.R")  # Move this first

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
##----- Sidebar -----
#------------------------------------------------------------#
sidebar <- dashboardSidebar(
  width = 250,
  uiOutput("sidebarUI")
)

#------------------------------------------------------------#
#----- Body -----
#------------------------------------------------------------#
ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  header = header,
  sidebar = sidebar,
  
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
      tabItem(tabName = "admin", uiOutput("adminScreen")),
      tabItem(tabName = "roster", uiOutput("rosterScreen")),
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
    
    logged <- list(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Roster", tabName = "roster", icon = icon("users")),
      menuItem("Tests", tabName = "tests", icon = icon("stopwatch")),
      if (!is.null(creds$role) && creds$role == "admin") {
        menuItem("Manage Users", tabName = "admin", icon = icon("gear"))
      }
    )
    
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
            if(!is.null(creds$loggedIn) && isTruthy(creds$loggedIn)) {
              logged
            } else{
              menuItem("Login", tabName = "home", icon = icon("right-to-bracket"))
            }
          ),
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
      
      # Redirect to home after successful login
      updateTabItems(session, "tabs", "home")  
    } else {
      creds$loggedIn <- FALSE
      shinyjs::html("login-error", "Invalid email or password.")
    }
  })
  
  #------------------------------------------------------------#
  ##-----| Reset Password -----
  #------------------------------------------------------------#
  
  ### Forgot Password Link -----
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
  
  ### Reset Password Button -----
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
      ###----- Login Page UI -----
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
              textInput("email", "Email", value = "info@btgphysicaltherapy.com"),
              passwordInput("password", "Password", value = "BTG.2025!"),
              actionButton("login_btn", "Login", class = "btn-primary"),
              div(id = "login-error", style = "color: red; margin-top: 10px;"),
              actionLink("forgot_password", "Forgot Password?", style = "margin-top: 20px; color: #6397d0;")
            )
          )
        )
      )
    } else {
      #--------------------------------------------------#
      ###----- Home Page UI -----
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
  ##-----| Admin Page UI -----
  #------------------------------------------------------------#
  output$adminScreen <- renderUI({
    if (creds$role == "admin") {
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          h1("Admin")
        ),
        column(width = 3)
      )
    }
  })
  
  #------------------------------------------------------------#
  ##-----| Roster Page UI -----
  #------------------------------------------------------------#
  output$rosterScreen <- renderUI({
    fluidRow(
      column(width = 3),
      column(
        width = 6,
        h1("Roster")
      ),
      column(width = 3)
    )
  })
  
  #------------------------------------------------------------#
  ##-----| Test Page UI -----
  #------------------------------------------------------------#
  
  # Reactive Test Screen UI Variable
  test_screen <- reactiveVal("selectTest")
  
  #------------------------------------------------------------#
  ###----- Testing Logic -----
  #------------------------------------------------------------#
  
  #### Select Anthro Test -----
  observeEvent(input$selectTest_anthro, {
    #updateTabItems(session, "tabs", "anthroTab")
    test_screen("anthroTab")
  })
  
  #### Select 3/4 Court Sprint Test -----
  observeEvent(input$selectTest_3Qcourt, {
    #updateTabItems(session, "tabs", "3_4CourtTab")
    test_screen("3_4CourtTab")
  })
  
  #### Select Vert Test -----
  observeEvent(input$selectTest_vert, {
    #updateTabItems(session, "tabs", "vertTab")
    test_screen("vertTab")
  })
  
  #### Select Broad Jump Test -----
  observeEvent(input$selectTest_broad, {
    #updateTabItems(session, "tabs", "broadTab")
    test_screen("broadTab")
  })
  
  #### Select 40 Yard Test -----
  observeEvent(input$selectTest_40sprint, {
    #updateTabItems(session, "tabs", "40sprintTab")
    test_screen("40sprintTab")
  })
  
  #### Select 5-10-5 Agility Test -----
  observeEvent(input$selectTest_5105, {
    #updateTabItems(session, "tabs", "5105agilityTab")
    test_screen("5105agilityTab")
  })
  
  #### Select Pro Lane Agility Test -----
  observeEvent(input$selectTest_laneAgility, {
    #updateTabItems(session, "tabs", "laneAgilityTab")
    test_screen("laneAgilityTab")
  })
  
  #------------------------------------------------------------#
  ###----- Testing Page Outout -----
  #------------------------------------------------------------#
  output$testScreen <- renderUI({
    screen <- test_screen()
    if (screen == "anthroTab") {
      #--------------------------------------------------#
      ####----- Anthro Page UI -----
      #--------------------------------------------------#
      fluidPage(
        fluidRow(
          column(width = 3),
          column(
            width = 6,
            h1("Anthropometrics"),
            selectInput(
              inputId = "anthroSelect",
              label = "Select Athlete:",
              choices = c("Athlete 1", "Athlete 2", "Athlete 3"),
              selected = "Athlete 1"
            ),
            textInput(
              inputId = "anthroHeight",
              label = "Height (in)",
              placeholder = "72"
            ),
            textInput(
              inputId = "anthroWing",
              label = "Wingspan (in)",
              placeholder = "72"
            ),
            textInput(
              inputId = "anthroReach",
              label = "standing Reach (in)",
              placeholder = "90"
            ),
            actionBttn(
              inputId = "anthroSubmitBtn",
              label = "Submit",
              style = "unite",
              color = "primary",
              size = "md"
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
          column(width = 3),
          column(
            width = 6,
            h1("3/4 Court Sprint")
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
          column(width = 3),
          column(
            width = 6,
            h1("Standing or Approach vertical Jump")
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
          column(width = 3),
          column(
            width = 6,
            h1("Broad Jump")
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
          column(width = 3),
          column(
            width = 6,
            h1("40 Yard Sprint")
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
          column(width = 3),
          column(
            width = 6,
            h1("5-10-5 Agility")
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
          column(width = 3),
          column(
            width = 6,
            h1("Lane Agility Tests")
          ),
          column(width = 3)
        )
      )
    } else if(!is.null(screen) || screen == "selectTest" ) {
      #--------------------------------------------------#
      ####----- Test Selections Page UI -----
      #--------------------------------------------------#
      fluidPage(
        title = "Test Selection Page",
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
    }
  })
  
}

shinyApp(ui, server)

