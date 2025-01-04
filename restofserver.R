



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
    if (isTruthy(creds$loggedIn)) {
      sidebarMenu(
        id = "tabs",  # Use sidebarMenu ID for tab navigation
        tags$div(
          style = "height: 100%; display: flex; flex-direction: column;",
          # Sidebar Menu Items
          tags$div(
            style = "flex-grow: 1;",
            menuItem("Home", tabName = "home", icon = icon("home"), selected = TRUE),  # Default tab for logged-in users
            menuItem("Roster", tabName = "roster", icon = icon("users")),
            menuItem("Tests", tabName = "tests", icon = icon("stopwatch")),
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
      
      # Get Testing Data
      get_btg_data()
      
      # Redirect to home after successful login
      updateTabItems(session, "tabs", "home") 
      
      # Access Hawkin
      hawkinR::get_access(hdToken)
      
      # Sync Roster Data
      # Add in future
      
      # Update Force Plate Data
      updateForcePlates()
      
    } else {
      creds$loggedIn <- FALSE
      shinyjs::html("login-error", "Invalid email or password.")
    }
  })
  
  
  
  
  
  #------------------------------------------------------------#
  ##-----| Roster Page -----
  #------------------------------------------------------------#
  
  #------------------------------------------------------------#
  ###----- Roster Page Logic -----
  #------------------------------------------------------------#
  
  #### Reactive Roster Data -----
  rosterReact <- reactive({
    # Get the current roster and teams data
    team_df <- get("teams")
    df <- get("roster")
    
    # Filter by selected teams
    if (!is.null(input$rost_teamSelect) && length(input$rost_teamSelect) > 0) {
      # Get the UUIDs corresponding to the selected team names
      selected_team_uuids <- team_df$id[team_df$proper_name %in% input$rost_teamSelect]
      
      # Filter roster for athletes in the selected teams
      df <- df[sapply(df$teams, function(team) {
        any(selected_team_uuids %in% unlist(strsplit(team, ",")))
      }), ]
    }
    
    # Return the filtered data
    df
  })
  
  #### Update Roster Table -----
  output$rosterTable <- DT::renderDataTable({
    DT::datatable(
      rosterReact(),
      extensions = 'Responsive',
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50),
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })
  
  #------------------------------------------------------------#
  ###----- Roster Page UI -----
  #------------------------------------------------------------#
  
  output$rosterScreen <- renderUI({
    fluidPage(
      title = "Roster Page",
      fluidRow(
        column(
          width = 5,
          virtualSelectInput(
            inputId = "rost_teamSelect",
            label = "Select Team:",
            choices = c(teams$proper_name),
            multiple = TRUE,
            search = TRUE,
            showValueAsTags = TRUE, 
            noOfDisplayValues = 5,
            disableOptionGroupCheckbox = TRUE,
            width = "100%",
            dropboxWrapper = "body"
          )
        ),
        column(width = 7)
      ),
      fluidRow(
        DT::dataTableOutput("rosterTable")
      )
    )
    
  })
  
  #------------------------------------------------------------#
  ##-----| Reports Page -----
  #------------------------------------------------------------#
  
  #------------------------------------------------------------#
  ###----- Reports Page Logic -----
  #------------------------------------------------------------#
  
  #### Test Variables -----
  rep_cmj <- reactiveVal()
  rep_sj <- reactiveVal()
  rep_mr <- reactiveVal()
  rep_laneAgility <- reactiveVal()
  rep_ftf <- reactiveVal()
  rep_threeQ <- reactiveVal()
  rep_anthro <- reactiveVal()
  rep_broad <- reactiveVal()
  rep_forty <- reactiveVal()
  rep_vert <- reactiveVal()
  
  #### Reactive Roster Data -----
  reportAth <- reactive({
    roster_df <- roster
    team_df <- teams
    
    if(!is.null(input$report_teamSelect)) {
      athList <- roster_df %>% filter(teams %in% input$report_teamSelect) %>% pull(name)
    } else {
      athList <- roster_df$name
    }
    
    athList
  })
  
  #### Reactive Dates Data -----
  reportDates <- reactive({
    ath <- input$report_athlete
    
    # Get Test Dates for Selected Athlete
    dates <- filter_dates(ath)
    
    # Return the dates
    dates
  })
  
  #### Reactive Test Data -----
  observeEvent(input$report_load, {
    ath <- input$report_athlete
    date <-  input$report_date
    
    # CMJ Data
    rep_cmj(filterCMJ(ath, date))
    # SJ Data
    rep_sj(filterSJ(ath, date))
    # MR Data
    rep_mr(filterMR(ath, date))
    # Lane Agility Data
    rep_laneAgility(filterLaneAgility(ath, date))
    # 5-10-5 Data
    rep_ftf(filterFTF(ath, date))
    # 3/4 Court Data
    rep_threeQ(filterThreeQ(ath, date))
    # Anthropometrics Data
    rep_anthro(filterAnthro(ath, date))
    # Broad Jump Data
    rep_broad(filterBroad(ath, date))
    # 40 Yard Data
    rep_forty(filterForty(ath, date))
    # Vertical Jump Data
    rep_vert(filterVert(ath, date))
  })
  
  #### CMJ Table -----
  output$cmjTable <- DT::renderDataTable({
    DT::datatable(
      rep_cmj(),
      extensions = 'Responsive',
      options = list(
        pageLength = 2,
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 't'
      )
    )
  })
  
  #### SJ Table -----
  output$sjTable <- DT::renderDataTable({
    DT::datatable(
      rep_sj(),
      extensions = 'Responsive',
      options = list(
        pageLength = 2,
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 't'
      )
    )
  })
  
  #### MR Table -----
  output$mrTable <- DT::renderDataTable({
    DT::datatable(
      rep_mr(),
      extensions = 'Responsive',
      options = list(
        pageLength = 2,
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 't'
      )
    )
  })
  
  #### Lane Agility Table -----
  output$laneAgilityTable <- DT::renderDataTable({
    DT::datatable(
      rep_laneAgility(),
      extensions = 'Responsive',
      options = list(
        pageLength = 2,
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 't'
      )
    )
  })
  
  #### 5-10-5 Table -----
  output$ftfTable <- DT::renderDataTable({
    DT::datatable(
      rep_ftf(),
      extensions = 'Responsive',
      options = list(
        pageLength = 2,
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 't'
      )
    )
  })
  
  #### 3/4 Court Table -----
  output$threeQTable <- DT::renderDataTable({
    DT::datatable(
      rep_threeQ(),
      extensions = 'Responsive',
      options = list(
        pageLength = 2,
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 't'
      )
    )
  })
  
  #### Anthropometrics Table -----
  output$anthroTable <- DT::renderDataTable({
    DT::datatable(
      rep_anthro(),
      extensions = 'Responsive',
      options = list(
        pageLength = 2,
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 't'
      )
    )
  })
  
  #### Broad Jump Table -----
  output$broadTable <- DT::renderDataTable({
    DT::datatable(
      rep_broad(),
      extensions = 'Responsive',
      options = list(
        pageLength = 2,
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 't'
      )
    )
  })
  
  #### 40 Yard Table -----
  output$fortyTable <- DT::renderDataTable({
    DT::datatable(
      rep_forty(),
      extensions = 'Responsive',
      options = list(
        pageLength = 2,
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 't'
      )
    )
  })
  
  #### Vertical Jump Table -----
  output$vertTable <- DT::renderDataTable({
    DT::datatable(
      rep_vert(),
      extensions = 'Responsive',
      options = list(
        pageLength = 2,
        searching = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        rownames = FALSE,
        dom = 't'
      )
    )
  })
  
  #------------------------------------------------------------#
  ###----- Reports Page UI -----
  #------------------------------------------------------------#
  
  output$reportScreen <- renderUI({
    fluidPage(
      title = "Admin Page",
      fluidRow(
        actionLink("backToHome", "Back", icon = icon("arrow-left"), style = "color: #6397d0; margin-bottom: 15px;"),
      ),
      #----------------------------------------#
      ####----- Filters -----
      #----------------------------------------#
      fluidRow(
        column(
          width = 3,
          virtualSelectInput(
            inputId = "report_teamSelect",
            label = "Select Team:",
            choices = c(teams$proper_name),
            multiple = TRUE,
            search = TRUE,
            disableOptionGroupCheckbox = TRUE,
            width = "100%",
            dropboxWrapper = "body"
          )
        ),
        column(
          width = 3,
          virtualSelectInput(
            inputId = "report_athlete",
            label = "Select Athlete:", 
            choices = c(reportAth()),
            multiple = FALSE,
            disableOptionGroupCheckbox = TRUE,
            search = TRUE,
            width = "100%",
            dropboxWrapper = "body"
          )
        ),
        column(
          width = 3,
          virtualSelectInput(
            inputId = "report_date",
            label = "Select Date:", 
            choices = c(reportDates()),
            multiple = FALSE,
            disableOptionGroupCheckbox = TRUE,
            search = TRUE,
            width = "100%",
            dropboxWrapper = "body"
          )
        ),
        column(
          width = 12,
          actionButton("report_load", "Load Data", icon = icon("download"), style = "color: #6397d0; margin-bottom: 15px;")
        )
      ),
      #----------------------------------------#
      ####----- Anthropometrics -----
      #----------------------------------------#
      fluidRow(
        column(
          width = 12,
          box(
            title = "Anthropometrics",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("anthroTable")
          )
        )
      ),
      #----------------------------------------#
      ####----- CMJ & SJ -----
      #----------------------------------------#
      fluidRow(
        column(
          width = 6,
          box(
            title = "Counter Movement Jump",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("cmjTable")
          )
        ),
        column(
          width = 6,
          box(
            title = "Squat Jump",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("sjTable")
          )
        )
      ),
      #----------------------------------------#
      ####----- Multi-Rebound & Broad -----
      #----------------------------------------#
      fluidRow(
        column(
          width = 4,
          box(
            title = "Vertical Jump",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("vertTable")
          )
        ),
        column(
          width = 4,
          box(
            title = "Multi-Rebound",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("mrTable")
          )
        ),
        column(
          width = 4,
          box(
            title = "Broad Jump",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("broadTable")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            title = "5-10-5",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("ftfTable")
          )
        ),
        column(
          width = 6,
          box(
            title = "Lane Agility",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("laneAgilityTable")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            title = "40 Yard Sprint",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("fortyTable")
          )
        ),
        column(
          width = 6,
          box(
            title = "3/4 Court Sprint",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("threeQTable")
          )
        )
      )
    )
  })
  
  
  
  
}

shinyApp(ui, server, enableBookmarking = "server")

