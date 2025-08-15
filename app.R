# app.R - Main Shiny Application File
# RQDA Online - Web-based Qualitative Data Analysis

# ---- Packages ----
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(RMySQL)
  library(DBI)
  library(shinyjs)
  library(shinyWidgets)
  library(shinycssloaders)
  library(stringr)
  library(dplyr)
  library(plotly)
  library(pool)
})

# ---- Sources (these must exist) ----
source("config.R")                     # get_db_config()
source("modules/logging.R")            # log_info/log_warn/log_error/...
source("modules/db_helpers.R")         # db_get()/db_exec()
source("modules/database.R")           # db_connect_pool()/db_disconnect_pool()

# ---- App-specific modules ----
source("modules/authentication.R")
source("modules/project_management.R")
source("modules/file_management.R")
source("modules/coding_system.R")
source("modules/analysis_tools.R")

# ---- Options ----
options(shiny.maxRequestSize = 50 * 1024^2)  # 50MB uploads
options(stringsAsFactors = FALSE)

# -------------------------------------------------------------------
# One-time DB pool for the entire app
DB_CONFIG <- get_db_config()

pool <- NULL
init_pool <- function() {
  if (is.null(pool)) {
    log_info("Initializing DB pool...")
    pool <<- db_connect_pool(DB_CONFIG)
    shiny::onStop(function() {
      log_info("Shutting down DB pool...")
      try(db_disconnect_pool(pool), silent = TRUE)
    })
  }
}
init_pool()

# Optional: capture SSL cipher status (handy to show in the UI)
db_ssl_cipher <- tryCatch({
  val <- DBI::dbGetQuery(pool, "SHOW STATUS LIKE 'Ssl_cipher'")
  if (nrow(val) && nzchar(val$Value[1])) val$Value[1] else ""
}, error = function(e) "")

# Make shared objects easy to reach from modules (if needed)
assign("POOL", pool, envir = .GlobalEnv)
assign("DB_CONFIG", DB_CONFIG, envir = .GlobalEnv)
assign("DB_SSL_CIPHER", db_ssl_cipher, envir = .GlobalEnv)

# -------------------------------------------------------------------
# UI
ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = "RQDA Online",
    titleWidth = 250,
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      textOutput("current_user")
    )
  ),

  # Sidebar (Font Awesome 4 icon names for shinydashboard)
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("gauge")),        # was tachometer
      menuItem("Projects",  tabName = "projects",  icon = icon("folder-open")),
      menuItem("Files",     tabName = "files",     icon = icon("file-lines")),    # was file-text-o (FA4)
      menuItem("Codes",     tabName = "codes",     icon = icon("tags")),
      menuItem("Coding",    tabName = "coding",    icon = icon("highlighter")),   # was highlight
      menuItem("Analysis",  tabName = "analysis",  icon = icon("chart-bar")),     # FA6 still supports this
      menuItem("Export",    tabName = "export",    icon = icon("download")),
      br(),
      conditionalPanel(
        condition = "output.user_authenticated",
        actionButton(
          "logout", "Logout",
          icon  = icon("right-from-bracket"),  # was sign-out / sign-out-alt
          class = "btn-warning", style = "margin: 10px;"
        )
      )
    )

  ),

  # Body
  dashboardBody(
    useShinyjs(),

    # Custom CSS
    tags$head(
      tags$style(HTML("
        .main-header .navbar {
          margin-left: 250px;
        }
        .content-wrapper {
          background-color: #f4f4f4;
        }
        .coding-text {
          line-height: 1.6;
          font-family: 'Georgia', serif;
        }
        .coded-segment {
          background-color: #ffff99;
          border: 1px solid #ffcc00;
          border-radius: 3px;
          padding: 2px;
          margin: 1px;
        }
        .login-panel {
          max-width: 400px;
          margin: 100px auto;
        }
        .connection-status {
          position: fixed;
          top: 10px;
          right: 10px;
          z-index: 9999;
          padding: 5px 10px;
          border-radius: 3px;
          font-size: 12px;
        }
        .ssl-enabled {
          background-color: #d4edda;
          color: #155724;
          border: 1px solid #c3e6cb;
        }
        .ssl-disabled {
          background-color: #f8d7da;
          color: #721c24;
          border: 1px solid #f5c6cb;
        }
      "))
    ),

    # SSL Connection Status Indicator
    div(
      id = "ssl_status",
      class = "connection-status",
      uiOutput("ssl_status_indicator")
    ),

    # Conditional UI based on authentication
    conditionalPanel(
      condition = "!output.user_authenticated",
      authenticationUI("auth")
    ),

    conditionalPanel(
      condition = "output.user_authenticated",
      tabItems(
        # Dashboard tab
        tabItem(
          tabName = "dashboard",
          fluidRow(
            box(
              title = "Welcome to RQDA Online",
              status = "primary",
              solidHeader = TRUE,
              width = 8,
              h4("Web-based Qualitative Data Analysis"),
              p("This application provides a web interface for qualitative data analysis,
                similar to the R-based RQDA package. You can manage projects, import files,
                create codes, perform coding, and analyze your qualitative data."),
              br(),
              projectSummaryUI("project_summary")
            ),
            box(
              title = "Connection Status",
              status = "info",
              solidHeader = TRUE,
              width = 4,
              verbatimTextOutput("connection_details")
            )
          )
        ),

        # Projects tab
        tabItem(
          tabName = "projects",
          projectManagementUI("project_mgmt")
        ),

        # Files tab
        tabItem(
          tabName = "files",
          fileManagementUI("file_mgmt")
        ),

        # Codes tab
        tabItem(
          tabName = "codes",
          codingSystemUI("coding_system")
        ),

        # Coding tab
        tabItem(
          tabName = "coding",
          fluidRow(
            box(
              title = "Text Coding Interface",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              height = "600px",
              textCodingUI("text_coding")
            )
          )
        ),

        # Analysis tab
        tabItem(
          tabName = "analysis",
          analysisToolsUI("analysis_tools")
        ),

        # Export tab
        tabItem(
          tabName = "export",
          fluidRow(
            box(
              title = "Export Data",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              exportDataUI("export_data")
            )
          )
        )
      )
    )
  )
)

# -------------------------------------------------------------------
# Server
server <- function(input, output, session) {
  # Reactive values for connection status
  connection_status <- reactiveValues(
    ssl_enabled = FALSE,
    ssl_cipher = NULL,
    connection_error = NULL,
    pool = NULL
  )

  # Initialize from global pool created at startup
  connection_status$pool        <- POOL
  connection_status$ssl_enabled <- nzchar(DB_SSL_CIPHER)
  connection_status$ssl_cipher  <- if (nzchar(DB_SSL_CIPHER)) DB_SSL_CIPHER else NULL

  # SSL Status Indicator
  output$ssl_status_indicator <- renderUI({
    if (connection_status$ssl_enabled) {
      div(class = "ssl-enabled", icon("lock"), " SSL Enabled")
    } else if (!is.null(connection_status$connection_error)) {
      div(class = "ssl-disabled", icon("exclamation-triangle"), " Connection Issues")
    } else {
      div(class = "ssl-disabled", icon("unlock"), " SSL Disabled")
    }
  })

  # Connection Details
  output$connection_details <- renderText({
    details <- c()

    if (connection_status$ssl_enabled) {
      details <- c(details, "✅ SSL Connection: Active")
      if (!is.null(connection_status$ssl_cipher)) {
        details <- c(details, paste("🔐 SSL Cipher:", connection_status$ssl_cipher))
      }
    } else {
      details <- c(details, "⚠️ SSL Connection: Inactive")
    }

    if (!is.null(connection_status$pool)) {
      details <- c(details, "✅ Database Pool: Connected")

      # Test basic connectivity
      test_result <- tryCatch({
        res <- pool::dbGetQuery(connection_status$pool, "SELECT 1 as test")
        "✅ Database Query: Working"
      }, error = function(e) {
        paste("❌ Database Query: Failed -", e$message)
      })
      details <- c(details, test_result)
    } else {
      details <- c(details, "❌ Database Pool: Not Connected")
    }

    if (!is.null(connection_status$connection_error)) {
      details <- c(details, "", "Connection Error:")
      details <- c(details, connection_status$connection_error)
    }

    paste(details, collapse = "\n")
  })

  # Provide pool to modules
  db_pool <- reactive({ connection_status$pool })

  # Authentication
  auth_result <- callModule(authenticationServer, "auth", db_pool)

  # Authentication status for conditionalPanel
  output$user_authenticated <- reactive({
    !is.null(auth_result$user_id()) && !is.null(db_pool())
  })
  outputOptions(output, "user_authenticated", suspendWhenHidden = FALSE)

  # Current user display
  output$current_user <- renderText({
    if (!is.null(auth_result$username())) {
      paste("Welcome,", auth_result$username())
    } else {
      ""
    }
  })

  # Project management
  project_data <- callModule(
    projectManagementServer,
    "project_mgmt",
    db_pool,
    auth_result$user_id
  )

  # Project summary
  callModule(
    projectSummaryServer,
    "project_summary",
    db_pool,
    auth_result$user_id,
    project_data$current_project_id
  )

  # File management
  file_data <- callModule(
    fileManagementServer,
    "file_mgmt",
    db_pool,
    auth_result$user_id,
    project_data$current_project_id
  )

  # Coding system
  code_data <- callModule(
    codingSystemServer,
    "coding_system",
    db_pool,
    auth_result$user_id,
    project_data$current_project_id
  )

  # Text coding
  callModule(
    textCodingServer,
    "text_coding",
    db_pool,
    auth_result$user_id,
    project_data$current_project_id,
    file_data$selected_file,
    code_data$codes
  )

  # Analysis tools
  callModule(
    analysisToolsServer,
    "analysis_tools",
    db_pool,
    auth_result$user_id,
    project_data$current_project_id
  )

  # Export data
  callModule(
    exportDataServer,
    "export_data",
    db_pool,
    auth_result$user_id,
    project_data$current_project_id
  )

  # Logout = hard reload
  observeEvent(input$logout, {
    session$reload()
  })
}

# -------------------------------------------------------------------
# Run the application
shinyApp(ui = ui, server = server)
