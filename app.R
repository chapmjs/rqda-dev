# app.R - Main Shiny Application File
# RQDA Online - Web-based Qualitative Data Analysis

# Load required libraries
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

# Source configuration and validation
source("config.R")

# Source modules
source("modules/database.R")
source("modules/authentication.R")
source("modules/project_management.R")
source("modules/file_management.R")
source("modules/coding_system.R")
source("modules/analysis_tools.R")

# Validate environment and database connection at startup
tryCatch({
  startup_validation()
}, error = function(e) {
  cat("❌ Startup validation failed:", e$message, "\n")
  cat("Please check your configuration and try again.\n")
  stop(e$message)
})

# Database configuration - all parameters from environment variables
DB_CONFIG <- get_db_config()

# Test SSL connection before starting the app
cat("🔐 Testing SSL database connection...\n")
ssl_test_result <- tryCatch({
  test_ssl_connection(DB_CONFIG)
}, error = function(e) {
  cat("❌ SSL connection test failed:", e$message, "\n")
  FALSE
})

if (ssl_test_result) {
  cat("✅ SSL connection test successful\n")
} else {
  cat("⚠️ SSL connection test failed - app will attempt connection anyway\n")
}

# Define UI
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
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Projects", tabName = "projects", icon = icon("folder-open")),
      menuItem("Files", tabName = "files", icon = icon("file-text")),
      menuItem("Codes", tabName = "codes", icon = icon("tags")),
      menuItem("Coding", tabName = "coding", icon = icon("highlight")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Export", tabName = "export", icon = icon("download")),
      br(),
      conditionalPanel(
        condition = "output.user_authenticated",
        actionButton("logout", "Logout", icon = icon("sign-out-alt"), 
                    class = "btn-warning", style = "margin: 10px;")
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

# Define Server
server <- function(input, output, session) {
  # Reactive values for connection status
  connection_status <- reactiveValues(
    ssl_enabled = FALSE,
    ssl_cipher = NULL,
    connection_error = NULL,
    pool = NULL
  )
  
  # Initialize database connection with SSL and automatic table creation
  observe({
    isolate({
      cat("🔗 Initializing SSL database connection...\n")
      
      connection_status$pool <- tryCatch({
        # Attempt SSL connection
        pool <- db_connect_pool(DB_CONFIG)
        
        # Test SSL status
        ssl_check <- tryCatch({
          ssl_status <- pool::dbGetQuery(pool, "SHOW STATUS LIKE 'Ssl_cipher'")
          if (nrow(ssl_status) > 0 && ssl_status$Value != "") {
            connection_status$ssl_enabled <- TRUE
            connection_status$ssl_cipher <- ssl_status$Value
            cat("✅ SSL connection established with cipher:", ssl_status$Value, "\n")
            TRUE
          } else {
            connection_status$ssl_enabled <- FALSE
            cat("⚠️ Connection established but SSL not active\n")
            FALSE
          }
        }, error = function(e) {
          connection_status$ssl_enabled <- FALSE
          cat("⚠️ Could not verify SSL status:", e$message, "\n")
          FALSE
        })
        
        # Create tables if they don't exist
        cat("📋 Initializing database schema...\n")
        table_result <- create_tables_if_not_exist(pool)
        
        if (table_result) {
          cat("✅ Database schema initialized successfully\n")
        } else {
          cat("⚠️ Some database tables could not be created\n")
        }
        
        connection_status$connection_error <- NULL
        pool
        
      }, error = function(e) {
        cat("❌ Database initialization failed:", e$message, "\n")
        connection_status$connection_error <- e$message
        connection_status$ssl_enabled <- FALSE
        
        # Try fallback connection without SSL
        cat("🔄 Attempting fallback connection without SSL...\n")
        tryCatch({
          # Create a temporary config without SSL for fallback
          fallback_config <- DB_CONFIG
          
          fallback_pool <- pool::dbPool(
            drv = RMySQL::MySQL(),
            host = fallback_config$host,
            dbname = fallback_config$dbname,
            username = fallback_config$username,
            password = fallback_config$password,
            port = ifelse(is.null(fallback_config$port), 3306, fallback_config$port),
            encoding = "utf8mb4",
            minSize = 1,
            maxSize = 10
          )
          
          cat("✅ Fallback connection (no SSL) established\n")
          connection_status$connection_error <- paste("SSL failed, using non-SSL connection:", e$message)
          
          # Create tables if they don't exist
          create_tables_if_not_exist(fallback_pool)
          
          fallback_pool
          
        }, error = function(e2) {
          cat("❌ All connection attempts failed:", e2$message, "\n")
          connection_status$connection_error <- paste("All connections failed:", e2$message)
          
          showNotification(
            paste("Database connection failed:", e2$message, "Please check your configuration."), 
            type = "error", 
            duration = NULL
          )
          NULL
        })
      })
    })
  })
  
  # SSL Status Indicator
  output$ssl_status_indicator <- renderUI({
    if (connection_status$ssl_enabled) {
      div(
        class = "ssl-enabled",
        icon("lock"), " SSL Enabled"
      )
    } else if (!is.null(connection_status$connection_error)) {
      div(
        class = "ssl-disabled",
        icon("exclamation-triangle"), " Connection Issues"
      )
    } else {
      div(
        class = "ssl-disabled",
        icon("unlock"), " SSL Disabled"
      )
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
        result <- pool::dbGetQuery(connection_status$pool, "SELECT 1 as test")
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
  
  # Get database pool for use in modules
  db_pool <- reactive({
    connection_status$pool
  })
  
  # Authentication
  auth_result <- callModule(authenticationServer, "auth", db_pool)
  
  # Authentication status
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
  
  # Logout functionality
  observeEvent(input$logout, {
    session$reload()
  })
  
  # Clean up database connections on session end
  session$onSessionEnded(function() {
    if (!is.null(connection_status$pool)) {
      tryCatch({
        pool::poolClose(connection_status$pool)
        cat("🔌 Database connection closed\n")
      }, error = function(e) {
        cat("⚠️ Error closing database connection:", e$message, "\n")
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
