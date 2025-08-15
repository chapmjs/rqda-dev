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
      "))
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
              width = 12,
              h4("Web-based Qualitative Data Analysis"),
              p("This application provides a web interface for qualitative data analysis,
                similar to the R-based RQDA package. You can manage projects, import files,
                create codes, perform coding, and analyze your qualitative data."),
              br(),
              projectSummaryUI("project_summary")
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
  # Initialize database connection
  db_pool <- db_connect_pool(DB_CONFIG)
  
  # Authentication
  auth_result <- callModule(authenticationServer, "auth", db_pool)
  
  # Authentication status
  output$user_authenticated <- reactive({
    !is.null(auth_result$user_id())
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
    if (exists("db_pool")) {
      pool::poolClose(db_pool)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
