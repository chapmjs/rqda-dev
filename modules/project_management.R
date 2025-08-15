# modules/project_management.R
# Project management functionality

library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)

# Source database functions if not already loaded
if (!exists("db_execute_query")) {
  source("modules/database.R")
}

# Project Management UI
projectManagementUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Project list and management
    box(
      title = "My Projects",
      status = "primary",
      solidHeader = TRUE,
      width = 8,
      
      # Action buttons
      fluidRow(
        column(
          width = 12,
          actionButton(
            ns("create_project"),
            "New Project",
            icon = icon("plus"),
            class = "btn-success",
            style = "margin-bottom: 15px;"
          ),
          actionButton(
            ns("refresh_projects"),
            "Refresh",
            icon = icon("refresh"),
            class = "btn-info",
            style = "margin-bottom: 15px; margin-left: 10px;"
          ),
          conditionalPanel(
            condition = paste0("output['", ns("has_selected_project"), "']"),
            actionButton(
              ns("open_project"),
              "Open Selected",
              icon = icon("folder-open"),
              class = "btn-primary",
              style = "margin-bottom: 15px; margin-left: 10px;"
            ),
            actionButton(
              ns("duplicate_project"),
              "Duplicate",
              icon = icon("copy"),
              class = "btn-warning",
              style = "margin-bottom: 15px; margin-left: 10px;"
            ),
            actionButton(
              ns("delete_project"),
              "Delete Selected",
              icon = icon("trash"),
              class = "btn-danger",
              style = "margin-bottom: 15px; margin-left: 10px;"
            )
          )
        )
      ),
      
      # Projects table
      div(
        style = "margin-top: 10px;",
        withSpinner(DT::dataTableOutput(ns("projects_table")))
      )
    ),
    
    # Current project info and actions
    box(
      title = "Current Project",
      status = "info",
      solidHeader = TRUE,
      width = 4,
      
      conditionalPanel(
        condition = paste0("output['", ns("has_current_project"), "']"),
        
        div(
          style = "text-align: center; margin-bottom: 15px;",
          h4(textOutput(ns("current_project_name")), style = "color: #337ab7; margin-bottom: 5px;"),
          tags$small(textOutput(ns("current_project_date")), style = "color: #666;")
        ),
        
        hr(),
        
        # Project statistics
        fluidRow(
          column(6, 
            div(style = "text-align: center;",
              h4(textOutput(ns("current_project_files")), style = "color: #5cb85c; margin-bottom: 2px;"),
              tags$small("Files", style = "color: #666;")
            )
          ),
          column(6,
            div(style = "text-align: center;",
              h4(textOutput(ns("current_project_codings")), style = "color: #f0ad4e; margin-bottom: 2px;"),
              tags$small("Codings", style = "color: #666;")
            )
          )
        ),
        
        fluidRow(
          column(6,
            div(style = "text-align: center;",
              h4(textOutput(ns("current_project_codes")), style = "color: #d9534f; margin-bottom: 2px;"),
              tags$small("Codes", style = "color: #666;")
            )
          ),
          column(6,
            div(style = "text-align: center;",
              h4(textOutput(ns("current_project_owner")), style = "color: #5bc0de; margin-bottom: 2px;"),
              tags$small("Owner", style = "color: #666;")
            )
          )
        ),
        
        br(),
        
        # Project actions
        fluidRow(
          column(12,
            actionButton(
              ns("project_memo"),
              "Project Memo",
              icon = icon("sticky-note"),
              class = "btn-info btn-sm",
              style = "width: 100%; margin-bottom: 5px;"
            ),
            actionButton(
              ns("project_settings"),
              "Project Settings",
              icon = icon("cog"),
              class = "btn-warning btn-sm",
              style = "width: 100%; margin-bottom: 5px;"
            ),
            actionButton(
              ns("project_sharing"),
              "Share Project",
              icon = icon("share"),
              class = "btn-success btn-sm",
              style = "width: 100%; margin-bottom: 5px;"
            ),
            actionButton(
              ns("export_project"),
              "Export Data",
              icon = icon("download"),
              class = "btn-default btn-sm",
              style = "width: 100%;"
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = paste0("!output['", ns("has_current_project"), "']"),
        div(
          style = "text-align: center; padding: 30px;",
          icon("folder", "fa-3x", class = "text-muted"),
          h4("No Project Selected", class = "text-muted", style = "margin-top: 15px;"),
          p("Create a new project or select an existing one to get started.", class = "text-muted"),
          br(),
          actionButton(
            ns("quick_create"),
            "Quick Create Project",
            icon = icon("plus"),
            class = "btn-success"
          )
        )
      )
    )
  )
}

# Project Summary UI (for dashboard)
projectSummaryUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    valueBoxOutput(ns("total_projects"), width = 3),
    valueBoxOutput(ns("total_files"), width = 3),
    valueBoxOutput(ns("total_codes"), width = 3),
    valueBoxOutput(ns("total_codings"), width = 3)
  )
}

# Recent Activity UI
recentActivityUI <- function(id) {
  ns <- NS(id)
  
  box(
    title = "Recent Activity",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    height = "400px",
    
    div(
      style = "max-height: 350px; overflow-y: auto;",
      withSpinner(uiOutput(ns("activity_feed")))
    )
  )
}

# Project Management Server
projectManagementServer <- function(input, output, session, db_pool, user_id) {
  ns <- session$ns
  
  # Reactive values
  values <- reactiveValues(
    projects = NULL,
    current_project_id = NULL,
    current_project_info = NULL,
    selected_project_row = NULL,
    activity_log = NULL
  )
  
  # Load user projects
  load_projects <- function() {
    if (!is.null(user_id())) {
      projects <- get_user_projects(db_pool, user_id())
      values$projects <- projects
      
      # Load recent activity
      load_recent_activity()
    }
  }
  
  # Load recent activity
  load_recent_activity <- function() {
    if (!is.null(user_id())) {
      activity_query <- "
        SELECT 'project' as type, project_name as name, date as timestamp, 'Created project' as action
        FROM project 
        WHERE created_by = ? 
        UNION ALL
        SELECT 'file' as type, name, date as timestamp, 'Added file' as action
        FROM source 
        WHERE owner = (SELECT username FROM users WHERE id = ?) AND project_id IN 
          (SELECT id FROM project WHERE created_by = ? OR id IN 
           (SELECT project_id FROM project_permissions WHERE user_id = ?))
        UNION ALL
        SELECT 'coding' as type, seltext as name, date as timestamp, 'Added coding' as action
        FROM coding 
        WHERE owner = (SELECT username FROM users WHERE id = ?) AND project_id IN 
          (SELECT id FROM project WHERE created_by = ? OR id IN 
           (SELECT project_id FROM project_permissions WHERE user_id = ?))
        ORDER BY timestamp DESC 
        LIMIT 10
      "
      
      activity <- db_execute_query(db_pool, activity_query, 
                                 list(user_id(), user_id(), user_id(), user_id(),
                                      user_id(), user_id(), user_id()))
      values$activity_log <- activity
    }
  }
  
  # Load projects on start and when user changes
  observe({
    load_projects()
  })
  
  # Refresh projects
  observeEvent(input$refresh_projects, {
    load_projects()
    showNotification("Projects refreshed", type = "message")
  })
  
  # Render projects table
  output$projects_table <- DT::renderDataTable({
    if (is.null(values$projects)) return(NULL)
    
    # Prepare display data
    display_data <- values$projects
    if (nrow(display_data) > 0) {
      display_data$date <- format(as.POSIXct(display_data$date), "%Y-%m-%d %H:%M")
      display_data$permission_display <- ifelse(display_data$permission_level == "owner", 
                                               "Owner", 
                                               paste0("Shared (", display_data$permission_level, ")"))
    }
    
    DT::datatable(
      display_data[, c("project_name", "permission_display", "date", "file_count", "coding_count")],
      selection = "single",
      rownames = FALSE,
      colnames = c("Project Name", "Access", "Created", "Files", "Codings"),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'lrtip',
        columnDefs = list(
          list(width = '40%', targets = 0),  # Project name column wider
          list(width = '15%', targets = 1),  # Access column
          list(width = '20%', targets = 2),  # Date column
          list(width = '12.5%', targets = 3), # Files column
          list(width = '12.5%', targets = 4)  # Codings column
        )
      )
    ) %>%
      DT::formatStyle(
        "permission_display",
        backgroundColor = DT::styleEqual(
          c("Owner", "Shared (editor)", "Shared (viewer)"),
          c("#d4edda", "#fff3cd", "#f8d7da")
        )
      )
  })
  
  # Track selected project
  observeEvent(input$projects_table_rows_selected, {
    values$selected_project_row <- input$projects_table_rows_selected
  })
  
  # Check if project is selected
  output$has_selected_project <- reactive({
    !is.null(values$selected_project_row)
  })
  outputOptions(output, "has_selected_project", suspendWhenHidden = FALSE)
  
  # Check if current project exists
  output$has_current_project <- reactive({
    !is.null(values$current_project_id)
  })
  outputOptions(output, "has_current_project", suspendWhenHidden = FALSE)
  
  # Create new project modal
  observeEvent(input$create_project, {
    show_create_project_modal()
  })
  
  # Quick create project
  observeEvent(input$quick_create, {
    show_create_project_modal()
  })
  
  # Show create project modal
  show_create_project_modal <- function() {
    showModal(
      modalDialog(
        title = "Create New Project",
        size = "m",
        
        textInput(
          ns("new_project_name"),
          "Project Name *",
          placeholder = "Enter a descriptive project name",
          value = ""
        ),
        
        textAreaInput(
          ns("new_project_description"),
          "Project Description",
          placeholder = "Describe your research project, goals, and methodology...",
          height = "120px",
          value = ""
        ),
        
        selectInput(
          ns("new_project_template"),
          "Project Template",
          choices = list(
            "Blank Project" = "blank",
            "Interview Study" = "interview",
            "Focus Group Study" = "focus_group",
            "Document Analysis" = "document",
            "Mixed Methods" = "mixed"
          ),
          selected = "blank"
        ),
        
        checkboxInput(
          ns("new_project_sample_data"),
          "Include sample data and codes",
          value = FALSE
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_create_project"),
            "Create Project",
            class = "btn-success",
            icon = icon("plus")
          )
        )
      )
    )
  }
  
  # Confirm create project
  observeEvent(input$confirm_create_project, {
    if (is.null(input$new_project_name) || input$new_project_name == "") {
      showNotification("Please enter a project name", type = "error")
      return()
    }
    
    # Get username for owner field
    user_info_query <- "SELECT username FROM users WHERE id = ?"
    user_info <- db_execute_query(db_pool, user_info_query, list(user_id()))
    owner_name <- if (!is.null(user_info)) user_info$username[1] else "Unknown"
    
    result <- create_project(
      db_pool, 
      input$new_project_name, 
      user_id(), 
      owner_name
    )
    
    if (result$success) {
      # Update project memo if provided
      if (!is.null(input$new_project_description) && input$new_project_description != "") {
        memo_query <- "UPDATE project SET memo = ? WHERE id = ?"
        pool::dbExecute(db_pool, memo_query, list(input$new_project_description, result$project_id))
      }
      
      # Add sample data if requested
      if (input$new_project_sample_data) {
        add_sample_data(result$project_id, input$new_project_template)
      }
      
      showNotification("Project created successfully!", type = "message")
      removeModal()
      load_projects()
      
      # Auto-select the new project
      values$current_project_id <- result$project_id
      load_current_project_info()
      
    } else {
      showNotification(result$message, type = "error")
    }
  })
  
  # Add sample data function
  add_sample_data <- function(project_id, template_type) {
    tryCatch({
      if (template_type == "interview") {
        # Add sample interview codes
        sample_codes <- list(
          list(name = "Background", color = "#FFE4B5"),
          list(name = "Experiences", color = "#E6F3FF"),
          list(name = "Challenges", color = "#FFE4E1"),
          list(name = "Opportunities", color = "#F0FFF0")
        )
        
        for (code in sample_codes) {
          code_query <- "INSERT INTO freecode (name, color, project_id, owner, status) VALUES (?, ?, ?, ?, 1)"
          pool::dbExecute(db_pool, code_query, list(code$name, code$color, project_id, "System"))
        }
      }
      # Add other template types as needed
    }, error = function(e) {
      # Silently fail sample data creation
    })
  }
  
  # Open selected project
  observeEvent(input$open_project, {
    if (!is.null(values$selected_project_row) && !is.null(values$projects)) {
      selected_project <- values$projects[values$selected_project_row, ]
      values$current_project_id <- selected_project$id
      load_current_project_info()
      showNotification(paste("Opened project:", selected_project$project_name), type = "message")
    }
  })
  
  # Duplicate project
  observeEvent(input$duplicate_project, {
    if (!is.null(values$selected_project_row) && !is.null(values$projects)) {
      selected_project <- values$projects[values$selected_project_row, ]
      
      showModal(
        modalDialog(
          title = "Duplicate Project",
          size = "m",
          
          textInput(
            ns("duplicate_project_name"),
            "New Project Name",
            value = paste(selected_project$project_name, "- Copy")
          ),
          
          checkboxInput(
            ns("duplicate_include_files"),
            "Include files",
            value = TRUE
          ),
          
          checkboxInput(
            ns("duplicate_include_codes"),
            "Include codes and categories",
            value = TRUE
          ),
          
          checkboxInput(
            ns("duplicate_include_codings"),
            "Include codings",
            value = FALSE
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_duplicate_project"),
              "Duplicate Project",
              class = "btn-warning"
            )
          )
        )
      )
    }
  })
  
  # Confirm duplicate project
  observeEvent(input$confirm_duplicate_project, {
    # Duplication logic would go here
    showNotification("Project duplication functionality coming soon!", type = "message")
    removeModal()
  })
  
  # Delete project confirmation
  observeEvent(input$delete_project, {
    if (!is.null(values$selected_project_row) && !is.null(values$projects)) {
      selected_project <- values$projects[values$selected_project_row, ]
      
      # Check if user has owner permission
      if (selected_project$permission_level != "owner") {
        showNotification("Only project owners can delete projects", type = "error")
        return()
      }
      
      showModal(
        modalDialog(
          title = "Delete Project",
          size = "m",
          
          div(
            class = "alert alert-danger",
            icon("exclamation-triangle"),
            strong(" Warning: "),
            "This action cannot be undone!"
          ),
          
          p(paste("Are you sure you want to delete the project:", 
                  strong(selected_project$project_name), "?")),
          
          p("This will permanently delete:"),
          tags$ul(
            tags$li(paste(selected_project$file_count, "files and their content")),
            tags$li(paste(selected_project$coding_count, "codings")),
            tags$li("All codes and categories"),
            tags$li("All memos and annotations"),
            tags$li("All project data and settings")
          ),
          
          br(),
          textInput(
            ns("confirm_delete_text"),
            paste('Type "DELETE" to confirm:'),
            placeholder = "DELETE"
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_delete_project"),
              "Delete Project",
              class = "btn-danger"
            )
          )
        )
      )
    }
  })
  
  # Confirm delete project
  observeEvent(input$confirm_delete_project, {
    if (!is.null(values$selected_project_row) && !is.null(values$projects)) {
      
      if (input$confirm_delete_text != "DELETE") {
        showNotification("Please type 'DELETE' to confirm", type = "error")
        return()
      }
      
      selected_project <- values$projects[values$selected_project_row, ]
      
      tryCatch({
        # Delete project (CASCADE will handle related data)
        delete_query <- "DELETE FROM project WHERE id = ? AND created_by = ?"
        pool::dbExecute(db_pool, delete_query, list(selected_project$id, user_id()))
        
        showNotification("Project deleted successfully", type = "message")
        removeModal()
        
        # Clear current project if it was deleted
        if (!is.null(values$current_project_id) && values$current_project_id == selected_project$id) {
          values$current_project_id <- NULL
          values$current_project_info <- NULL
        }
        
        load_projects()
        
      }, error = function(e) {
        showNotification(paste("Error deleting project:", e$message), type = "error")
      })
    }
  })
  
  # Load current project information
  load_current_project_info <- function() {
    if (!is.null(values$current_project_id)) {
      query <- "
        SELECT p.*, 
               (SELECT COUNT(*) FROM source WHERE project_id = p.id AND status = 1) as file_count,
               (SELECT COUNT(*) FROM coding WHERE project_id = p.id AND status = 1) as coding_count,
               (SELECT COUNT(*) FROM freecode WHERE project_id = p.id AND status = 1) as code_count
        FROM project p 
        WHERE p.id = ?
      "
      
      info <- db_execute_query(db_pool, query, list(values$current_project_id))
      values$current_project_info <- info
    }
  }
  
  # Render current project info
  output$current_project_name <- renderText({
    if (!is.null(values$current_project_info)) {
      values$current_project_info$project_name[1]
    }
  })
  
  output$current_project_owner <- renderText({
    if (!is.null(values$current_project_info)) {
      values$current_project_info$owner[1]
    }
  })
  
  output$current_project_date <- renderText({
    if (!is.null(values$current_project_info)) {
      format(as.POSIXct(values$current_project_info$date[1]), "%Y-%m-%d")
    }
  })
  
  output$current_project_files <- renderText({
    if (!is.null(values$current_project_info)) {
      values$current_project_info$file_count[1]
    } else {
      "0"
    }
  })
  
  output$current_project_codings <- renderText({
    if (!is.null(values$current_project_info)) {
      values$current_project_info$coding_count[1]
    } else {
      "0"
    }
  })
  
  output$current_project_codes <- renderText({
    if (!is.null(values$current_project_info)) {
      values$current_project_info$code_count[1]
    } else {
      "0"
    }
  })
  
  # Project memo modal
  observeEvent(input$project_memo, {
    if (!is.null(values$current_project_info)) {
      current_memo <- values$current_project_info$memo[1]
      if (is.na(current_memo)) current_memo <- ""
      
      showModal(
        modalDialog(
          title = paste("Project Memo -", values$current_project_info$project_name[1]),
          size = "l",
          
          textAreaInput(
            ns("project_memo_text"),
            "Project Memo",
            value = current_memo,
            height = "300px",
            placeholder = "Document your research approach, findings, and insights..."
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("save_project_memo"),
              "Save Memo",
              class = "btn-primary",
              icon = icon("save")
            )
          )
        )
      )
    }
  })
  
  # Save project memo
  observeEvent(input$save_project_memo, {
    if (!is.null(values$current_project_id)) {
      memo_query <- "UPDATE project SET memo = ?, dateM = NOW() WHERE id = ?"
      
      tryCatch({
        pool::dbExecute(db_pool, memo_query, list(input$project_memo_text, values$current_project_id))
        showNotification("Project memo saved", type = "message")
        removeModal()
        load_current_project_info()
      }, error = function(e) {
        showNotification(paste("Error saving memo:", e$message), type = "error")
      })
    }
  })
  
  # Project settings modal
  observeEvent(input$project_settings, {
    if (!is.null(values$current_project_info)) {
      showModal(
        modalDialog(
          title = paste("Project Settings -", values$current_project_info$project_name[1]),
          size = "m",
          
          h4("Project Information"),
          p(strong("Project ID: "), values$current_project_info$id[1]),
          p(strong("Database Version: "), values$current_project_info$databaseversion[1]),
          p(strong("Created: "), format(as.POSIXct(values$current_project_info$date[1]), "%Y-%m-%d %H:%M")),
          p(strong("Last Modified: "), format(as.POSIXct(values$current_project_info$dateM[1]), "%Y-%m-%d %H:%M")),
          
          hr(),
          
          h4("Project Statistics"),
          p(strong("Files: "), values$current_project_info$file_count[1]),
          p(strong("Codes: "), values$current_project_info$code_count[1]),
          p(strong("Codings: "), values$current_project_info$coding_count[1]),
          
          hr(),
          
          h4("Project Settings"),
          textInput(
            ns("settings_project_name"),
            "Project Name",
            value = values$current_project_info$project_name[1]
          ),
          
          footer = tagList(
            modalButton("Close"),
            actionButton(
              ns("save_project_settings"),
              "Save Settings",
              class = "btn-primary"
            )
          )
        )
      )
    }
  })
  
  # Save project settings
  observeEvent(input$save_project_settings, {
    if (!is.null(values$current_project_id)) {
      settings_query <- "UPDATE project SET project_name = ?, dateM = NOW() WHERE id = ?"
      
      tryCatch({
        pool::dbExecute(db_pool, settings_query, 
                       list(input$settings_project_name, values$current_project_id))
        showNotification("Project settings saved", type = "message")
        removeModal()
        load_current_project_info()
        load_projects()  # Refresh project list
      }, error = function(e) {
        showNotification(paste("Error saving settings:", e$message), type = "error")
      })
    }
  })
  
  # Project sharing modal
  observeEvent(input$project_sharing, {
    if (!is.null(values$current_project_id)) {
      showNotification("Project sharing functionality coming soon!", type = "message")
    }
  })
  
  # Export project
  observeEvent(input$export_project, {
    if (!is.null(values$current_project_id)) {
      showNotification("Project export functionality coming soon!", type = "message")
    }
  })
  
  # Return reactive values for use by other modules
  return(list(
    current_project_id = reactive({ values$current_project_id }),
    current_project_info = reactive({ values$current_project_info }),
    projects = reactive({ values$projects }),
    activity_log = reactive({ values$activity_log })
  ))
}

# Project Summary Server (for dashboard)
projectSummaryServer <- function(input, output, session, db_pool, user_id, current_project_id) {
  
  # Get summary statistics
  get_user_stats <- function() {
    if (is.null(user_id())) return(NULL)
    
    query <- "
      SELECT 
        COUNT(DISTINCT p.id) as total_projects,
        COUNT(DISTINCT s.id) as total_files,
        COUNT(DISTINCT f.id) as total_codes,
        COUNT(DISTINCT c.rowid) as total_codings
      FROM project p
      LEFT JOIN project_permissions pp ON p.id = pp.project_id
      LEFT JOIN source s ON p.id = s.project_id AND s.status = 1
      LEFT JOIN freecode f ON p.id = f.project_id AND f.status = 1
      LEFT JOIN coding c ON p.id = c.project_id AND c.status = 1
      WHERE p.created_by = ? OR pp.user_id = ?
    "
    
    db_execute_query(db_pool, query, list(user_id(), user_id()))
  }
  
  # Get current project stats
  get_current_project_stats <- function() {
    if (is.null(current_project_id())) return(NULL)
    
    query <- "
      SELECT 
        COUNT(DISTINCT s.id) as project_files,
        COUNT(DISTINCT f.id) as project_codes,
        COUNT(DISTINCT c.rowid) as project_codings
      FROM project p
      LEFT JOIN source s ON p.id = s.project_id AND s.status = 1
      LEFT JOIN freecode f ON p.id = f.project_id AND f.status = 1
      LEFT JOIN coding c ON p.id = c.project_id AND c.status = 1
      WHERE p.id = ?
    "
    
    db_execute_query(db_pool, query, list(current_project_id()))
  }
  
  # Value boxes
  output$total_projects <- renderValueBox({
    stats <- get_user_stats()
    value <- if (!is.null(stats)) stats$total_projects[1] else 0
    
    valueBox(
      value = value,
      subtitle = "Total Projects",
      icon = icon("folder"),
      color = "blue"
    )
  })
  
  output$total_files <- renderValueBox({
    if (!is.null(current_project_id())) {
      stats <- get_current_project_stats()
      value <- if (!is.null(stats)) stats$project_files[1] else 0
      subtitle <- "Files in Current Project"
      color <- "green"
    } else {
      stats <- get_user_stats()
      value <- if (!is.null(stats)) stats$total_files[1] else 0
      subtitle <- "Total Files"
      color <- "green"
    }
    
    valueBox(
      value = value,
      subtitle = subtitle,
      icon = icon("file-text"),
      color = color
    )
  })
  
  output$total_codes <- renderValueBox({
    if (!is.null(current_project_id())) {
      stats <- get_current_project_stats()
      value <- if (!is.null(stats)) stats$project_codes[1] else 0
      subtitle <- "Codes in Current Project"
      color <- "yellow"
    } else {
      stats <- get_user_stats()
      value <- if (!is.null(stats)) stats$total_codes[1] else 0
      subtitle <- "Total Codes"
      color <- "yellow"
    }
    
    valueBox(
      value = value,
      subtitle = subtitle,
      icon = icon("tags"),
      color = color
    )
  })
  
  output$total_codings <- renderValueBox({
    if (!is.null(current_project_id())) {
      stats <- get_current_project_stats()
      value <- if (!is.null(stats)) stats$project_codings[1] else 0
      subtitle <- "Codings in Current Project"
      color <- "red"
    } else {
      stats <- get_user_stats()
      value <- if (!is.null(stats)) stats$total_codings[1] else 0
      subtitle <- "Total Codings"
      color <- "red"
    }
    
    valueBox(
      value = value,
      subtitle = subtitle,
      icon = icon("highlight"),
      color = color
    )
  })
}

# Recent Activity Server
recentActivityServer <- function(input, output, session, db_pool, user_id, activity_log) {
  
  # Render activity feed
  output$activity_feed <- renderUI({
    if (is.null(user_id()) || is.null(activity_log()) || nrow(activity_log()) == 0) {
      return(
        div(
          style = "text-align: center; padding: 50px; color: #666;",
          icon("clock", "fa-2x"),
          h4("No Recent Activity", style = "margin-top: 15px;"),
          p("Start working on your projects to see activity here.")
        )
      )
    }
    
    activity_items <- lapply(1:nrow(activity_log()), function(i) {
      item <- activity_log()[i, ]
      
      # Format timestamp
      time_diff <- difftime(Sys.time(), as.POSIXct(item$timestamp), units = "hours")
      if (time_diff < 1) {
        time_str <- "Just now"
      } else if (time_diff < 24) {
        time_str <- paste(floor(time_diff), "hours ago")
      } else {
        time_str <- format(as.POSIXct(item$timestamp), "%Y-%m-%d")
      }
      
      # Choose icon based on type
      icon_name <- switch(item$type,
                         "project" = "folder",
                         "file" = "file-text",
                         "coding" = "highlight",
                         "code" = "tag",
                         "clock")
      
      # Choose color based on type
      icon_color <- switch(item$type,
                          "project" = "#337ab7",
                          "file" = "#5cb85c", 
                          "coding" = "#f0ad4e",
                          "code" = "#d9534f",
                          "#666")
      
      div(
        style = "border-left: 3px solid #ddd; padding-left: 15px; margin-bottom: 15px;",
        div(
          style = "display: flex; align-items: center; margin-bottom: 5px;",
          icon(icon_name, style = paste0("color: ", icon_color, "; margin-right: 8px;")),
          strong(item$action),
          tags$small(time_str, style = "margin-left: auto; color: #666;")
        ),
        div(
          style = "color: #333; margin-left: 20px;",
          if (nchar(item$name) > 50) {
            paste0(substr(item$name, 1, 50), "...")
          } else {
            item$name
          }
        )
      )
    })
    
    do.call(tagList, activity_items)
  })
}
