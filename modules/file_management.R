# modules/file_management.R
# File management functionality

library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(stringr)

# Source database functions if not already loaded
if (!exists("db_execute_query")) {
  source("modules/database.R")
}

# File Management UI
fileManagementUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # File list and management
    box(
      title = "Project Files",
      status = "primary",
      solidHeader = TRUE,
      width = 8,
      
      # File management toolbar
      fluidRow(
        column(
          width = 12,
          # Upload section
          div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
            h5("Upload Files", style = "margin-top: 0;"),
            fluidRow(
              column(8,
                fileInput(
                  ns("file_upload"),
                  NULL,
                  multiple = TRUE,
                  accept = c(".txt", ".docx", ".pdf", ".csv", ".rtf"),
                  placeholder = "Choose text files...",
                  buttonLabel = "Browse",
                  width = "100%"
                )
              ),
              column(4,
                actionButton(
                  ns("upload_files"),
                  "Upload Files",
                  icon = icon("upload"),
                  class = "btn-success",
                  style = "margin-top: 25px; width: 100%;"
                )
              )
            ),
            div(
              style = "margin-top: 10px; color: #666; font-size: 0.9em;",
              "Supported formats: TXT, DOCX, PDF, CSV, RTF (Max 10MB per file)"
            )
          ),
          
          # Action buttons
          div(
            style = "margin-bottom: 15px;",
            actionButton(
              ns("refresh_files"),
              "Refresh",
              icon = icon("refresh"),
              class = "btn-info"
            ),
            actionButton(
              ns("add_text_file"),
              "New Text File",
              icon = icon("plus"),
              class = "btn-success",
              style = "margin-left: 10px;"
            ),
            conditionalPanel(
              condition = paste0("output['", ns("has_selected_file"), "']"),
              actionButton(
                ns("view_file"),
                "View Selected",
                icon = icon("eye"),
                class = "btn-primary",
                style = "margin-left: 10px;"
              ),
              actionButton(
                ns("edit_file"),
                "Edit Selected",
                icon = icon("edit"),
                class = "btn-warning",
                style = "margin-left: 10px;"
              ),
              actionButton(
                ns("delete_file"),
                "Delete Selected",
                icon = icon("trash"),
                class = "btn-danger",
                style = "margin-left: 10px;"
              )
            )
          )
        )
      ),
      
      # Files table
      div(
        style = "margin-top: 10px;",
        withSpinner(DT::dataTableOutput(ns("files_table")))
      )
    ),
    
    # File categories and organization
    box(
      title = "File Organization",
      status = "info",
      solidHeader = TRUE,
      width = 4,
      
      # File categories section
      h5("File Categories"),
      div(
        style = "margin-bottom: 15px;",
        actionButton(
          ns("add_category"),
          "Add Category",
          icon = icon("plus"),
          class = "btn-success btn-sm",
          style = "width: 100%;"
        )
      ),
      
      # Categories list
      div(
        id = ns("categories_list"),
        style = "max-height: 200px; overflow-y: auto; margin-bottom: 15px;",
        withSpinner(uiOutput(ns("categories_display")))
      ),
      
      hr(),
      
      # File statistics
      h5("File Statistics"),
      div(
        style = "text-align: center;",
        fluidRow(
          column(6,
            div(
              h4(textOutput(ns("total_files")), style = "color: #5cb85c; margin-bottom: 2px;"),
              tags$small("Total Files", style = "color: #666;")
            )
          ),
          column(6,
            div(
              h4(textOutput(ns("total_size")), style = "color: #f0ad4e; margin-bottom: 2px;"),
              tags$small("Total Size", style = "color: #666;")
            )
          )
        ),
        
        fluidRow(
          column(6,
            div(
              h4(textOutput(ns("coded_files")), style = "color: #d9534f; margin-bottom: 2px;"),
              tags$small("Coded Files", style = "color: #666;")
            )
          ),
          column(6,
            div(
              h4(textOutput(ns("avg_codings")), style = "color: #5bc0de; margin-bottom: 2px;"),
              tags$small("Avg Codings", style = "color: #666;")
            )
          )
        )
      ),
      
      br(),
      
      # Quick actions
      h5("Quick Actions"),
      actionButton(
        ns("bulk_categorize"),
        "Bulk Categorize",
        icon = icon("tags"),
        class = "btn-info btn-sm",
        style = "width: 100%; margin-bottom: 5px;"
      ),
      actionButton(
        ns("export_files"),
        "Export File List",
        icon = icon("download"),
        class = "btn-default btn-sm",
        style = "width: 100%; margin-bottom: 5px;"
      ),
      actionButton(
        ns("file_search"),
        "Search in Files",
        icon = icon("search"),
        class = "btn-warning btn-sm",
        style = "width: 100%;"
      )
    )
  )
}

# File Viewer UI
fileViewerUI <- function(id) {
  ns <- NS(id)
  
  div(
    id = ns("file_viewer_container"),
    style = "display: none;",
    
    box(
      title = "File Viewer",
      status = "primary", 
      solidHeader = TRUE,
      width = 12,
      height = "600px",
      
      # Viewer toolbar
      div(
        style = "margin-bottom: 15px; border-bottom: 1px solid #ddd; padding-bottom: 10px;",
        fluidRow(
          column(8,
            h4(textOutput(ns("viewer_file_name")), style = "margin: 0; color: #337ab7;")
          ),
          column(4,
            div(
              style = "text-align: right;",
              actionButton(
                ns("close_viewer"),
                "Close",
                icon = icon("times"),
                class = "btn-default btn-sm"
              ),
              actionButton(
                ns("edit_in_viewer"),
                "Edit",
                icon = icon("edit"),
                class = "btn-warning btn-sm",
                style = "margin-left: 5px;"
              )
            )
          )
        )
      ),
      
      # File content display
      div(
        style = "height: 450px; overflow-y: auto; background-color: #fff; border: 1px solid #ddd; padding: 15px; font-family: 'Georgia', serif; line-height: 1.6;",
        withSpinner(
          htmlOutput(ns("file_content_display"))
        )
      ),
      
      # File metadata
      div(
        style = "margin-top: 15px; padding-top: 10px; border-top: 1px solid #ddd; font-size: 0.9em; color: #666;",
        fluidRow(
          column(3, textOutput(ns("file_size_display"))),
          column(3, textOutput(ns("file_date_display"))),
          column(3, textOutput(ns("file_owner_display"))),
          column(3, textOutput(ns("file_codings_display")))
        )
      )
    )
  )
}

# File Management Server
fileManagementServer <- function(input, output, session, db_pool, user_id, current_project_id) {
  ns <- session$ns
  
  # Reactive values
  values <- reactiveValues(
    files = NULL,
    categories = NULL,
    selected_file_row = NULL,
    current_file = NULL,
    file_stats = NULL
  )
  
  # Load project files
  load_files <- function() {
    if (!is.null(current_project_id()) && !is.null(user_id())) {
      files <- get_project_files(db_pool, current_project_id(), user_id())
      values$files <- files
      load_file_statistics()
    }
  }
  
  # Load file categories
  load_categories <- function() {
    if (!is.null(current_project_id()) && !is.null(user_id())) {
      categories_query <- "
        SELECT catid, name, memo,
               (SELECT COUNT(*) FROM treefile WHERE catid = filecat.catid AND status = 1) as file_count
        FROM filecat 
        WHERE project_id = ? AND status = 1 
        ORDER BY name
      "
      categories <- db_execute_query(db_pool, categories_query, list(current_project_id()))
      values$categories <- categories
    }
  }
  
  # Load file statistics
  load_file_statistics <- function() {
    if (!is.null(current_project_id()) && !is.null(values$files)) {
      stats <- list(
        total_files = nrow(values$files),
        total_size = sum(values$files$file_size, na.rm = TRUE),
        coded_files = sum(values$files$coding_count > 0, na.rm = TRUE),
        avg_codings = round(mean(values$files$coding_count, na.rm = TRUE), 1)
      )
      values$file_stats <- stats
    }
  }
  
  # Load data when project changes
  observe({
    load_files()
    load_categories()
  })
  
  # Refresh files
  observeEvent(input$refresh_files, {
    load_files()
    load_categories()
    showNotification("Files refreshed", type = "message")
  })
  
  # Render files table
  output$files_table <- DT::renderDataTable({
    if (is.null(values$files) || is.null(current_project_id())) {
      return(data.frame(
        Message = "No project selected or no files found",
        stringsAsFactors = FALSE
      ))
    }
    
    if (nrow(values$files) == 0) {
      return(data.frame(
        Message = "No files in this project. Upload some files to get started!",
        stringsAsFactors = FALSE
      ))
    }
    
    # Prepare display data
    display_data <- values$files
    display_data$file_size_mb <- round(display_data$file_size / 1024, 2)
    display_data$date <- format(as.POSIXct(display_data$date), "%Y-%m-%d %H:%M")
    
    DT::datatable(
      display_data[, c("name", "owner", "date", "file_size_mb", "coding_count")],
      selection = "single",
      rownames = FALSE,
      colnames = c("File Name", "Owner", "Added", "Size (KB)", "Codings"),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'lrtip',
        columnDefs = list(
          list(width = '40%', targets = 0),  # File name column wider
          list(width = '15%', targets = 1),  # Owner column
          list(width = '20%', targets = 2),  # Date column
          list(width = '12.5%', targets = 3), # Size column
          list(width = '12.5%', targets = 4)  # Codings column
        )
      )
    ) %>%
      DT::formatStyle(
        "coding_count",
        backgroundColor = DT::styleInterval(
          cuts = c(0, 1, 10),
          values = c("#f8d7da", "#fff3cd", "#d4edda", "#c3e6cb")
        )
      )
  })
  
  # Track selected file
  observeEvent(input$files_table_rows_selected, {
    values$selected_file_row <- input$files_table_rows_selected
  })
  
  # Check if file is selected
  output$has_selected_file <- reactive({
    !is.null(values$selected_file_row)
  })
  outputOptions(output, "has_selected_file", suspendWhenHidden = FALSE)
  
  # File upload handling
  observeEvent(input$upload_files, {
    if (is.null(input$file_upload)) {
      showNotification("Please select files to upload", type = "warning")
      return()
    }
    
    if (is.null(current_project_id())) {
      showNotification("Please select a project first", type = "error")
      return()
    }
    
    upload_files_to_project()
  })
  
  # Upload files function
  upload_files_to_project <- function() {
    file_info <- input$file_upload
    success_count <- 0
    error_count <- 0
    
    # Get username for owner field
    user_info_query <- "SELECT username FROM users WHERE id = ?"
    user_info <- db_execute_query(db_pool, user_info_query, list(user_id()))
    owner_name <- if (!is.null(user_info)) user_info$username[1] else "Unknown"
    
    for (i in 1:nrow(file_info)) {
      file_path <- file_info$datapath[i]
      file_name <- file_info$name[i]
      file_size <- file_info$size[i]
      
      # Check file size (10MB limit)
      if (file_size > 10 * 1024 * 1024) {
        showNotification(paste("File", file_name, "is too large (max 10MB)"), type = "warning")
        error_count <- error_count + 1
        next
      }
      
      # Read file content based on extension
      file_content <- tryCatch({
        read_file_content(file_path, file_name)
      }, error = function(e) {
        showNotification(paste("Error reading", file_name, ":", e$message), type = "error")
        error_count <<- error_count + 1
        return(NULL)
      })
      
      if (is.null(file_content)) next
      
      # Insert file into database
      insert_query <- "
        INSERT INTO source (name, file, project_id, owner, date, status) 
        VALUES (?, ?, ?, ?, NOW(), 1)
      "
      
      tryCatch({
        pool::dbExecute(db_pool, insert_query, 
                       list(file_name, file_content, current_project_id(), owner_name))
        success_count <- success_count + 1
      }, error = function(e) {
        showNotification(paste("Error saving", file_name, ":", e$message), type = "error")
        error_count <- error_count + 1
      })
    }
    
    # Show summary notification
    if (success_count > 0) {
      showNotification(paste("Successfully uploaded", success_count, "files"), type = "message")
      load_files()
    }
    
    if (error_count > 0) {
      showNotification(paste(error_count, "files failed to upload"), type = "warning")
    }
  }
  
  # Read file content based on type
  read_file_content <- function(file_path, file_name) {
    ext <- tolower(tools::file_ext(file_name))
    
    content <- switch(ext,
      "txt" = readLines(file_path, encoding = "UTF-8", warn = FALSE),
      "csv" = {
        # For CSV files, read and convert to text representation
        df <- read.csv(file_path, stringsAsFactors = FALSE)
        paste(capture.output(print(df)), collapse = "\n")
      },
      "rtf" = {
        # Basic RTF reading (you might want to use a specialized package)
        readLines(file_path, encoding = "UTF-8", warn = FALSE)
      },
      "docx" = {
        # For DOCX files, you'd need the officer or docxtractr package
        # For now, just return a placeholder
        "DOCX file content extraction requires additional packages. Please convert to TXT format."
      },
      "pdf" = {
        # For PDF files, you'd need the pdftools package
        # For now, just return a placeholder
        "PDF file content extraction requires additional packages. Please convert to TXT format."
      },
      {
        # Default: try to read as text
        readLines(file_path, encoding = "UTF-8", warn = FALSE)
      }
    )
    
    # Convert to single string if it's a vector
    if (is.character(content) && length(content) > 1) {
      content <- paste(content, collapse = "\n")
    }
    
    return(content)
  }
  
  # Add new text file
  observeEvent(input$add_text_file, {
    if (is.null(current_project_id())) {
      showNotification("Please select a project first", type = "error")
      return()
    }
    
    showModal(
      modalDialog(
        title = "Create New Text File",
        size = "l",
        
        textInput(
          ns("new_file_name"),
          "File Name",
          placeholder = "Enter file name (e.g., Interview_01.txt)",
          value = ""
        ),
        
        textAreaInput(
          ns("new_file_content"),
          "File Content",
          placeholder = "Enter or paste your text content here...",
          height = "400px",
          value = ""
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_create_file"),
            "Create File",
            class = "btn-success",
            icon = icon("plus")
          )
        )
      )
    )
  })
  
  # Confirm create file
  observeEvent(input$confirm_create_file, {
    if (is.null(input$new_file_name) || input$new_file_name == "") {
      showNotification("Please enter a file name", type = "error")
      return()
    }
    
    if (is.null(input$new_file_content) || input$new_file_content == "") {
      showNotification("Please enter some content", type = "error")
      return()
    }
    
    # Get username for owner field
    user_info_query <- "SELECT username FROM users WHERE id = ?"
    user_info <- db_execute_query(db_pool, user_info_query, list(user_id()))
    owner_name <- if (!is.null(user_info)) user_info$username[1] else "Unknown"
    
    # Insert file into database
    insert_query <- "
      INSERT INTO source (name, file, project_id, owner, date, status) 
      VALUES (?, ?, ?, ?, NOW(), 1)
    "
    
    tryCatch({
      pool::dbExecute(db_pool, insert_query, 
                     list(input$new_file_name, input$new_file_content, 
                          current_project_id(), owner_name))
      
      showNotification("File created successfully", type = "message")
      removeModal()
      load_files()
      
    }, error = function(e) {
      showNotification(paste("Error creating file:", e$message), type = "error")
    })
  })
  
  # View selected file
  observeEvent(input$view_file, {
    if (!is.null(values$selected_file_row) && !is.null(values$files)) {
      selected_file <- values$files[values$selected_file_row, ]
      
      # Get full file content
      file_content <- get_file_content(db_pool, selected_file$id, user_id(), current_project_id())
      
      if (!is.null(file_content)) {
        values$current_file <- file_content
        
        # Show file viewer
        shinyjs::show("file_viewer_container")
        
        # Scroll to viewer
        shinyjs::runjs("document.getElementById('file_viewer_container').scrollIntoView();")
      } else {
        showNotification("Error loading file content", type = "error")
      }
    }
  })
  
  # Edit selected file
  observeEvent(input$edit_file, {
    if (!is.null(values$selected_file_row) && !is.null(values$files)) {
      selected_file <- values$files[values$selected_file_row, ]
      
      # Get full file content
      file_content <- get_file_content(db_pool, selected_file$id, user_id(), current_project_id())
      
      if (!is.null(file_content)) {
        showModal(
          modalDialog(
            title = paste("Edit File:", selected_file$name),
            size = "l",
            
            textInput(
              ns("edit_file_name"),
              "File Name",
              value = selected_file$name
            ),
            
            textAreaInput(
              ns("edit_file_content"),
              "File Content",
              value = file_content$file[1],
              height = "400px"
            ),
            
            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                ns("confirm_edit_file"),
                "Save Changes",
                class = "btn-primary",
                icon = icon("save")
              )
            )
          )
        )
      } else {
        showNotification("Error loading file for editing", type = "error")
      }
    }
  })
  
  # Confirm edit file
  observeEvent(input$confirm_edit_file, {
    if (!is.null(values$selected_file_row) && !is.null(values$files)) {
      selected_file <- values$files[values$selected_file_row, ]
      
      update_query <- "
        UPDATE source 
        SET name = ?, file = ?, dateM = NOW() 
        WHERE id = ? AND project_id = ?
      "
      
      tryCatch({
        pool::dbExecute(db_pool, update_query, 
                       list(input$edit_file_name, input$edit_file_content, 
                            selected_file$id, current_project_id()))
        
        showNotification("File updated successfully", type = "message")
        removeModal()
        load_files()
        
        # Refresh viewer if open
        if (!is.null(values$current_file)) {
          updated_content <- get_file_content(db_pool, selected_file$id, user_id(), current_project_id())
          values$current_file <- updated_content
        }
        
      }, error = function(e) {
        showNotification(paste("Error updating file:", e$message), type = "error")
      })
    }
  })
  
  # Delete selected file
  observeEvent(input$delete_file, {
    if (!is.null(values$selected_file_row) && !is.null(values$files)) {
      selected_file <- values$files[values$selected_file_row, ]
      
      showModal(
        modalDialog(
          title = "Delete File",
          size = "m",
          
          div(
            class = "alert alert-danger",
            icon("exclamation-triangle"),
            strong(" Warning: "),
            "This action cannot be undone!"
          ),
          
          p(paste("Are you sure you want to delete the file:", 
                  strong(selected_file$name), "?")),
          
          p("This will permanently delete:"),
          tags$ul(
            tags$li("The file and its content"),
            tags$li(paste(selected_file$coding_count, "associated codings")),
            tags$li("All annotations in this file")
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_delete_file"),
              "Delete File",
              class = "btn-danger"
            )
          )
        )
      )
    }
  })
  
  # Confirm delete file
  observeEvent(input$confirm_delete_file, {
    if (!is.null(values$selected_file_row) && !is.null(values$files)) {
      selected_file <- values$files[values$selected_file_row, ]
      
      tryCatch({
        # Soft delete (set status to 0)
        delete_query <- "UPDATE source SET status = 0, dateM = NOW() WHERE id = ? AND project_id = ?"
        pool::dbExecute(db_pool, delete_query, list(selected_file$id, current_project_id()))
        
        showNotification("File deleted successfully", type = "message")
        removeModal()
        load_files()
        
        # Close viewer if this file was open
        if (!is.null(values$current_file) && values$current_file$id[1] == selected_file$id) {
          shinyjs::hide("file_viewer_container")
          values$current_file <- NULL
        }
        
      }, error = function(e) {
        showNotification(paste("Error deleting file:", e$message), type = "error")
      })
    }
  })
  
  # Render file statistics
  output$total_files <- renderText({
    if (!is.null(values$file_stats)) {
      values$file_stats$total_files
    } else {
      "0"
    }
  })
  
  output$total_size <- renderText({
    if (!is.null(values$file_stats)) {
      size_mb <- round(values$file_stats$total_size / 1024, 1)
      paste0(size_mb, " KB")
    } else {
      "0 KB"
    }
  })
  
  output$coded_files <- renderText({
    if (!is.null(values$file_stats)) {
      values$file_stats$coded_files
    } else {
      "0"
    }
  })
  
  output$avg_codings <- renderText({
    if (!is.null(values$file_stats) && !is.na(values$file_stats$avg_codings)) {
      values$file_stats$avg_codings
    } else {
      "0"
    }
  })
  
  # Render categories
  output$categories_display <- renderUI({
    if (is.null(values$categories) || nrow(values$categories) == 0) {
      return(
        div(
          style = "text-align: center; padding: 20px; color: #666;",
          icon("folder-open"),
          p("No categories yet", style = "margin-top: 10px;"),
          tags$small("Create categories to organize your files")
        )
      )
    }
    
    category_items <- lapply(1:nrow(values$categories), function(i) {
      cat <- values$categories[i, ]
      
      div(
        style = "border: 1px solid #ddd; border-radius: 3px; padding: 8px; margin-bottom: 8px; background-color: #f9f9f9;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          strong(cat$name),
          tags$small(paste(cat$file_count, "files"), style = "color: #666;")
        ),
        if (!is.na(cat$memo) && cat$memo != "") {
          div(
            style = "margin-top: 5px; color: #666; font-size: 0.9em;",
            substr(cat$memo, 1, 50),
            if (nchar(cat$memo) > 50) "..." else ""
          )
        }
      )
    })
    
    do.call(tagList, category_items)
  })
  
  # Add category
  observeEvent(input$add_category, {
    if (is.null(current_project_id())) {
      showNotification("Please select a project first", type = "error")
      return()
    }
    
    showModal(
      modalDialog(
        title = "Add File Category",
        size = "m",
        
        textInput(
          ns("new_category_name"),
          "Category Name",
          placeholder = "e.g., Interviews, Documents, Survey Data"
        ),
        
        textAreaInput(
          ns("new_category_memo"),
          "Description (Optional)",
          placeholder = "Describe what files belong in this category...",
          height = "100px"
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_add_category"),
            "Add Category",
            class = "btn-success"
          )
        )
      )
    )
  })
  
  # Confirm add category
  observeEvent(input$confirm_add_category, {
    if (is.null(input$new_category_name) || input$new_category_name == "") {
      showNotification("Please enter a category name", type = "error")
      return()
    }
    
    # Get username for owner field
    user_info_query <- "SELECT username FROM users WHERE id = ?"
    user_info <- db_execute_query(db_pool, user_info_query, list(user_id()))
    owner_name <- if (!is.null(user_info)) user_info$username[1] else "Unknown"
    
    insert_query <- "
      INSERT INTO filecat (name, memo, project_id, owner, date, status) 
      VALUES (?, ?, ?, ?, NOW(), 1)
    "
    
    tryCatch({
      pool::dbExecute(db_pool, insert_query, 
                     list(input$new_category_name, input$new_category_memo, 
                          current_project_id(), owner_name))
      
      showNotification("Category added successfully", type = "message")
      removeModal()
      load_categories()
      
    }, error = function(e) {
      showNotification(paste("Error adding category:", e$message), type = "error")
    })
  })
  
  # File viewer outputs
  output$viewer_file_name <- renderText({
    if (!is.null(values$current_file)) {
      values$current_file$name[1]
    }
  })
  
  output$file_content_display <- renderUI({
    if (!is.null(values$current_file)) {
      content <- values$current_file$file[1]
      
      # Format content for display
      formatted_content <- gsub("\n", "<br>", content)
      formatted_content <- gsub("  ", "&nbsp;&nbsp;", formatted_content)
      
      HTML(formatted_content)
    }
  })
  
  output$file_size_display <- renderText({
    if (!is.null(values$current_file)) {
      size_kb <- round(nchar(values$current_file$file[1]) / 1024, 1)
      paste("Size:", size_kb, "KB")
    }
  })
  
  output$file_date_display <- renderText({
    if (!is.null(values$current_file)) {
      paste("Added:", format(as.POSIXct(values$current_file$date[1]), "%Y-%m-%d"))
    }
  })
  
  output$file_owner_display <- renderText({
    if (!is.null(values$current_file)) {
      paste("Owner:", values$current_file$owner[1])
    }
  })
  
  output$file_codings_display <- renderText({
    if (!is.null(values$current_file)) {
      # Get coding count for this file
      coding_query <- "SELECT COUNT(*) as count FROM coding WHERE fid = ? AND status = 1"
      coding_count <- db_execute_query(db_pool, coding_query, list(values$current_file$id[1]))
      count <- if (!is.null(coding_count)) coding_count$count[1] else 0
      paste("Codings:", count)
    }
  })
  
  # Close file viewer
  observeEvent(input$close_viewer, {
    shinyjs::hide("file_viewer_container")
    values$current_file <- NULL
  })
  
  # Edit in viewer
  observeEvent(input$edit_in_viewer, {
    if (!is.null(values$current_file)) {
      showModal(
        modalDialog(
          title = paste("Edit File:", values$current_file$name[1]),
          size = "l",
          
          textInput(
            ns("viewer_edit_file_name"),
            "File Name",
            value = values$current_file$name[1]
          ),
          
          textAreaInput(
            ns("viewer_edit_file_content"),
            "File Content",
            value = values$current_file$file[1],
            height = "400px"
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_viewer_edit_file"),
              "Save Changes",
              class = "btn-primary",
              icon = icon("save")
            )
          )
        )
      )
    }
  })
  
  # Confirm viewer edit
  observeEvent(input$confirm_viewer_edit_file, {
    if (!is.null(values$current_file)) {
      update_query <- "
        UPDATE source 
        SET name = ?, file = ?, dateM = NOW() 
        WHERE id = ? AND project_id = ?
      "
      
      tryCatch({
        pool::dbExecute(db_pool, update_query, 
                       list(input$viewer_edit_file_name, input$viewer_edit_file_content, 
                            values$current_file$id[1], current_project_id()))
        
        showNotification("File updated successfully", type = "message")
        removeModal()
        load_files()
        
        # Refresh viewer content
        updated_content <- get_file_content(db_pool, values$current_file$id[1], user_id(), current_project_id())
        values$current_file <- updated_content
        
      }, error = function(e) {
        showNotification(paste("Error updating file:", e$message), type = "error")
      })
    }
  })
  
  # Bulk categorize functionality
  observeEvent(input$bulk_categorize, {
    if (is.null(values$files) || nrow(values$files) == 0) {
      showNotification("No files to categorize", type = "warning")
      return()
    }
    
    if (is.null(values$categories) || nrow(values$categories) == 0) {
      showNotification("Please create categories first", type = "warning")
      return()
    }
    
    showNotification("Bulk categorization functionality coming soon!", type = "message")
  })
  
  # Export files functionality
  observeEvent(input$export_files, {
    if (is.null(values$files) || nrow(values$files) == 0) {
      showNotification("No files to export", type = "warning")
      return()
    }
    
    showNotification("File export functionality coming soon!", type = "message")
  })
  
  # File search functionality
  observeEvent(input$file_search, {
    if (is.null(values$files) || nrow(values$files) == 0) {
      showNotification("No files to search", type = "warning")
      return()
    }
    
    showModal(
      modalDialog(
        title = "Search in Files",
        size = "m",
        
        textInput(
          ns("search_term"),
          "Search Term",
          placeholder = "Enter text to search for..."
        ),
        
        checkboxInput(
          ns("search_case_sensitive"),
          "Case sensitive",
          value = FALSE
        ),
        
        checkboxInput(
          ns("search_whole_words"),
          "Whole words only",
          value = FALSE
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("perform_search"),
            "Search",
            class = "btn-primary",
            icon = icon("search")
          )
        )
      )
    )
  })
  
  # Perform search
  observeEvent(input$perform_search, {
    if (is.null(input$search_term) || input$search_term == "") {
      showNotification("Please enter a search term", type = "error")
      return()
    }
    
    search_term <- input$search_term
    case_sensitive <- input$search_case_sensitive
    whole_words <- input$search_whole_words
    
    # Build search query
    search_query <- "
      SELECT s.id, s.name, COUNT(*) as matches
      FROM source s
      WHERE s.project_id = ? AND s.status = 1 AND 
    "
    
    if (case_sensitive) {
      if (whole_words) {
        search_query <- paste0(search_query, "s.file REGEXP '\\\\b", search_term, "\\\\b'")
      } else {
        search_query <- paste0(search_query, "s.file LIKE '%", search_term, "%'")
      }
    } else {
      if (whole_words) {
        search_query <- paste0(search_query, "LOWER(s.file) REGEXP '\\\\b", tolower(search_term), "\\\\b'")
      } else {
        search_query <- paste0(search_query, "LOWER(s.file) LIKE '%", tolower(search_term), "%'")
      }
    }
    
    search_query <- paste0(search_query, " GROUP BY s.id, s.name ORDER BY matches DESC")
    
    search_results <- db_execute_query(db_pool, search_query, list(current_project_id()))
    
    if (is.null(search_results) || nrow(search_results) == 0) {
      showNotification(paste("No files contain the term:", search_term), type = "info")
    } else {
      result_text <- paste(
        "Found", nrow(search_results), "files containing '", search_term, "':",
        paste(search_results$name, collapse = ", ")
      )
      showNotification(result_text, type = "message", duration = 10)
    }
    
    removeModal()
  })
  
  # Return reactive values for use by other modules
  return(list(
    files = reactive({ values$files }),
    selected_file = reactive({ 
      if (!is.null(values$selected_file_row) && !is.null(values$files)) {
        values$files[values$selected_file_row, ]
      } else {
        NULL
      }
    }),
    current_file = reactive({ values$current_file }),
    categories = reactive({ values$categories }),
    file_stats = reactive({ values$file_stats })
  ))
}

# File Viewer Server (for standalone file viewer)
fileViewerServer <- function(input, output, session, db_pool, user_id, current_project_id, file_data) {
  ns <- session$ns
  
  # This server function can be used for a dedicated file viewer component
  # Implementation would be similar to the file viewer parts above
  
  # Return reactive values
  return(list(
    # Viewer state and functions
  ))
}

# Helper functions for file operations

# Format file size for display
format_file_size <- function(size_bytes) {
  if (size_bytes < 1024) {
    paste(size_bytes, "B")
  } else if (size_bytes < 1024^2) {
    paste(round(size_bytes / 1024, 1), "KB")
  } else if (size_bytes < 1024^3) {
    paste(round(size_bytes / 1024^2, 1), "MB")
  } else {
    paste(round(size_bytes / 1024^3, 1), "GB")
  }
}

# Extract text from various file formats
extract_text_content <- function(file_path, file_type) {
  tryCatch({
    switch(tolower(file_type),
      "txt" = paste(readLines(file_path, encoding = "UTF-8", warn = FALSE), collapse = "\n"),
      "csv" = {
        df <- read.csv(file_path, stringsAsFactors = FALSE)
        paste(capture.output(print(df)), collapse = "\n")
      },
      "rtf" = paste(readLines(file_path, encoding = "UTF-8", warn = FALSE), collapse = "\n"),
      # Default fallback
      paste(readLines(file_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    )
  }, error = function(e) {
    paste("Error reading file:", e$message)
  })
}

# Validate file for upload
validate_file_upload <- function(file_info, max_size_mb = 10) {
  errors <- c()
  
  # Check file size
  if (file_info$size > max_size_mb * 1024 * 1024) {
    errors <- c(errors, paste("File too large (max", max_size_mb, "MB)"))
  }
  
  # Check file extension
  allowed_extensions <- c("txt", "csv", "rtf", "docx", "pdf")
  file_ext <- tolower(tools::file_ext(file_info$name))
  
  if (!file_ext %in% allowed_extensions) {
    errors <- c(errors, paste("Unsupported file type:", file_ext))
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors
  ))
}

# Search text content
search_text_content <- function(text, search_term, case_sensitive = FALSE, whole_words = FALSE) {
  if (!case_sensitive) {
    text <- tolower(text)
    search_term <- tolower(search_term)
  }
  
  if (whole_words) {
    pattern <- paste0("\\b", search_term, "\\b")
    matches <- gregexpr(pattern, text, perl = TRUE)
  } else {
    matches <- gregexpr(search_term, text, fixed = TRUE)
  }
  
  if (matches[[1]][1] != -1) {
    return(length(matches[[1]]))
  } else {
    return(0)
  }
}
