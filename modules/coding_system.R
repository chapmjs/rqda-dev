# modules/coding_system.R
# Coding system functionality

library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(stringr)
library(colourpicker)

# Source database functions if not already loaded
if (!exists("db_execute_query")) {
  source("modules/database.R")
}

# Coding System UI
codingSystemUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Codes management panel
    box(
      title = "Codes & Categories",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      
      # Code management toolbar
      div(
        style = "margin-bottom: 15px;",
        actionButton(
          ns("add_code"),
          "New Code",
          icon = icon("plus"),
          class = "btn-success btn-sm",
          style = "width: 48%;"
        ),
        actionButton(
          ns("add_category"),
          "New Category",
          icon = icon("folder-plus"),
          class = "btn-info btn-sm",
          style = "width: 48%; margin-left: 4%;"
        )
      ),
      
      # Codes tree display
      div(
        style = "max-height: 500px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #fafafa;",
        withSpinner(uiOutput(ns("codes_tree")))
      ),
      
      # Selected code actions
      conditionalPanel(
        condition = paste0("output['", ns("has_selected_code"), "']"),
        hr(),
        div(
          style = "text-align: center;",
          h5("Selected Code Actions"),
          actionButton(
            ns("edit_code"),
            "Edit",
            icon = icon("edit"),
            class = "btn-warning btn-sm",
            style = "margin: 2px;"
          ),
          actionButton(
            ns("code_memo"),
            "Memo",
            icon = icon("sticky-note"),
            class = "btn-info btn-sm",
            style = "margin: 2px;"
          ),
          actionButton(
            ns("auto_code"),
            "Auto Code",
            icon = icon("magic"),
            class = "btn-primary btn-sm",
            style = "margin: 2px;"
          ),
          actionButton(
            ns("delete_code"),
            "Delete",
            icon = icon("trash"),
            class = "btn-danger btn-sm",
            style = "margin: 2px;"
          )
        )
      )
    ),
    
    # Text coding interface
    box(
      title = "Text Coding Interface",
      status = "success",
      solidHeader = TRUE,
      width = 8,
      
      # File selection and coding toolbar
      fluidRow(
        column(6,
          selectInput(
            ns("selected_file"),
            "Select File to Code:",
            choices = NULL,
            width = "100%"
          )
        ),
        column(6,
          div(
            style = "margin-top: 25px;",
            actionButton(
              ns("save_coding"),
              "Save Coding",
              icon = icon("save"),
              class = "btn-success btn-sm"
            ),
            actionButton(
              ns("clear_selection"),
              "Clear Selection",
              icon = icon("eraser"),
              class = "btn-warning btn-sm",
              style = "margin-left: 5px;"
            ),
            actionButton(
              ns("toggle_codings"),
              "Show/Hide Codings",
              icon = icon("eye"),
              class = "btn-info btn-sm",
              style = "margin-left: 5px;"
            )
          )
        )
      ),
      
      # Coding status and selected text info
      conditionalPanel(
        condition = paste0("output['", ns("has_text_selection"), "']"),
        div(
          class = "alert alert-info",
          style = "margin: 10px 0;",
          icon("info-circle"),
          strong(" Text Selected: "),
          textOutput(ns("selection_info"), inline = TRUE)
        )
      ),
      
      # Text display area with coding interface
      div(
        style = "height: 500px; overflow-y: auto; border: 2px solid #ddd; padding: 15px; background-color: #fff; font-family: 'Georgia', serif; line-height: 1.8; font-size: 16px;",
        id = ns("text_coding_area"),
        withSpinner(
          htmlOutput(ns("text_content_display"))
        )
      ),
      
      # Coding legend
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        h6("Coding Legend:", style = "margin-bottom: 10px;"),
        div(
          id = ns("coding_legend"),
          style = "max-height: 100px; overflow-y: auto;",
          uiOutput(ns("coding_legend_display"))
        )
      )
    )
  )
}

# Text Coding Interface UI (for dedicated coding tab)
textCodingUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Quick codes panel
    column(3,
      box(
        title = "Quick Codes",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        height = "600px",
        
        div(
          style = "max-height: 500px; overflow-y: auto;",
          uiOutput(ns("quick_codes_list"))
        ),
        
        div(
          style = "position: absolute; bottom: 15px; left: 15px; right: 15px;",
          actionButton(
            ns("manage_codes"),
            "Manage Codes",
            icon = icon("cog"),
            class = "btn-info btn-sm",
            style = "width: 100%;"
          )
        )
      )
    ),
    
    # Main coding interface
    column(9,
      box(
        title = "Document Coding",
        status = "success", 
        solidHeader = TRUE,
        width = NULL,
        
        # Document selection and tools
        fluidRow(
          column(4,
            selectInput(
              ns("coding_file_select"),
              "Document:",
              choices = NULL,
              width = "100%"
            )
          ),
          column(4,
            selectInput(
              ns("active_code_select"),
              "Active Code:",
              choices = NULL,
              width = "100%"
            )
          ),
          column(4,
            div(
              style = "margin-top: 25px;",
              actionButton(
                ns("coding_help"),
                "Help",
                icon = icon("question-circle"),
                class = "btn-info btn-sm"
              ),
              actionButton(
                ns("coding_settings"),
                "Settings",
                icon = icon("cog"),
                class = "btn-default btn-sm",
                style = "margin-left: 5px;"
              )
            )
          )
        ),
        
        # Coding instructions
        div(
          class = "alert alert-success",
          style = "margin: 15px 0;",
          icon("lightbulb"),
          strong(" How to Code: "),
          "Select text with your mouse, then click a code or press the number key. Use Shift+Click to extend selection."
        ),
        
        # Text display with coding capabilities
        div(
          style = "height: 600px; overflow-y: auto; border: 2px solid #28a745; padding: 20px; background-color: #fff; font-family: 'Georgia', serif; line-height: 2.0; font-size: 17px; user-select: text;",
          id = ns("main_text_area"),
          htmlOutput(ns("main_text_display"))
        ),
        
        # Coding summary
        div(
          style = "margin-top: 15px; padding: 10px; background-color: #e9ecef; border-radius: 5px;",
          fluidRow(
            column(3,
              strong("Total Codings: "),
              textOutput(ns("total_codings"), inline = TRUE)
            ),
            column(3,
              strong("Active Code: "),
              textOutput(ns("active_code_name"), inline = TRUE)
            ),
            column(3,
              strong("Last Action: "),
              textOutput(ns("last_action"), inline = TRUE)
            ),
            column(3,
              actionButton(
                ns("view_all_codings"),
                "View All Codings",
                icon = icon("list"),
                class = "btn-outline-primary btn-sm",
                style = "width: 100%;"
              )
            )
          )
        )
      )
    )
  )
}

# Coding System Server
codingSystemServer <- function(input, output, session, db_pool, user_id, current_project_id) {
  ns <- session$ns
  
  # Reactive values
  values <- reactiveValues(
    codes = NULL,
    categories = NULL,
    files = NULL,
    selected_code_id = NULL,
    selected_file_id = NULL,
    current_file_content = NULL,
    text_selection = NULL,
    codings = NULL,
    show_codings = TRUE
  )
  
  # Load codes and categories
  load_codes <- function() {
    if (!is.null(current_project_id()) && !is.null(user_id())) {
      codes <- get_project_codes(db_pool, current_project_id(), user_id())
      values$codes <- codes
      
      # Load categories
      categories_query <- "
        SELECT catid, name, memo 
        FROM codecat 
        WHERE project_id = ? AND status = 1 
        ORDER BY name
      "
      categories <- db_execute_query(db_pool, categories_query, list(current_project_id()))
      values$categories <- categories
    }
  }
  
  # Load project files for coding
  load_files_for_coding <- function() {
    if (!is.null(current_project_id()) && !is.null(user_id())) {
      files <- get_project_files(db_pool, current_project_id(), user_id())
      values$files <- files
      
      # Update file choices
      if (!is.null(files) && nrow(files) > 0) {
        file_choices <- setNames(files$id, files$name)
        updateSelectInput(session, "selected_file", choices = file_choices)
        updateSelectInput(session, "coding_file_select", choices = file_choices)
      }
    }
  }
  
  # Load codings for current file
  load_file_codings <- function() {
    if (!is.null(values$selected_file_id) && !is.null(current_project_id())) {
      codings <- get_file_codings(db_pool, values$selected_file_id, current_project_id(), user_id())
      values$codings <- codings
    }
  }
  
  # Load data when project changes
  observe({
    load_codes()
    load_files_for_coding()
  })
  
  # Render codes tree
  output$codes_tree <- renderUI({
    if (is.null(values$codes) && is.null(values$categories)) {
      return(
        div(
          style = "text-align: center; padding: 30px; color: #666;",
          icon("code", "fa-2x"),
          h5("No Codes Yet", style = "margin-top: 15px;"),
          p("Create your first code to start analyzing"),
          actionButton(
            ns("create_first_code"),
            "Create First Code",
            icon = icon("plus"),
            class = "btn-success btn-sm"
          )
        )
      )
    }
    
    # Build hierarchical display
    tree_items <- list()
    
    # Uncategorized codes first
    if (!is.null(values$codes)) {
      uncategorized_codes <- values$codes[is.na(values$codes$category_name) | values$codes$category_name == "", ]
      
      if (nrow(uncategorized_codes) > 0) {
        tree_items <- append(tree_items, list(
          div(
            style = "margin-bottom: 15px;",
            h6("Uncategorized Codes", style = "color: #666; margin-bottom: 10px;"),
            lapply(1:nrow(uncategorized_codes), function(i) {
              code <- uncategorized_codes[i, ]
              create_code_item(code, ns)
            })
          )
        ))
      }
    }
    
    # Categorized codes
    if (!is.null(values$categories) && nrow(values$categories) > 0) {
      for (i in 1:nrow(values$categories)) {
        category <- values$categories[i, ]
        category_codes <- values$codes[!is.na(values$codes$category_name) & values$codes$category_name == category$name, ]
        
        tree_items <- append(tree_items, list(
          div(
            style = "margin-bottom: 15px; border-left: 3px solid #007bff; padding-left: 10px;",
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
              h6(category$name, style = "color: #007bff; margin: 0;"),
              actionButton(
                ns(paste0("edit_category_", category$catid)),
                "",
                icon = icon("edit"),
                class = "btn-link btn-xs",
                style = "padding: 2px 6px;"
              )
            ),
            if (!is.null(category_codes) && nrow(category_codes) > 0) {
              lapply(1:nrow(category_codes), function(j) {
                code <- category_codes[j, ]
                create_code_item(code, ns)
              })
            } else {
              tags$small("No codes in this category", style = "color: #999; font-style: italic;")
            }
          )
        ))
      }
    }
    
    do.call(tagList, tree_items)
  })
  
  # Create code item display
  create_code_item <- function(code, ns) {
    div(
      class = "code-item",
      style = paste0("margin: 5px 0; padding: 8px; border-radius: 4px; background-color: ", 
                     ifelse(is.na(code$color) || code$color == "", "#f8f9fa", code$color), 
                     "; border: 1px solid #dee2e6; cursor: pointer; transition: all 0.2s;"),
      onclick = paste0("Shiny.setInputValue('", ns("selected_code_id"), "', ", code$id, ");"),
      
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(
          strong(code$name),
          if (!is.na(code$usage_count) && code$usage_count > 0) {
            tags$small(
              paste("(", code$usage_count, ")"),
              style = "margin-left: 5px; color: #666;"
            )
          }
        ),
        div(
          style = "opacity: 0.7;",
          if (!is.na(code$memo) && code$memo != "") {
            icon("sticky-note", style = "margin-right: 3px;")
          },
          tags$small(format(as.POSIXct(code$date), "%m/%d"), style = "font-size: 0.8em;")
        )
      ),
      
      if (!is.na(code$memo) && code$memo != "" && nchar(code$memo) > 0) {
        div(
          style = "margin-top: 5px; font-size: 0.9em; color: #666; font-style: italic;",
          substr(code$memo, 1, 50),
          if (nchar(code$memo) > 50) "..." else ""
        )
      }
    )
  }
  
  # Track selected code
  observe({
    if (!is.null(input$selected_code_id)) {
      values$selected_code_id <- input$selected_code_id
    }
  })
  
  # Check if code is selected
  output$has_selected_code <- reactive({
    !is.null(values$selected_code_id)
  })
  outputOptions(output, "has_selected_code", suspendWhenHidden = FALSE)
  
  # Add new code
  observeEvent(input$add_code, {
    show_add_code_modal()
  })
  
  observeEvent(input$create_first_code, {
    show_add_code_modal()
  })
  
  # Show add code modal
  show_add_code_modal <- function() {
    if (is.null(current_project_id())) {
      showNotification("Please select a project first", type = "error")
      return()
    }
    
    # Get category choices
    category_choices <- list("None (Uncategorized)" = "")
    if (!is.null(values$categories) && nrow(values$categories) > 0) {
      cat_choices <- setNames(values$categories$catid, values$categories$name)
      category_choices <- c(category_choices, cat_choices)
    }
    
    showModal(
      modalDialog(
        title = "Create New Code",
        size = "m",
        
        textInput(
          ns("new_code_name"),
          "Code Name *",
          placeholder = "Enter a descriptive code name",
          value = ""
        ),
        
        textAreaInput(
          ns("new_code_memo"),
          "Code Description",
          placeholder = "Describe what this code represents...",
          height = "100px",
          value = ""
        ),
        
        fluidRow(
          column(6,
            colourInput(
              ns("new_code_color"),
              "Code Color",
              value = "#FFFF99",
              palette = "limited",
              allowedCols = c("#FFFF99", "#FFE4B5", "#E6F3FF", "#F0FFF0", 
                             "#FFE4E1", "#F5F5DC", "#E0E6FF", "#FFF0F5")
            )
          ),
          column(6,
            selectInput(
              ns("new_code_category"),
              "Category",
              choices = category_choices,
              selected = ""
            )
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_add_code"),
            "Create Code",
            class = "btn-success",
            icon = icon("plus")
          )
        )
      )
    )
  }
  
  # Confirm add code
  observeEvent(input$confirm_add_code, {
    if (is.null(input$new_code_name) || input$new_code_name == "") {
      showNotification("Please enter a code name", type = "error")
      return()
    }
    
    # Get username for owner field
    user_info_query <- "SELECT username FROM users WHERE id = ?"
    user_info <- db_execute_query(db_pool, user_info_query, list(user_id()))
    owner_name <- if (!is.null(user_info)) user_info$username[1] else "Unknown"
    
    result <- add_code(
      db_pool, 
      input$new_code_name, 
      current_project_id(), 
      user_id(), 
      owner_name,
      input$new_code_color,
      input$new_code_memo
    )
    
    if (result$success) {
      # Add to category if selected
      if (!is.null(input$new_code_category) && input$new_code_category != "") {
        category_query <- "
          INSERT INTO treecode (cid, catid, project_id, owner, date, status) 
          VALUES (?, ?, ?, ?, NOW(), 1)
        "
        tryCatch({
          pool::dbExecute(db_pool, category_query, 
                         list(result$code_id, input$new_code_category, 
                              current_project_id(), owner_name))
        }, error = function(e) {
          # Silently fail category assignment
        })
      }
      
      showNotification("Code created successfully!", type = "message")
      removeModal()
      load_codes()
      
      # Auto-select the new code
      values$selected_code_id <- result$code_id
      
    } else {
      showNotification(result$message, type = "error")
    }
  })
  
  # Add new category
  observeEvent(input$add_category, {
    if (is.null(current_project_id())) {
      showNotification("Please select a project first", type = "error")
      return()
    }
    
    showModal(
      modalDialog(
        title = "Create Code Category",
        size = "m",
        
        textInput(
          ns("new_category_name"),
          "Category Name *",
          placeholder = "e.g., Themes, Concepts, Emotions",
          value = ""
        ),
        
        textAreaInput(
          ns("new_category_memo"),
          "Category Description",
          placeholder = "Describe what codes belong in this category...",
          height = "100px",
          value = ""
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_add_category"),
            "Create Category",
            class = "btn-info",
            icon = icon("folder-plus")
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
      INSERT INTO codecat (name, memo, project_id, owner, date, status) 
      VALUES (?, ?, ?, ?, NOW(), 1)
    "
    
    tryCatch({
      pool::dbExecute(db_pool, insert_query, 
                     list(input$new_category_name, input$new_category_memo, 
                          current_project_id(), owner_name))
      
      showNotification("Category created successfully!", type = "message")
      removeModal()
      load_codes()
      
    }, error = function(e) {
      showNotification(paste("Error creating category:", e$message), type = "error")
    })
  })
  
  # Edit selected code
  observeEvent(input$edit_code, {
    if (is.null(values$selected_code_id)) {
      showNotification("Please select a code first", type = "warning")
      return()
    }
    
    # Get current code details
    selected_code <- values$codes[values$codes$id == values$selected_code_id, ][1, ]
    
    if (is.null(selected_code) || nrow(selected_code) == 0) {
      showNotification("Selected code not found", type = "error")
      return()
    }
    
    # Get category choices
    category_choices <- list("None (Uncategorized)" = "")
    if (!is.null(values$categories) && nrow(values$categories) > 0) {
      cat_choices <- setNames(values$categories$catid, values$categories$name)
      category_choices <- c(category_choices, cat_choices)
    }
    
    # Find current category
    current_category <- ""
    if (!is.na(selected_code$category_name) && selected_code$category_name != "") {
      category_match <- values$categories[values$categories$name == selected_code$category_name, ]
      if (nrow(category_match) > 0) {
        current_category <- category_match$catid[1]
      }
    }
    
    showModal(
      modalDialog(
        title = paste("Edit Code:", selected_code$name),
        size = "m",
        
        textInput(
          ns("edit_code_name"),
          "Code Name *",
          value = selected_code$name
        ),
        
        textAreaInput(
          ns("edit_code_memo"),
          "Code Description",
          value = ifelse(is.na(selected_code$memo), "", selected_code$memo),
          height = "100px"
        ),
        
        fluidRow(
          column(6,
            colourInput(
              ns("edit_code_color"),
              "Code Color",
              value = ifelse(is.na(selected_code$color), "#FFFF99", selected_code$color),
              palette = "limited"
            )
          ),
          column(6,
            selectInput(
              ns("edit_code_category"),
              "Category",
              choices = category_choices,
              selected = current_category
            )
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_edit_code"),
            "Save Changes",
            class = "btn-warning",
            icon = icon("save")
          )
        )
      )
    )
  })
  
  # Confirm edit code
  observeEvent(input$confirm_edit_code, {
    if (is.null(input$edit_code_name) || input$edit_code_name == "") {
      showNotification("Please enter a code name", type = "error")
      return()
    }
    
    # Update code
    update_query <- "
      UPDATE freecode 
      SET name = ?, memo = ?, color = ?, dateM = NOW() 
      WHERE id = ? AND project_id = ?
    "
    
    tryCatch({
      pool::dbExecute(db_pool, update_query, 
                     list(input$edit_code_name, input$edit_code_memo, 
                          input$edit_code_color, values$selected_code_id, current_project_id()))
      
      # Update category assignment
      # First remove existing category assignment
      pool::dbExecute(db_pool, "DELETE FROM treecode WHERE cid = ? AND project_id = ?", 
                     list(values$selected_code_id, current_project_id()))
      
      # Add new category assignment if selected
      if (!is.null(input$edit_code_category) && input$edit_code_category != "") {
        user_info_query <- "SELECT username FROM users WHERE id = ?"
        user_info <- db_execute_query(db_pool, user_info_query, list(user_id()))
        owner_name <- if (!is.null(user_info)) user_info$username[1] else "Unknown"
        
        category_query <- "
          INSERT INTO treecode (cid, catid, project_id, owner, date, status) 
          VALUES (?, ?, ?, ?, NOW(), 1)
        "
        pool::dbExecute(db_pool, category_query, 
                       list(values$selected_code_id, input$edit_code_category, 
                            current_project_id(), owner_name))
      }
      
      showNotification("Code updated successfully!", type = "message")
      removeModal()
      load_codes()
      
    }, error = function(e) {
      showNotification(paste("Error updating code:", e$message), type = "error")
    })
  })
  
  # Delete selected code
  observeEvent(input$delete_code, {
    if (is.null(values$selected_code_id)) {
      showNotification("Please select a code first", type = "warning")
      return()
    }
    
    selected_code <- values$codes[values$codes$id == values$selected_code_id, ][1, ]
    
    showModal(
      modalDialog(
        title = "Delete Code",
        size = "m",
        
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          strong(" Warning: "),
          "This action cannot be undone!"
        ),
        
        p(paste("Are you sure you want to delete the code:", 
                strong(selected_code$name), "?")),
        
        p("This will permanently delete:"),
        tags$ul(
          tags$li("The code definition"),
          tags$li(paste(selected_code$usage_count, "associated codings")),
          tags$li("All memos attached to this code")
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_delete_code"),
            "Delete Code",
            class = "btn-danger"
          )
        )
      )
    )
  })
  
  # Confirm delete code
  observeEvent(input$confirm_delete_code, {
    tryCatch({
      # Soft delete (set status to 0)
      delete_query <- "UPDATE freecode SET status = 0, dateM = NOW() WHERE id = ? AND project_id = ?"
      pool::dbExecute(db_pool, delete_query, list(values$selected_code_id, current_project_id()))
      
      showNotification("Code deleted successfully", type = "message")
      removeModal()
      load_codes()
      values$selected_code_id <- NULL
      
    }, error = function(e) {
      showNotification(paste("Error deleting code:", e$message), type = "error")
    })
  })
  
  # Handle file selection for coding
  observeEvent(input$selected_file, {
    if (!is.null(input$selected_file) && input$selected_file != "") {
      values$selected_file_id <- as.integer(input$selected_file)
      
      # Load file content
      file_content <- get_file_content(db_pool, values$selected_file_id, user_id(), current_project_id())
      values$current_file_content <- file_content
      
      # Load codings for this file
      load_file_codings()
    }
  })
  
  # Render text content with coding highlights
  output$text_content_display <- renderUI({
    if (is.null(values$current_file_content)) {
      return(
        div(
          style = "text-align: center; padding: 50px; color: #666;",
          icon("file-text", "fa-3x"),
          h4("No File Selected", style = "margin-top: 15px;"),
          p("Select a file from the dropdown to start coding")
        )
      )
    }
    
    text_content <- values$current_file_content$file[1]
    
    # Apply coding highlights if enabled
    if (values$show_codings && !is.null(values$codings) && nrow(values$codings) > 0) {
      text_content <- apply_coding_highlights(text_content, values$codings)
    }
    
    # Make text selectable and add JavaScript for selection handling
    div(
      id = ns("selectable_text"),
      style = "user-select: text; -webkit-user-select: text; -moz-user-select: text; -ms-user-select: text;",
      HTML(text_content),
      tags$script(HTML(paste0("
        document.getElementById('", ns("selectable_text"), "').addEventListener('mouseup', function() {
          var selection = window.getSelection();
          if (selection.toString().length > 0) {
            var range = selection.getRangeAt(0);
            var startOffset = range.startOffset;
            var endOffset = range.endOffset;
            var selectedText = selection.toString();
            
            // Calculate absolute positions in the document
            var textNode = range.startContainer;
            var absoluteStart = getAbsoluteOffset(textNode, startOffset);
            var absoluteEnd = absoluteStart + selectedText.length;
            
            Shiny.setInputValue('", ns("text_selection"), "', {
              text: selectedText,
              start: absoluteStart,
              end: absoluteEnd,
              timestamp: Date.now()
            });
          }
        });
        
        function getAbsoluteOffset(node, offset) {
          var textContent = document.getElementById('", ns("selectable_text"), "').textContent;
          var nodeText = node.textContent;
          var nodeStart = textContent.indexOf(nodeText);
          return nodeStart + offset;
        }
      ")))
    )
  })
  
  # Apply coding highlights to text
  apply_coding_highlights <- function(text, codings) {
    if (nrow(codings) == 0) return(text)
    
    # Sort codings by start position (descending to apply from end to beginning)
    codings <- codings[order(codings$selfirst, decreasing = TRUE), ]
    
    for (i in 1:nrow(codings)) {
      coding <- codings[i, ]
      start_pos <- coding$selfirst
      end_pos <- coding$selend
      color <- ifelse(is.na(coding$color), "#FFFF99", coding$color)
      
      # Extract the coded text
      before_text <- substr(text, 1, start_pos - 1)
      coded_text <- substr(text, start_pos, end_pos)
      after_text <- substr(text, end_pos + 1, nchar(text))
      
      # Create highlighted span
      highlighted_span <- paste0(
        '<span class="coded-segment" style="background-color: ', color, 
        '; border-left: 3px solid ', darken_color(color), 
        '; padding: 2px 4px; margin: 1px; border-radius: 3px;" ',
        'title="Code: ', coding$code_name, 
        ifelse(!is.na(coding$coding_memo) && coding$coding_memo != "", 
               paste0('\nMemo: ', coding$coding_memo), ''), 
        '">', coded_text, '</span>'
      )
      
      # Reconstruct text with highlight
      text <- paste0(before_text, highlighted_span, after_text)
    }
    
    return(text)
  }
  
  # Helper function to darken colors for borders
  darken_color <- function(color) {
    if (color == "#FFFF99") return("#FFD700")
    if (color == "#FFE4B5") return("#DEB887")
    if (color == "#E6F3FF") return("#87CEEB")
    if (color == "#F0FFF0") return("#90EE90")
    if (color == "#FFE4E1") return("#FFC0CB")
    return("#999999")  # Default darker color
  }
  
  # Handle text selection
  observe({
    if (!is.null(input$text_selection)) {
      values$text_selection <- input$text_selection
    }
  })
  
  # Check if text is selected
  output$has_text_selection <- reactive({
    !is.null(values$text_selection) && 
    !is.null(values$text_selection$text) && 
    nchar(values$text_selection$text) > 0
  })
  outputOptions(output, "has_text_selection", suspendWhenHidden = FALSE)
  
  # Display selection info
  output$selection_info <- renderText({
    if (!is.null(values$text_selection)) {
      selected_text <- values$text_selection$text
      if (nchar(selected_text) > 50) {
        paste0('"', substr(selected_text, 1, 50), '..." (', nchar(selected_text), ' characters)')
      } else {
        paste0('"', selected_text, '" (', nchar(selected_text), ' characters)')
      }
    }
  })
  
  # Save coding
  observeEvent(input$save_coding, {
    if (is.null(values$text_selection) || is.null(values$selected_code_id)) {
      showNotification("Please select text and a code first", type = "warning")
      return()
    }
    
    if (is.null(values$selected_file_id)) {
      showNotification("Please select a file first", type = "error")
      return()
    }
    
    # Get username for owner field
    user_info_query <- "SELECT username FROM users WHERE id = ?"
    user_info <- db_execute_query(db_pool, user_info_query, list(user_id()))
    owner_name <- if (!is.null(user_info)) user_info$username[1] else "Unknown"
    
    result <- add_coding(
      db_pool,
      values$selected_code_id,
      values$selected_file_id,
      values$text_selection$text,
      values$text_selection$start,
      values$text_selection$end,
      current_project_id(),
      user_id(),
      owner_name
    )
    
    if (result$success) {
      showNotification("Coding added successfully!", type = "message")
      
      # Refresh codings display
      load_file_codings()
      
      # Clear selection
      values$text_selection <- NULL
      shinyjs::runjs("window.getSelection().removeAllRanges();")
      
    } else {
      showNotification(result$message, type = "error")
    }
  })
  
  # Clear text selection
  observeEvent(input$clear_selection, {
    values$text_selection <- NULL
    shinyjs::runjs("window.getSelection().removeAllRanges();")
  })
  
  # Toggle coding highlights
  observeEvent(input$toggle_codings, {
    values$show_codings <- !values$show_codings
    showNotification(
      paste("Coding highlights", ifelse(values$show_codings, "shown", "hidden")), 
      type = "message"
    )
  })
  
  # Render coding legend
  output$coding_legend_display <- renderUI({
    if (is.null(values$codings) || nrow(values$codings) == 0) {
      return(tags$small("No codings in this file", style = "color: #666; font-style: italic;"))
    }
    
    # Get unique codes used in this file
    unique_codes <- unique(values$codings[, c("code_name", "color")])
    
    legend_items <- lapply(1:nrow(unique_codes), function(i) {
      code_info <- unique_codes[i, ]
      code_count <- sum(values$codings$code_name == code_info$code_name)
      
      span(
        style = paste0("display: inline-block; margin: 2px 8px 2px 0; padding: 3px 8px; ",
                       "background-color: ", code_info$color, "; border-radius: 3px; ",
                       "border: 1px solid ", darken_color(code_info$color), "; font-size: 0.9em;"),
        code_info$code_name, " (", code_count, ")"
      )
    })
    
    do.call(tagList, legend_items)
  })
  
  # Auto-coding functionality
  observeEvent(input$auto_code, {
    if (is.null(values$selected_code_id)) {
      showNotification("Please select a code first", type = "warning")
      return()
    }
    
    selected_code <- values$codes[values$codes$id == values$selected_code_id, ][1, ]
    
    showModal(
      modalDialog(
        title = paste("Auto-Code with:", selected_code$name),
        size = "m",
        
        textInput(
          ns("auto_code_term"),
          "Search Term",
          placeholder = "Enter text to search for and automatically code",
          value = ""
        ),
        
        fluidRow(
          column(6,
            checkboxInput(
              ns("auto_code_case_sensitive"),
              "Case sensitive",
              value = FALSE
            )
          ),
          column(6,
            checkboxInput(
              ns("auto_code_whole_words"),
              "Whole words only",
              value = FALSE
            )
          )
        ),
        
        checkboxInput(
          ns("auto_code_all_files"),
          "Apply to all files in project",
          value = FALSE
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_auto_code"),
            "Start Auto-Coding",
            class = "btn-primary",
            icon = icon("magic")
          )
        )
      )
    )
  })
  
  # Confirm auto-coding
  observeEvent(input$confirm_auto_code, {
    if (is.null(input$auto_code_term) || input$auto_code_term == "") {
      showNotification("Please enter a search term", type = "error")
      return()
    }
    
    # Get username for owner field
    user_info_query <- "SELECT username FROM users WHERE id = ?"
    user_info <- db_execute_query(db_pool, user_info_query, list(user_id()))
    owner_name <- if (!is.null(user_info)) user_info$username[1] else "Unknown"
    
    # Determine which files to process
    file_ids <- if (input$auto_code_all_files) {
      if (!is.null(values$files)) values$files$id else NULL
    } else {
      if (!is.null(values$selected_file_id)) c(values$selected_file_id) else NULL
    }
    
    if (is.null(file_ids)) {
      showNotification("No files available for auto-coding", type = "error")
      return()
    }
    
    result <- auto_code_search(
      db_pool,
      input$auto_code_term,
      values$selected_code_id,
      current_project_id(),
      user_id(),
      owner_name,
      file_ids,
      input$auto_code_case_sensitive
    )
    
    if (result$success) {
      showNotification(result$message, type = "message")
      removeModal()
      
      # Refresh codings if current file was affected
      if (values$selected_file_id %in% file_ids) {
        load_file_codings()
      }
      
    } else {
      showNotification(result$message, type = "error")
    }
  })
  
  # Code memo functionality
  observeEvent(input$code_memo, {
    if (is.null(values$selected_code_id)) {
      showNotification("Please select a code first", type = "warning")
      return()
    }
    
    selected_code <- values$codes[values$codes$id == values$selected_code_id, ][1, ]
    current_memo <- ifelse(is.na(selected_code$memo), "", selected_code$memo)
    
    showModal(
      modalDialog(
        title = paste("Code Memo:", selected_code$name),
        size = "l",
        
        textAreaInput(
          ns("code_memo_text"),
          "Code Memo",
          value = current_memo,
          height = "300px",
          placeholder = "Document your thoughts about this code, its meaning, and how it relates to your analysis..."
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_code_memo"),
            "Save Memo",
            class = "btn-primary",
            icon = icon("save")
          )
        )
      )
    )
  })
  
  # Save code memo
  observeEvent(input$save_code_memo, {
    if (!is.null(values$selected_code_id)) {
      memo_query <- "UPDATE freecode SET memo = ?, dateM = NOW() WHERE id = ? AND project_id = ?"
      
      tryCatch({
        pool::dbExecute(db_pool, memo_query, 
                       list(input$code_memo_text, values$selected_code_id, current_project_id()))
        showNotification("Code memo saved", type = "message")
        removeModal()
        load_codes()
      }, error = function(e) {
        showNotification(paste("Error saving memo:", e$message), type = "error")
      })
    }
  })
  
  # Return reactive values for use by other modules
  return(list(
    codes = reactive({ values$codes }),
    selected_code_id = reactive({ values$selected_code_id }),
    selected_file_id = reactive({ values$selected_file_id }),
    current_file_content = reactive({ values$current_file_content }),
    codings = reactive({ values$codings }),
    text_selection = reactive({ values$text_selection })
  ))
}

# Text Coding Server (for dedicated coding interface)
textCodingServer <- function(input, output, session, db_pool, user_id, current_project_id, file_data, code_data) {
  ns <- session$ns
  
  # Reactive values for dedicated coding interface
  values <- reactiveValues(
    active_code_id = NULL,
    active_file_id = NULL,
    file_content = NULL,
    codings = NULL,
    text_selection = NULL,
    last_action = "Ready to code"
  )
  
  # Update file choices when files change
  observe({
    if (!is.null(file_data()) && nrow(file_data()) > 0) {
      file_choices <- setNames(file_data()$id, file_data()$name)
      updateSelectInput(session, "coding_file_select", choices = file_choices)
    }
  })
  
  # Update code choices when codes change
  observe({
    if (!is.null(code_data()) && nrow(code_data()) > 0) {
      code_choices <- setNames(code_data()$id, code_data()$name)
      updateSelectInput(session, "active_code_select", choices = code_choices)
    }
  })
  
  # Render quick codes list
  output$quick_codes_list <- renderUI({
    if (is.null(code_data()) || nrow(code_data()) == 0) {
      return(
        div(
          style = "text-align: center; padding: 30px; color: #666;",
          icon("code"),
          p("No codes available", style = "margin-top: 10px;"),
          tags$small("Create codes first")
        )
      )
    }
    
    # Create quick access buttons for codes
    code_buttons <- lapply(1:min(nrow(code_data()), 10), function(i) {
      code <- code_data()[i, ]
      
      actionButton(
        ns(paste0("quick_code_", code$id)),
        paste0(i, ". ", code$name),
        class = "btn-outline-primary btn-sm",
        style = paste0("width: 100%; margin-bottom: 5px; text-align: left; background-color: ", 
                       ifelse(is.na(code$color), "#f8f9fa", code$color), ";"),
        onclick = paste0("Shiny.setInputValue('", ns("quick_code_selected"), "', ", code$id, ");")
      )
    })
    
    do.call(tagList, code_buttons)
  })
  
  # Handle quick code selection
  observe({
    if (!is.null(input$quick_code_selected)) {
      values$active_code_id <- input$quick_code_selected
      updateSelectInput(session, "active_code_select", selected = input$quick_code_selected)
    }
  })
  
  # Handle file selection
  observeEvent(input$coding_file_select, {
    if (!is.null(input$coding_file_select) && input$coding_file_select != "") {
      values$active_file_id <- as.integer(input$coding_file_select)
      
      # Load file content
      file_content <- get_file_content(db_pool, values$active_file_id, user_id(), current_project_id())
      values$file_content <- file_content
      
      # Load existing codings
      codings <- get_file_codings(db_pool, values$active_file_id, current_project_id(), user_id())
      values$codings <- codings
      
      values$last_action <- paste("Loaded file:", file_content$name[1])
    }
  })
  
  # Handle active code selection
  observeEvent(input$active_code_select, {
    if (!is.null(input$active_code_select) && input$active_code_select != "") {
      values$active_code_id <- as.integer(input$active_code_select)
      
      # Get code name for display
      if (!is.null(code_data())) {
        selected_code <- code_data()[code_data()$id == values$active_code_id, ]
        if (nrow(selected_code) > 0) {
          values$last_action <- paste("Selected code:", selected_code$name[1])
        }
      }
    }
  })
  
  # Render main text display
  output$main_text_display <- renderUI({
    if (is.null(values$file_content)) {
      return(
        div(
          style = "text-align: center; padding: 100px; color: #666;",
          icon("file-text", "fa-4x"),
          h3("Select a document to begin coding", style = "margin-top: 20px;"),
          p("Choose a file from the dropdown above to start your analysis")
        )
      )
    }
    
    text_content <- values$file_content$file[1]
    
    # Apply coding highlights
    if (!is.null(values$codings) && nrow(values$codings) > 0) {
      text_content <- apply_coding_highlights(text_content, values$codings)
    }
    
    # Add JavaScript for text selection and keyboard shortcuts
    div(
      id = ns("main_coding_text"),
      style = "user-select: text;",
      HTML(text_content),
      tags$script(HTML(paste0("
        // Text selection handler
        document.getElementById('", ns("main_coding_text"), "').addEventListener('mouseup', function() {
          var selection = window.getSelection();
          if (selection.toString().length > 0) {
            var selectedText = selection.toString();
            var range = selection.getRangeAt(0);
            var startOffset = getAbsoluteOffset(range.startContainer, range.startOffset);
            var endOffset = startOffset + selectedText.length;
            
            Shiny.setInputValue('", ns("main_text_selection"), "', {
              text: selectedText,
              start: startOffset,
              end: endOffset,
              timestamp: Date.now()
            });
          }
        });
        
        // Keyboard shortcuts for coding
        document.addEventListener('keydown', function(e) {
          if (e.target.closest('#", ns("main_coding_text"), "')) {
            // Number keys 1-9 for quick coding
            if (e.key >= '1' && e.key <= '9' && !e.ctrlKey && !e.altKey && !e.metaKey) {
              var codeIndex = parseInt(e.key) - 1;
              Shiny.setInputValue('", ns("keyboard_code_selected"), "', codeIndex);
              e.preventDefault();
            }
            // Enter key to apply active code
            else if (e.key === 'Enter' && !e.shiftKey) {
              Shiny.setInputValue('", ns("apply_active_code"), "', Date.now());
              e.preventDefault();
            }
          }
        });
        
        function getAbsoluteOffset(node, offset) {
          var textContent = document.getElementById('", ns("main_coding_text"), "').textContent;
          var nodeText = node.textContent;
          var nodeStart = textContent.indexOf(nodeText);
          return nodeStart + offset;
        }
      ")))
    )
  })
  
  # Handle text selection
  observe({
    if (!is.null(input$main_text_selection)) {
      values$text_selection <- input$main_text_selection
    }
  })
  
  # Handle keyboard shortcuts
  observe({
    if (!is.null(input$keyboard_code_selected) && !is.null(code_data())) {
      code_index <- input$keyboard_code_selected + 1
      if (code_index <= nrow(code_data())) {
        selected_code_id <- code_data()$id[code_index]
        values$active_code_id <- selected_code_id
        updateSelectInput(session, "active_code_select", selected = selected_code_id)
        
        # Auto-apply if text is selected
        if (!is.null(values$text_selection)) {
          apply_coding()
        }
      }
    }
  })
  
  # Handle enter key for coding
  observeEvent(input$apply_active_code, {
    apply_coding()
  })
  
  # Apply coding function
  apply_coding <- function() {
    if (is.null(values$text_selection) || is.null(values$active_code_id) || is.null(values$active_file_id)) {
      showNotification("Please select text and choose an active code", type = "warning")
      return()
    }
    
    # Get username for owner field
    user_info_query <- "SELECT username FROM users WHERE id = ?"
    user_info <- db_execute_query(db_pool, user_info_query, list(user_id()))
    owner_name <- if (!is.null(user_info)) user_info$username[1] else "Unknown"
    
    result <- add_coding(
      db_pool,
      values$active_code_id,
      values$active_file_id,
      values$text_selection$text,
      values$text_selection$start,
      values$text_selection$end,
      current_project_id(),
      user_id(),
      owner_name
    )
    
    if (result$success) {
      # Refresh codings
      codings <- get_file_codings(db_pool, values$active_file_id, current_project_id(), user_id())
      values$codings <- codings
      
      # Update last action
      if (!is.null(code_data())) {
        code_name <- code_data()[code_data()$id == values$active_code_id, ]$name[1]
        values$last_action <- paste("Applied code:", code_name)
      }
      
      # Clear selection
      values$text_selection <- NULL
      shinyjs::runjs("window.getSelection().removeAllRanges();")
      
    } else {
      showNotification(result$message, type = "error")
    }
  }
  
  # Render coding summary outputs
  output$total_codings <- renderText({
    if (!is.null(values$codings)) {
      nrow(values$codings)
    } else {
      "0"
    }
  })
  
  output$active_code_name <- renderText({
    if (!is.null(values$active_code_id) && !is.null(code_data())) {
      selected_code <- code_data()[code_data()$id == values$active_code_id, ]
      if (nrow(selected_code) > 0) {
        selected_code$name[1]
      } else {
        "None selected"
      }
    } else {
      "None selected"
    }
  })
  
  output$last_action <- renderText({
    values$last_action
  })
  
  # Coding help modal
  observeEvent(input$coding_help, {
    showModal(
      modalDialog(
        title = "Coding Help",
        size = "l",
        
        h4("How to Code Text"),
        tags$ul(
          tags$li("Select text with your mouse by clicking and dragging"),
          tags$li("Choose an active code from the dropdown or quick codes panel"),
          tags$li("Press Enter or use number keys (1-9) to apply the code"),
          tags$li("Coded segments will be highlighted in the text")
        ),
        
        h4("Keyboard Shortcuts"),
        tags$ul(
          tags$li("Numbers 1-9: Select and apply quick codes"),
          tags$li("Enter: Apply the active code to selected text"),
          tags$li("Shift+Click: Extend text selection")
        ),
        
        h4("Tips for Effective Coding"),
        tags$ul(
          tags$li("Start with broad codes and refine as you analyze"),
          tags$li("Use consistent coding standards across your project"),
          tags$li("Add memos to codes to document your thinking"),
          tags$li("Review and revise your coding scheme regularly")
        ),
        
        footer = modalButton("Close")
      )
    )
  })
  
  # View all codings
  observeEvent(input$view_all_codings, {
    if (is.null(values$codings) || nrow(values$codings) == 0) {
      showNotification("No codings in this file", type = "info")
      return()
    }
    
    showModal(
      modalDialog(
        title = "All Codings in This File",
        size = "l",
        
        DT::dataTableOutput(ns("all_codings_table")),
        
        footer = modalButton("Close")
      )
    )
  })
  
  # Render all codings table
  output$all_codings_table <- DT::renderDataTable({
    if (!is.null(values$codings)) {
      display_data <- values$codings[, c("code_name", "seltext", "owner", "date")]
      display_data$date <- format(as.POSIXct(display_data$date), "%Y-%m-%d %H:%M")
      
      DT::datatable(
        display_data,
        colnames = c("Code", "Text Segment", "Coded By", "Date"),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
  })
  
  # Return reactive values
  return(list(
    active_code_id = reactive({ values$active_code_id }),
    active_file_id = reactive({ values$active_file_id }),
    codings = reactive({ values$codings }),
    last_action = reactive({ values$last_action })
  ))
}
