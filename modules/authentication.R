# modules/authentication.R
# User authentication system

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(digest)
library(DT)

# Authentication UI
authenticationUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "login-panel",
    box(
      title = "RQDA Online",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      # App description
      div(
        style = "text-align: center; margin-bottom: 20px;",
        h4("Web-based Qualitative Data Analysis", style = "color: #666;"),
        p("Collaborative platform for qualitative research", style = "color: #888;")
      ),
      
      tabsetPanel(
        id = ns("auth_tabs"),
        
        # Login tab
        tabPanel(
          "Login",
          br(),
          div(
            style = "max-width: 300px; margin: 0 auto;",
            
            textInput(
              ns("login_username"),
              "Username or Email",
              placeholder = "Enter your username or email",
              width = "100%"
            ),
            
            passwordInput(
              ns("login_password"),
              "Password",
              placeholder = "Enter your password",
              width = "100%"
            ),
            
            div(
              style = "text-align: left; margin: 10px 0;",
              checkboxInput(
                ns("remember_me"),
                "Remember me",
                value = FALSE
              )
            ),
            
            br(),
            
            actionButton(
              ns("login_btn"),
              "Login",
              class = "btn-primary btn-lg",
              style = "width: 100%;",
              icon = icon("sign-in-alt")
            ),
            
            br(), br(),
            
            div(
              style = "text-align: center;",
              actionLink(
                ns("show_register"), 
                "Don't have an account? Register here",
                style = "color: #337ab7;"
              ),
              br(),
              actionLink(
                ns("forgot_password"), 
                "Forgot your password?",
                style = "color: #666; font-size: 0.9em;"
              )
            )
          )
        ),
        
        # Registration tab
        tabPanel(
          "Register",
          br(),
          div(
            style = "max-width: 300px; margin: 0 auto;",
            
            textInput(
              ns("reg_username"),
              "Username",
              placeholder = "Choose a username (3-20 characters)",
              width = "100%"
            ),
            
            textInput(
              ns("reg_email"),
              "Email Address",
              placeholder = "Enter your email address",
              width = "100%"
            ),
            
            passwordInput(
              ns("reg_password"),
              "Password",
              placeholder = "Choose a password (min 8 characters)",
              width = "100%"
            ),
            
            passwordInput(
              ns("reg_password_confirm"),
              "Confirm Password",
              placeholder = "Confirm your password",
              width = "100%"
            ),
            
            div(
              style = "margin: 15px 0;",
              checkboxInput(
                ns("agree_terms"),
                HTML("I agree to the <a href='#' target='_blank'>Terms of Service</a> and <a href='#' target='_blank'>Privacy Policy</a>"),
                value = FALSE
              )
            ),
            
            br(),
            
            actionButton(
              ns("register_btn"),
              "Create Account",
              class = "btn-success btn-lg",
              style = "width: 100%;",
              icon = icon("user-plus")
            ),
            
            br(), br(),
            
            div(
              style = "text-align: center;",
              actionLink(
                ns("show_login"), 
                "Already have an account? Login here",
                style = "color: #337ab7;"
              )
            )
          )
        ),
        
        # Password Reset tab (initially hidden)
        tabPanel(
          "Reset Password",
          br(),
          div(
            style = "max-width: 300px; margin: 0 auto;",
            
            h4("Reset Your Password", style = "text-align: center; margin-bottom: 20px;"),
            
            p("Enter your email address and we'll help you reset your password.", 
              style = "text-align: center; color: #666; margin-bottom: 20px;"),
            
            textInput(
              ns("reset_email"),
              "Email Address",
              placeholder = "Enter your email address",
              width = "100%"
            ),
            
            br(),
            
            actionButton(
              ns("reset_btn"),
              "Send Reset Instructions",
              class = "btn-warning btn-lg",
              style = "width: 100%;",
              icon = icon("envelope")
            ),
            
            br(), br(),
            
            div(
              style = "text-align: center;",
              actionLink(
                ns("back_to_login"), 
                "Back to Login",
                style = "color: #337ab7;"
              )
            )
          )
        )
      ),
      
      # Message area
      div(
        id = ns("message_area"),
        style = "margin-top: 15px;"
      )
    ),
    
    # Footer
    div(
      style = "text-align: center; margin-top: 20px; color: #999; font-size: 0.9em;",
      "RQDA Online v1.0 | Powered by R Shiny"
    )
  )
}

# User Management UI (for admin users)
userManagementUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "User Management",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      # Action buttons
      fluidRow(
        column(
          width = 12,
          actionButton(
            ns("refresh_users"),
            "Refresh",
            icon = icon("refresh"),
            class = "btn-info",
            style = "margin-bottom: 15px;"
          ),
          actionButton(
            ns("add_user"),
            "Add User",
            icon = icon("user-plus"),
            class = "btn-success",
            style = "margin-bottom: 15px; margin-left: 10px;"
          ),
          conditionalPanel(
            condition = paste0("output['", ns("has_selected_user"), "']"),
            actionButton(
              ns("edit_user"),
              "Edit Selected",
              icon = icon("edit"),
              class = "btn-warning",
              style = "margin-bottom: 15px; margin-left: 10px;"
            ),
            actionButton(
              ns("delete_user"),
              "Delete Selected",
              icon = icon("trash"),
              class = "btn-danger",
              style = "margin-bottom: 15px; margin-left: 10px;"
            )
          )
        )
      ),
      
      # Users table
      div(
        style = "margin-top: 10px;",
        withSpinner(DT::dataTableOutput(ns("users_table")))
      )
    )
  )
}

# Authentication Server
authenticationServer <- function(input, output, session, db_pool) {
  ns <- session$ns
  
  # Reactive values to store authentication state
  auth_state <- reactiveValues(
    authenticated = FALSE,
    user_id = NULL,
    username = NULL,
    email = NULL,
    is_admin = FALSE,
    login_time = NULL,
    last_activity = NULL
  )
  
  # Navigation between tabs
  observeEvent(input$show_register, {
    updateTabsetPanel(session, "auth_tabs", selected = "Register")
  })
  
  observeEvent(input$show_login, {
    updateTabsetPanel(session, "auth_tabs", selected = "Login")
  })
  
  observeEvent(input$forgot_password, {
    updateTabsetPanel(session, "auth_tabs", selected = "Reset Password")
  })
  
  observeEvent(input$back_to_login, {
    updateTabsetPanel(session, "auth_tabs", selected = "Login")
  })
  
  # Helper function to validate username
  validate_username <- function(username) {
    errors <- c()
    
    if (is.null(username) || username == "") {
      errors <- c(errors, "Username is required")
    } else {
      if (nchar(username) < 3) {
        errors <- c(errors, "Username must be at least 3 characters")
      }
      if (nchar(username) > 20) {
        errors <- c(errors, "Username must be less than 20 characters")
      }
      if (!grepl("^[a-zA-Z0-9_]+$", username)) {
        errors <- c(errors, "Username can only contain letters, numbers, and underscores")
      }
    }
    
    return(errors)
  }
  
  # Helper function to validate email
  validate_email <- function(email) {
    if (is.null(email) || email == "") {
      return("Email is required")
    }
    
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", email)) {
      return("Please enter a valid email address")
    }
    
    return(NULL)
  }
  
  # Helper function to validate password
  validate_password <- function(password) {
    errors <- c()
    
    if (is.null(password) || password == "") {
      errors <- c(errors, "Password is required")
    } else {
      if (nchar(password) < 8) {
        errors <- c(errors, "Password must be at least 8 characters")
      }
      if (!grepl("[A-Za-z]", password)) {
        errors <- c(errors, "Password must contain at least one letter")
      }
      if (!grepl("[0-9]", password)) {
        errors <- c(errors, "Password must contain at least one number")
      }
    }
    
    return(errors)
  }
  
  # Registration logic
  observeEvent(input$register_btn, {
    # Clear previous messages
    clear_messages()
    
    # Validate inputs
    username_errors <- validate_username(input$reg_username)
    email_error <- validate_email(input$reg_email)
    password_errors <- validate_password(input$reg_password)
    
    all_errors <- c(username_errors, email_error, password_errors)
    
    # Check password confirmation
    if (!is.null(input$reg_password) && !is.null(input$reg_password_confirm)) {
      if (input$reg_password != input$reg_password_confirm) {
        all_errors <- c(all_errors, "Passwords do not match")
      }
    }
    
    # Check terms agreement
    if (!input$agree_terms) {
      all_errors <- c(all_errors, "You must agree to the Terms of Service and Privacy Policy")
    }
    
    # Show validation errors
    if (length(all_errors) > 0) {
      show_message(paste(all_errors, collapse = "<br>"), "error")
      return()
    }
    
    # Check if username or email already exists
    check_query <- "SELECT COUNT(*) as count FROM users WHERE username = ? OR email = ?"
    existing <- db_execute_query(db_pool, check_query, list(input$reg_username, input$reg_email))
    
    if (!is.null(existing) && existing$count[1] > 0) {
      show_message("Username or email already exists", "error")
      return()
    }
    
    # Hash password
    password_hash <- digest::digest(input$reg_password, algo = "sha256")
    
    # Insert new user
    insert_query <- "
      INSERT INTO users (username, email, password_hash, created_at, active) 
      VALUES (?, ?, ?, NOW(), 1)
    "
    
    tryCatch({
      result <- pool::dbExecute(db_pool, insert_query, 
                               list(input$reg_username, input$reg_email, password_hash))
      
      show_message("Registration successful! Please login with your new account.", "success")
      
      # Clear registration form
      updateTextInput(session, "reg_username", value = "")
      updateTextInput(session, "reg_email", value = "")
      updateTextInput(session, "reg_password", value = "")
      updateTextInput(session, "reg_password_confirm", value = "")
      updateCheckboxInput(session, "agree_terms", value = FALSE)
      
      # Switch to login tab
      updateTabsetPanel(session, "auth_tabs", selected = "Login")
      
      # Pre-fill username for convenience
      updateTextInput(session, "login_username", value = input$reg_username)
      
    }, error = function(e) {
      show_message(paste("Registration failed:", e$message), "error")
    })
  })
  
  # Login logic
  observeEvent(input$login_btn, {
    # Clear previous messages
    clear_messages()
    
    # Validate inputs
    if (is.null(input$login_username) || input$login_username == "") {
      show_message("Please enter your username or email", "error")
      return()
    }
    
    if (is.null(input$login_password) || input$login_password == "") {
      show_message("Please enter your password", "error")
      return()
    }
    
    # Hash password for comparison
    password_hash <- digest::digest(input$login_password, algo = "sha256")
    
    # Check credentials (allow login with username or email)
    login_query <- "
      SELECT id, username, email, active 
      FROM users 
      WHERE (username = ? OR email = ?) AND password_hash = ?
    "
    
    user <- db_execute_query(db_pool, login_query, 
                           list(input$login_username, input$login_username, password_hash))
    
    if (is.null(user) || nrow(user) == 0) {
      show_message("Invalid username/email or password", "error")
      return()
    }
    
    if (user$active[1] != 1) {
      show_message("Account is deactivated. Please contact administrator.", "error")
      return()
    }
    
    # Update last login
    update_query <- "UPDATE users SET last_login = NOW() WHERE id = ?"
    pool::dbExecute(db_pool, update_query, list(user$id[1]))
    
    # Set authentication state
    auth_state$authenticated <- TRUE
    auth_state$user_id <- user$id[1]
    auth_state$username <- user$username[1]
    auth_state$email <- user$email[1]
    auth_state$login_time <- Sys.time()
    auth_state$last_activity <- Sys.time()
    
    # Check if user is admin (first user or explicitly marked)
    admin_check_query <- "SELECT COUNT(*) as count FROM users WHERE id <= ?"
    admin_check <- db_execute_query(db_pool, admin_check_query, list(user$id[1]))
    auth_state$is_admin <- (!is.null(admin_check) && admin_check$count[1] == 1)
    
    show_message("Login successful! Welcome to RQDA Online.", "success")
    
    # Clear login form
    updateTextInput(session, "login_username", value = "")
    updateTextInput(session, "login_password", value = "")
    updateCheckboxInput(session, "remember_me", value = FALSE)
  })
  
  # Password reset logic
  observeEvent(input$reset_btn, {
    clear_messages()
    
    email_error <- validate_email(input$reset_email)
    if (!is.null(email_error)) {
      show_message(email_error, "error")
      return()
    }
    
    # Check if email exists
    check_query <- "SELECT id FROM users WHERE email = ? AND active = 1"
    user_check <- db_execute_query(db_pool, check_query, list(input$reset_email))
    
    if (is.null(user_check) || nrow(user_check) == 0) {
      # Don't reveal if email exists or not for security
      show_message("If this email address exists in our system, you will receive reset instructions shortly.", "info")
    } else {
      # In a real application, you would send an email here
      # For now, just show a message
      show_message("Password reset functionality would be implemented here. Please contact your administrator.", "info")
    }
    
    updateTextInput(session, "reset_email", value = "")
  })
  
  # Session timeout check
  observe({
    if (auth_state$authenticated) {
      # Update last activity
      auth_state$last_activity <- Sys.time()
      
      # Check for session timeout (30 minutes)
      if (difftime(Sys.time(), auth_state$last_activity, units = "mins") > 30) {
        auth_state$authenticated <- FALSE
        auth_state$user_id <- NULL
        auth_state$username <- NULL
        auth_state$email <- NULL
        auth_state$is_admin <- FALSE
        showNotification("Session expired. Please login again.", type = "warning")
      }
    }
  })
  
  # Helper function to show messages
  show_message <- function(message, type = "info") {
    if (type == "error") {
      class <- "alert alert-danger"
      icon_name <- "exclamation-triangle"
    } else if (type == "success") {
      class <- "alert alert-success"
      icon_name <- "check-circle"
    } else if (type == "warning") {
      class <- "alert alert-warning"
      icon_name <- "exclamation-triangle"
    } else {
      class <- "alert alert-info"
      icon_name <- "info-circle"
    }
    
    insertUI(
      selector = paste0("#", ns("message_area")),
      where = "afterBegin",
      ui = div(
        class = class,
        style = "margin-top: 10px;",
        icon(icon_name),
        " ",
        HTML(message),
        tags$button(
          type = "button",
          class = "close",
          `data-dismiss` = "alert",
          HTML("&times;")
        )
      )
    )
    
    # Auto-remove success messages after 5 seconds
    if (type == "success") {
      shinyjs::delay(5000, {
        shinyjs::runjs(paste0("$('#", ns("message_area"), " .alert-success').fadeOut();"))
      })
    }
  }
  
  # Helper function to clear messages
  clear_messages <- function() {
    shinyjs::runjs(paste0("$('#", ns("message_area"), "').empty();"))
  }
  
  # Get user info function
  get_user_info <- function(user_id) {
    if (is.null(user_id)) return(NULL)
    
    query <- "SELECT id, username, email, created_at, last_login, active FROM users WHERE id = ?"
    db_execute_query(db_pool, query, list(user_id))
  }
  
  # Return reactive values for use in main app
  return(list(
    authenticated = reactive({ auth_state$authenticated }),
    user_id = reactive({ auth_state$user_id }),
    username = reactive({ auth_state$username }),
    email = reactive({ auth_state$email }),
    is_admin = reactive({ auth_state$is_admin }),
    login_time = reactive({ auth_state$login_time }),
    get_user_info = get_user_info,
    logout = function() {
      auth_state$authenticated <- FALSE
      auth_state$user_id <- NULL
      auth_state$username <- NULL
      auth_state$email <- NULL
      auth_state$is_admin <- FALSE
      auth_state$login_time <- NULL
      auth_state$last_activity <- NULL
    }
  ))
}

# User Management Server (for admin users)
userManagementServer <- function(input, output, session, db_pool, auth_result) {
  ns <- session$ns
  
  # Reactive values
  values <- reactiveValues(
    users = NULL,
    selected_user_row = NULL
  )
  
  # Check if current user is admin
  observe({
    if (!auth_result$is_admin()) {
      showNotification("Access denied. Admin privileges required.", type = "error")
      return()
    }
  })
  
  # Load users function
  load_users <- function() {
    if (auth_result$is_admin()) {
      query <- "
        SELECT id, username, email, created_at, last_login, active,
               (SELECT COUNT(*) FROM project WHERE created_by = users.id) as project_count
        FROM users 
        ORDER BY created_at DESC
      "
      users <- db_execute_query(db_pool, query)
      if (!is.null(users)) {
        users$created_at <- format(as.POSIXct(users$created_at), "%Y-%m-%d %H:%M")
        users$last_login <- ifelse(is.na(users$last_login), "Never", 
                                  format(as.POSIXct(users$last_login), "%Y-%m-%d %H:%M"))
        users$active <- ifelse(users$active == 1, "Active", "Inactive")
      }
      values$users <- users
    }
  }
  
  # Load users on start
  observe({
    load_users()
  })
  
  # Refresh users
  observeEvent(input$refresh_users, {
    load_users()
    showNotification("Users refreshed", type = "message")
  })
  
  # Render users table
  output$users_table <- DT::renderDataTable({
    if (is.null(values$users) || !auth_result$is_admin()) return(NULL)
    
    DT::datatable(
      values$users[, c("username", "email", "active", "created_at", "last_login", "project_count")],
      selection = "single",
      rownames = FALSE,
      colnames = c("Username", "Email", "Status", "Created", "Last Login", "Projects"),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'lrtip'
      )
    ) %>%
      DT::formatStyle(
        "active",
        backgroundColor = DT::styleEqual(
          c("Active", "Inactive"),
          c("#d4edda", "#f8d7da")
        )
      )
  })
  
  # Track selected user
  observeEvent(input$users_table_rows_selected, {
    values$selected_user_row <- input$users_table_rows_selected
  })
  
  # Check if user is selected
  output$has_selected_user <- reactive({
    !is.null(values$selected_user_row)
  })
  outputOptions(output, "has_selected_user", suspendWhenHidden = FALSE)
  
  # Additional user management functions would go here...
  # (Add user, edit user, delete user, etc.)
  
  # Add user functionality
  observeEvent(input$add_user, {
    if (!auth_result$is_admin()) {
      showNotification("Access denied", type = "error")
      return()
    }
    
    showModal(
      modalDialog(
        title = "Add New User",
        size = "m",
        
        textInput(
          ns("new_user_username"),
          "Username",
          placeholder = "Enter username"
        ),
        
        textInput(
          ns("new_user_email"),
          "Email",
          placeholder = "Enter email address"
        ),
        
        passwordInput(
          ns("new_user_password"),
          "Temporary Password",
          placeholder = "Enter temporary password"
        ),
        
        checkboxInput(
          ns("new_user_active"),
          "Active Account",
          value = TRUE
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_add_user"),
            "Add User",
            class = "btn-success"
          )
        )
      )
    )
  })
  
  # Confirm add user
  observeEvent(input$confirm_add_user, {
    # Validation
    if (is.null(input$new_user_username) || input$new_user_username == "") {
      showNotification("Username is required", type = "error")
      return()
    }
    
    if (is.null(input$new_user_email) || input$new_user_email == "") {
      showNotification("Email is required", type = "error")
      return()
    }
    
    if (is.null(input$new_user_password) || input$new_user_password == "") {
      showNotification("Password is required", type = "error")
      return()
    }
    
    # Check if username or email exists
    check_query <- "SELECT COUNT(*) as count FROM users WHERE username = ? OR email = ?"
    existing <- db_execute_query(db_pool, check_query, 
                               list(input$new_user_username, input$new_user_email))
    
    if (!is.null(existing) && existing$count[1] > 0) {
      showNotification("Username or email already exists", type = "error")
      return()
    }
    
    # Hash password
    password_hash <- digest::digest(input$new_user_password, algo = "sha256")
    
    # Insert new user
    insert_query <- "
      INSERT INTO users (username, email, password_hash, created_at, active) 
      VALUES (?, ?, ?, NOW(), ?)
    "
    
    tryCatch({
      pool::dbExecute(db_pool, insert_query, 
                     list(input$new_user_username, input$new_user_email, 
                          password_hash, as.integer(input$new_user_active)))
      
      showNotification("User added successfully", type = "message")
      removeModal()
      load_users()
      
    }, error = function(e) {
      showNotification(paste("Error adding user:", e$message), type = "error")
    })
  })
  
  # Edit user functionality
  observeEvent(input$edit_user, {
    if (!auth_result$is_admin() || is.null(values$selected_user_row)) {
      showNotification("Please select a user to edit", type = "error")
      return()
    }
    
    selected_user <- values$users[values$selected_user_row, ]
    
    showModal(
      modalDialog(
        title = paste("Edit User:", selected_user$username),
        size = "m",
        
        textInput(
          ns("edit_user_username"),
          "Username",
          value = selected_user$username
        ),
        
        textInput(
          ns("edit_user_email"),
          "Email",
          value = selected_user$email
        ),
        
        checkboxInput(
          ns("edit_user_active"),
          "Active Account",
          value = selected_user$active == "Active"
        ),
        
        hr(),
        
        h5("Reset Password (Optional)"),
        passwordInput(
          ns("edit_user_new_password"),
          "New Password",
          placeholder = "Leave blank to keep current password"
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_edit_user"),
            "Update User",
            class = "btn-warning"
          )
        )
      )
    )
  })
  
  # Confirm edit user
  observeEvent(input$confirm_edit_user, {
    if (is.null(values$selected_user_row)) return()
    
    selected_user <- values$users[values$selected_user_row, ]
    user_id <- selected_user$id
    
    # Build update query
    if (!is.null(input$edit_user_new_password) && input$edit_user_new_password != "") {
      # Update with new password
      password_hash <- digest::digest(input$edit_user_new_password, algo = "sha256")
      update_query <- "
        UPDATE users 
        SET username = ?, email = ?, active = ?, password_hash = ? 
        WHERE id = ?
      "
      params <- list(input$edit_user_username, input$edit_user_email, 
                    as.integer(input$edit_user_active), password_hash, user_id)
    } else {
      # Update without changing password
      update_query <- "
        UPDATE users 
        SET username = ?, email = ?, active = ? 
        WHERE id = ?
      "
      params <- list(input$edit_user_username, input$edit_user_email, 
                    as.integer(input$edit_user_active), user_id)
    }
    
    tryCatch({
      pool::dbExecute(db_pool, update_query, params)
      showNotification("User updated successfully", type = "message")
      removeModal()
      load_users()
      
    }, error = function(e) {
      showNotification(paste("Error updating user:", e$message), type = "error")
    })
  })
  
  # Delete user functionality
  observeEvent(input$delete_user, {
    if (!auth_result$is_admin() || is.null(values$selected_user_row)) {
      showNotification("Please select a user to delete", type = "error")
      return()
    }
    
    selected_user <- values$users[values$selected_user_row, ]
    
    # Prevent deletion of current user
    if (selected_user$id == auth_result$user_id()) {
      showNotification("You cannot delete your own account", type = "error")
      return()
    }
    
    showModal(
      modalDialog(
        title = "Delete User",
        size = "m",
        
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          strong(" Warning: "),
          "This action cannot be undone!"
        ),
        
        p(paste("Are you sure you want to delete the user:", 
                strong(selected_user$username), "?")),
        
        p("This will permanently delete:"),
        tags$ul(
          tags$li("The user account"),
          tags$li("All projects owned by this user"),
          tags$li("All data associated with this user")
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_delete_user"),
            "Delete User",
            class = "btn-danger"
          )
        )
      )
    )
  })
  
  # Confirm delete user
  observeEvent(input$confirm_delete_user, {
    if (is.null(values$selected_user_row)) return()
    
    selected_user <- values$users[values$selected_user_row, ]
    
    tryCatch({
      # Delete user (CASCADE will handle related data)
      delete_query <- "DELETE FROM users WHERE id = ?"
      pool::dbExecute(db_pool, delete_query, list(selected_user$id))
      
      showNotification("User deleted successfully", type = "message")
      removeModal()
      load_users()
      
    }, error = function(e) {
      showNotification(paste("Error deleting user:", e$message), type = "error")
    })
  })
}

# Helper functions for password strength checking
check_password_strength <- function(password) {
  score <- 0
  feedback <- c()
  
  if (nchar(password) >= 8) {
    score <- score + 1
  } else {
    feedback <- c(feedback, "Use at least 8 characters")
  }
  
  if (grepl("[a-z]", password)) {
    score <- score + 1
  } else {
    feedback <- c(feedback, "Add lowercase letters")
  }
  
  if (grepl("[A-Z]", password)) {
    score <- score + 1
  } else {
    feedback <- c(feedback, "Add uppercase letters")
  }
  
  if (grepl("[0-9]", password)) {
    score <- score + 1
  } else {
    feedback <- c(feedback, "Add numbers")
  }
  
  if (grepl("[^A-Za-z0-9]", password)) {
    score <- score + 1
  } else {
    feedback <- c(feedback, "Add special characters")
  }
  
  strength <- switch(
    as.character(score),
    "0" = "Very Weak",
    "1" = "Very Weak", 
    "2" = "Weak",
    "3" = "Fair",
    "4" = "Good",
    "5" = "Strong"
  )
  
  return(list(
    score = score,
    strength = strength,
    feedback = feedback
  ))
}

# Generate secure temporary password
generate_temp_password <- function(length = 12) {
  chars <- c(letters, LETTERS, 0:9, c("!", "@", "#", "$", "%"))
  paste(sample(chars, length, replace = TRUE), collapse = "")
}

# Session management functions
is_session_valid <- function(last_activity, timeout_minutes = 30) {
  if (is.null(last_activity)) return(FALSE)
  
  time_diff <- difftime(Sys.time(), last_activity, units = "mins")
  return(time_diff <= timeout_minutes)
}

# Audit logging function
log_user_action <- function(db_pool, user_id, action, details = NULL) {
  # This would log user actions for security auditing
  # Implementation depends on your auditing requirements
  
  tryCatch({
    log_query <- "
      INSERT INTO user_audit_log (user_id, action, details, timestamp) 
      VALUES (?, ?, ?, NOW())
    "
    # Note: You'd need to create the user_audit_log table if implementing this
    # pool::dbExecute(db_pool, log_query, list(user_id, action, details))
  }, error = function(e) {
    # Silently fail audit logging to not break main functionality
  })
}
} <- "exclamation-triangle"
    } else if (type == "success") {
      class <- "alert alert-success"
      icon <- "check-circle"
    } else {
      class <- "alert alert-info"
      icon <- "info-circle"
    }
    
    insertUI(
      selector = paste0("#", ns("message_area")),
      where = "beforeEnd",
      ui = div(
        class = class,
        style = "margin-top: 10px;",
        icon(icon),
        " ",
        message,
        tags$button(
          type = "button",
          class = "close",
          `data-dismiss` = "alert",
          HTML("&times;")
        )
      )
    )
    
    # Auto-remove success messages after 3 seconds
    if (type == "success") {
      shinyjs::delay(3000, {
        shinyjs::runjs(paste0("$('#", ns("message_area"), " .alert-success').fadeOut();"))
      })
    }
  }
  
  # Return reactive values for use in main app
  return(list(
    authenticated = reactive({ auth_state$authenticated }),
    user_id = reactive({ auth_state$user_id }),
    username = reactive({ auth_state$username }),
    email = reactive({ auth_state$email })
  ))
}
