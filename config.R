# config.R - Configuration validation and setup utilities

# Validate environment variables
validate_environment <- function() {
  cat("Validating RQDA Online Environment Configuration...\n")
  cat("=" %R% 50, "\n")
  
  required_vars <- c("DB_HOST", "DB_NAME", "DB_USER", "DB_PASS")
  optional_vars <- c("DB_PORT", "SHINY_LOG_LEVEL", "APP_ENV")
  
  missing_vars <- c()
  warnings <- c()
  
  # Check required variables
  cat("\nRequired Environment Variables:\n")
  for (var in required_vars) {
    value <- Sys.getenv(var)
    if (value == "") {
      missing_vars <- c(missing_vars, var)
      cat(sprintf("✗ %s: NOT SET\n", var))
    } else {
      # Mask sensitive information
      display_value <- ifelse(var == "DB_PASS", 
                             paste0(substr(value, 1, 2), "***"), 
                             value)
      cat(sprintf("✓ %s: %s\n", var, display_value))
    }
  }
  
  # Check optional variables
  cat("\nOptional Environment Variables:\n")
  for (var in optional_vars) {
    value <- Sys.getenv(var)
    if (value == "") {
      cat(sprintf("- %s: not set (using default)\n", var))
    } else {
      cat(sprintf("✓ %s: %s\n", var, value))
    }
  }
  
  # Port validation
  port <- Sys.getenv("DB_PORT", "3306")
  if (!grepl("^[0-9]+$", port) || as.integer(port) < 1 || as.integer(port) > 65535) {
    warnings <- c(warnings, "DB_PORT should be a valid port number (1-65535)")
  }
  
  # Report results
  cat("\n")
  cat("=" %R% 50, "\n")
  
  if (length(missing_vars) > 0) {
    cat("❌ CONFIGURATION INCOMPLETE\n")
    cat(sprintf("Missing required variables: %s\n", paste(missing_vars, collapse = ", ")))
    cat("\nTo fix this, set these environment variables:\n")
    for (var in missing_vars) {
      cat(sprintf("  %s=your_value\n", var))
    }
    return(FALSE)
  }
  
  if (length(warnings) > 0) {
    cat("⚠️  CONFIGURATION WARNINGS\n")
    for (warning in warnings) {
      cat(sprintf("  - %s\n", warning))
    }
  }
  
  cat("✅ CONFIGURATION VALID\n")
  cat("All required environment variables are properly set.\n")
  return(TRUE)
}

# config.R - Configuration validation and setup utilities

# Create database configuration from environment variables only
get_db_config <- function() {
  config <- list(
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT", "3306"))
  )
  
  # Validate that all required variables are set
  required_vars <- c("host", "dbname", "username", "password")
  missing_vars <- c()
  
  for (var in required_vars) {
    if (is.null(config[[var]]) || config[[var]] == "" || is.na(config[[var]])) {
      missing_vars <- c(missing_vars, toupper(paste0("DB_", sub("host|dbname|username|password", 
                                                               c("HOST", "NAME", "USER", "PASS")[match(var, c("host", "dbname", "username", "password"))], var))))
    }
  }
  
  if (length(missing_vars) > 0) {
    stop(paste("Missing required environment variables:", paste(missing_vars, collapse = ", "), 
               "\nPlease set these in your Posit Connect app settings."))
  }
  
  return(config)
}

# Validate environment variables
validate_environment <- function() {
  cat("Validating RQDA Online Environment Configuration...\n")
  cat("=" %R% 50, "\n")
  
  required_vars <- c("DB_HOST", "DB_NAME", "DB_USER", "DB_PASS")
  optional_vars <- c("DB_PORT", "SHINY_LOG_LEVEL", "APP_ENV")
  
  missing_vars <- c()
  warnings <- c()
  
  # Check if running on Posit Connect
  is_posit_connect <- !is.na(Sys.getenv("R_CONFIG_ACTIVE", NA)) || 
                     !is.na(Sys.getenv("CONNECT_SERVER", NA)) ||
                     !is.na(Sys.getenv("RSTUDIO_PRODUCT", NA))
  
  if (is_posit_connect) {
    cat("🌐 Running on Posit Connect\n")
  } else {
    cat("💻 Running in local development environment\n")
  }
  
  # Check required variables
  cat("\nRequired Environment Variables:\n")
  for (var in required_vars) {
    value <- Sys.getenv(var)
    if (value == "") {
      missing_vars <- c(missing_vars, var)
      cat(sprintf("✗ %s: NOT SET\n", var))
    } else {
      # Mask sensitive information
      display_value <- ifelse(var == "DB_PASS", 
                             paste0(substr(value, 1, 2), "***"), 
                             value)
      cat(sprintf("✓ %s: %s\n", var, display_value))
    }
  }
  
  # Check optional variables
  cat("\nOptional Environment Variables:\n")
  for (var in optional_vars) {
    value <- Sys.getenv(var)
    if (value == "") {
      default_val <- switch(var,
        "DB_PORT" = "3306",
        "SHINY_LOG_LEVEL" = "INFO", 
        "APP_ENV" = "production",
        "not set"
      )
      cat(sprintf("- %s: not set (using default: %s)\n", var, default_val))
    } else {
      cat(sprintf("✓ %s: %s\n", var, value))
    }
  }
  
  # Port validation
  port <- Sys.getenv("DB_PORT", "3306")
  if (!grepl("^[0-9]+$", port) || as.integer(port) < 1 || as.integer(port) > 65535) {
    warnings <- c(warnings, "DB_PORT should be a valid port number (1-65535)")
  }
  
  # Environment-specific guidance
  cat("\nEnvironment Configuration:\n")
  if (is_posit_connect) {
    cat("📋 Posit Connect Deployment:\n")
    cat("   - Environment variables should be set in app settings\n")
    cat("   - Go to your app dashboard > Settings > Environment Variables\n")
    cat("   - Set: DB_HOST, DB_NAME, DB_USER, DB_PASS, DB_PORT\n")
  } else {
    cat("🏠 Local Development:\n")
    cat("   - Environment variables should be in .Renviron file\n")
    cat("   - Create .Renviron in your project root directory\n")
    cat("   - Add: DB_HOST=value, DB_NAME=value, etc.\n")
  }
  
  # Report results
  cat("\n")
  cat("=" %R% 50, "\n")
  
  if (length(missing_vars) > 0) {
    cat("❌ CONFIGURATION INCOMPLETE\n")
    cat(sprintf("Missing required variables: %s\n", paste(missing_vars, collapse = ", ")))
    
    if (is_posit_connect) {
      cat("\nTo fix on Posit Connect:\n")
      cat("1. Go to your app dashboard\n")
      cat("2. Click 'Settings' tab\n") 
      cat("3. Click 'Environment Variables' section\n")
      cat("4. Add the missing variables:\n")
      for (var in missing_vars) {
        cat(sprintf("   %s = your_value\n", var))
      }
      cat("5. Restart your application\n")
    } else {
      cat("\nTo fix locally:\n")
      cat("Create/edit .Renviron file with:\n")
      for (var in missing_vars) {
        cat(sprintf("   %s=your_value\n", var))
      }
      cat("Then restart R session\n")
    }
    return(FALSE)
  }
  
  if (length(warnings) > 0) {
    cat("⚠️  CONFIGURATION WARNINGS\n")
    for (warning in warnings) {
      cat(sprintf("  - %s\n", warning))
    }
  }
  
  cat("✅ CONFIGURATION VALID\n")
  cat("All required environment variables are properly set.\n")
  return(TRUE)
}

# Test database connection with detailed feedback and auto table creation
test_database_connection <- function(config = NULL) {
  if (is.null(config)) {
    config <- get_db_config()
  }
  
  cat("Testing Database Connection...\n")
  cat("Host:", config$host, "\n")
  cat("Database:", config$dbname, "\n")
  cat("User:", config$username, "\n")
  cat("Port:", config$port, "\n\n")
  
  tryCatch({
    # Test basic connection
    pool <- db_connect_pool(config)
    
    # Test query execution
    result <- pool::dbGetQuery(pool, "SELECT 1 as test, NOW() as current_time")
    
    cat("✅ Database connection successful!\n")
    cat("Server time:", as.character(result$current_time), "\n\n")
    
    # Create tables if they don't exist
    cat("Checking database schema...\n")
    schema_success <- create_tables_if_not_exist(pool)
    
    if (schema_success) {
      cat("✅ Database schema is ready\n")
    } else {
      cat("⚠️  Some tables could not be created\n")
    }
    
    # Verify core tables exist
    core_tables <- c("users", "project", "source", "freecode", "coding")
    missing_tables <- c()
    
    for (table in core_tables) {
      table_check <- pool::dbGetQuery(pool, sprintf("SHOW TABLES LIKE '%s'", table))
      if (nrow(table_check) == 0) {
        missing_tables <- c(missing_tables, table)
      }
    }
    
    if (length(missing_tables) == 0) {
      cat("✅ All core RQDA tables are present\n")
    } else {
      cat("❌ Missing core tables:", paste(missing_tables, collapse = ", "), "\n")
    }
    
    # Close connection
    pool::poolClose(pool)
    
    return(length(missing_tables) == 0)
    
  }, error = function(e) {
    cat("❌ Database connection failed!\n")
    cat("Error:", e$message, "\n")
    cat("\nCommon solutions:\n")
    cat("1. Check that DB_HOST is reachable\n")
    cat("2. Verify DB_USER and DB_PASS are correct\n")
    cat("3. Ensure the database DB_NAME exists\n")
    cat("4. Check firewall settings for DB_PORT\n")
    
    # Environment-specific troubleshooting
    is_posit_connect <- !is.na(Sys.getenv("R_CONFIG_ACTIVE", NA)) || 
                       !is.na(Sys.getenv("CONNECT_SERVER", NA))
    
    if (is_posit_connect) {
      cat("\nPostit Connect specific checks:\n")
      cat("5. Verify environment variables are set in app settings\n")
      cat("6. Check if database server allows connections from Posit Connect\n")
      cat("7. Verify network connectivity from Posit cloud to your database\n")
    } else {
      cat("\nLocal development checks:\n")
      cat("5. Check .Renviron file exists and has correct values\n")
      cat("6. Restart R session after changing .Renviron\n")
      cat("7. Test database connectivity from your local network\n")
    }
    
    return(FALSE)
  })
}

# Setup helper for new installations
setup_helper <- function() {
  is_posit_connect <- !is.na(Sys.getenv("R_CONFIG_ACTIVE", NA)) || 
                     !is.na(Sys.getenv("CONNECT_SERVER", NA))
  
  cat("RQDA Online Setup Helper\n")
  cat("========================\n\n")
  
  if (is_posit_connect) {
    cat("🌐 POSIT CONNECT DEPLOYMENT\n\n")
    
    cat("Step 1: Set Environment Variables in Posit Connect\n")
    cat("1. Go to your app dashboard\n")
    cat("2. Click the 'Settings' tab\n")
    cat("3. Scroll to 'Environment Variables' section\n")
    cat("4. Add these required variables:\n\n")
    cat("   DB_HOST=mexico.bbfarm.org\n")
    cat("   DB_NAME=chapmjs_rqdadb\n")
    cat("   DB_USER=your_mysql_username\n")
    cat("   DB_PASS=your_mysql_password\n")
    cat("   DB_PORT=3306\n\n")
    cat("5. Click 'Save' and restart your application\n\n")
    
    cat("Step 2: Database Setup\n")
    cat("Ensure the database 'chapmjs_rqdadb' exists on mexico.bbfarm.org\n")
    cat("The application will automatically create tables if they don't exist\n\n")
    
    cat("Step 3: Test Deployment\n")
    cat("The app will validate configuration on startup\n")
    cat("Check the logs for any configuration errors\n\n")
    
  } else {
    cat("💻 LOCAL DEVELOPMENT SETUP\n\n")
    
    cat("Step 1: Environment Variables\n")
    cat("Create a .Renviron file in your project root:\n\n")
    cat("# Database Configuration\n")
    cat("DB_HOST=mexico.bbfarm.org\n")
    cat("DB_NAME=chapmjs_rqdadb\n")
    cat("DB_USER=your_username\n")
    cat("DB_PASS=your_password\n")
    cat("DB_PORT=3306\n\n")
    
    cat("Step 2: Install Required Packages\n")
    cat("Run: source('packages.R')\n\n")
    
    cat("Step 3: Restart R Session\n")
    cat("Restart R to load the new environment variables\n\n")
    
    cat("Step 4: Validation\n")
    cat("Run: validate_environment() and test_database_connection()\n\n")
    
    cat("Step 5: Start Application\n")
    cat("Run: shiny::runApp()\n\n")
  }
}

# Environment file generator for local development only
generate_env_file <- function(file_path = ".Renviron") {
  # Check if running on Posit Connect
  is_posit_connect <- !is.na(Sys.getenv("R_CONFIG_ACTIVE", NA)) || 
                     !is.na(Sys.getenv("CONNECT_SERVER", NA))
  
  if (is_posit_connect) {
    cat("⚠️  Running on Posit Connect - environment variables should be set in app settings, not .Renviron file\n")
    cat("Use the Posit Connect dashboard to manage environment variables.\n")
    return(FALSE)
  }
  
  env_content <- "# RQDA Online Environment Configuration
# Generated on: %s
# WARNING: Keep this file secure and do not commit to version control
# NOTE: For Posit Connect deployment, set these as environment variables in app settings

# Database Configuration
DB_HOST=mexico.bbfarm.org
DB_NAME=chapmjs_rqdadb
DB_USER=your_mysql_username
DB_PASS=your_mysql_password
DB_PORT=3306

# Optional Configuration
# SHINY_LOG_LEVEL=INFO
# APP_ENV=development
"
  
  formatted_content <- sprintf(env_content, Sys.time())
  
  if (file.exists(file_path)) {
    cat("Warning:", file_path, "already exists. Backup created.\n")
    file.copy(file_path, paste0(file_path, ".backup"))
  }
  
  writeLines(formatted_content, file_path)
  cat("Environment file created:", file_path, "\n")
  cat("Please edit it with your actual database credentials.\n")
  cat("Remember to restart your R session after editing .Renviron\n")
  
  return(TRUE)
}

# Helper operator for string repetition
`%R%` <- function(x, n) {
  paste(rep(x, n), collapse = "")
}

# Main validation function to run at app startup
startup_validation <- function() {
  cat("Starting RQDA Online Application...\n\n")
  
  # Validate environment
  if (!validate_environment()) {
    stop("Environment validation failed. Please check your configuration.")
  }
  
  cat("\n")
  
  # Test database connection and create tables
  if (!test_database_connection()) {
    stop("Database connection failed or schema setup incomplete. Please check your database configuration.")
  }
  
  cat("\n✅ Startup validation complete - ready to launch!\n")
  cat("=" %R% 50, "\n\n")
}

# Check if running in different environments
get_deployment_environment <- function() {
  if (!is.na(Sys.getenv("R_CONFIG_ACTIVE", NA)) || 
      !is.na(Sys.getenv("CONNECT_SERVER", NA)) ||
      !is.na(Sys.getenv("RSTUDIO_PRODUCT", NA))) {
    return("posit_connect")
  } else if (!is.na(Sys.getenv("SHINYAPPS_USER", NA))) {
    return("shinyapps_io")
  } else if (!is.na(Sys.getenv("AWS_EXECUTION_ENV", NA))) {
    return("aws")
  } else {
    return("local")
  }
}

# Setup helper for new installations
setup_helper <- function() {
  cat("RQDA Online Setup Helper\n")
  cat("========================\n\n")
  
  cat("Step 1: Environment Variables\n")
  cat("For Local Development - Copy this template to your .Renviron file:\n\n")
  cat("# Database Configuration\n")
  cat("DB_HOST=mexico.bbfarm.org\n")
  cat("DB_NAME=chapmjs_rqdadb\n")
  cat("DB_USER=your_username\n")
  cat("DB_PASS=your_password\n")
  cat("DB_PORT=3306\n\n")
  
  cat("For Posit Connect - Set these environment variables in your app settings:\n")
  cat("DB_HOST, DB_NAME, DB_USER, DB_PASS, DB_PORT\n\n")
  
  cat("Step 2: Install Required Packages\n")
  cat("Run: source('packages.R')\n\n")
  
  cat("Step 3: Database Setup\n")
  cat("The application will automatically create tables if they don't exist\n")
  cat("Just ensure the database 'chapmjs_rqdadb' exists on your MySQL server\n\n")
  
  cat("Step 4: Validation\n")
  cat("Run: validate_environment() and test_database_connection()\n\n")
  
  cat("Step 5: Start Application\n")
  cat("Run: shiny::runApp()\n\n")
}

# Environment file generator for local development
generate_env_file <- function(file_path = ".Renviron") {
  env_content <- "# RQDA Online Environment Configuration
# Generated on: %s
# WARNING: Keep this file secure and do not commit to version control
# NOTE: For Posit Connect deployment, set these as environment variables in app settings

# Database Configuration
DB_HOST=mexico.bbfarm.org
DB_NAME=chapmjs_rqdadb
DB_USER=your_mysql_username
DB_PASS=your_mysql_password
DB_PORT=3306

# Optional Configuration
# SHINY_LOG_LEVEL=INFO
# APP_ENV=development
"
  
  formatted_content <- sprintf(env_content, Sys.time())
  
  if (file.exists(file_path)) {
    cat("Warning:", file_path, "already exists. Backup created.\n")
    file.copy(file_path, paste0(file_path, ".backup"))
  }
  
  writeLines(formatted_content, file_path)
  cat("Environment file created:", file_path, "\n")
  cat("Please edit it with your actual database credentials.\n")
}

# Helper operator for string repetition
`%R%` <- function(x, n) {
  paste(rep(x, n), collapse = "")
}

# Main validation function to run at app startup
startup_validation <- function() {
  cat("Starting RQDA Online Application...\n\n")
  
  # Validate environment
  if (!validate_environment()) {
    stop("Environment validation failed. Please check your configuration.")
  }
  
  cat("\n")
  
  # Test database connection and create tables
  if (!test_database_connection()) {
    stop("Database connection failed or schema setup incomplete. Please check your database configuration.")
  }
  
  cat("\n✅ Startup validation complete - ready to launch!\n")
  cat("=" %R% 50, "\n\n")
}
