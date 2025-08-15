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
  list(
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT", "3306"))
  )
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
    return(FALSE)
  })
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
