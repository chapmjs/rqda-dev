# packages.R - Install required packages for RQDA Online

cat("Installing RQDA Online Dependencies...\n")
cat("=====================================\n")

# Required packages
required_packages <- c(
  # Core Shiny packages
  "shiny",
  "shinydashboard", 
  "shinyjs",
  "shinyWidgets",
  "shinycssloaders",
  
  # Database packages
  "DBI",
  "RMySQL",
  "pool",
  
  # Data manipulation
  "dplyr",
  "stringr",
  "glue",
  
  # UI components
  "DT",
  "plotly",
  "htmltools",
  
  # Security
  "digest",
  
  # Development and deployment
  "rsconnect"
)

# Function to install packages with progress feedback
install_with_feedback <- function(packages) {
  total <- length(packages)
  installed_count <- 0
  failed_packages <- c()
  
  for (i in seq_along(packages)) {
    package <- packages[i]
    cat(sprintf("[%d/%d] Installing %s...", i, total, package))
    
    tryCatch({
      if (!require(package, character.only = TRUE, quietly = TRUE)) {
        install.packages(package, quiet = TRUE)
        library(package, character.only = TRUE)
      }
      cat(" ✓\n")
      installed_count <- installed_count + 1
    }, error = function(e) {
      cat(" ✗\n")
      cat(sprintf("   Error: %s\n", e$message))
      failed_packages <<- c(failed_packages, package)
    })
  }
  
  cat("\n")
  cat("Installation Summary:\n")
  cat("====================\n")
  cat(sprintf("Successfully installed: %d/%d packages\n", installed_count, total))
  
  if (length(failed_packages) > 0) {
    cat("Failed packages:", paste(failed_packages, collapse = ", "), "\n")
    cat("\nTroubleshooting failed packages:\n")
    cat("1. Check your internet connection\n")
    cat("2. Try installing manually: install.packages('package_name')\n")
    cat("3. Some packages may require system libraries\n")
    return(FALSE)
  } else {
    cat("✅ All packages installed successfully!\n")
    return(TRUE)
  }
}

# Check if running on Posit Connect
is_posit_connect <- function() {
  !is.na(Sys.getenv("R_CONFIG_ACTIVE", NA)) || 
  !is.na(Sys.getenv("CONNECT_SERVER", NA))
}

# Main installation
if (is_posit_connect()) {
  cat("Running on Posit Connect - packages should be pre-installed\n")
  cat("Checking package availability...\n")
  
  missing_packages <- c()
  for (package in required_packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      missing_packages <- c(missing_packages, package)
    }
  }
  
  if (length(missing_packages) > 0) {
    cat("Missing packages on Posit Connect:", paste(missing_packages, collapse = ", "), "\n")
    cat("These should be included in your deployment manifest\n")
  } else {
    cat("✅ All required packages are available\n")
  }
  
} else {
  cat("Installing packages locally...\n\n")
  
  # Update package repository
  options(repos = c(CRAN = "https://cloud.r-project.org/"))
  
  # Install packages
  success <- install_with_feedback(required_packages)
  
  if (success) {
    cat("\n🎉 Package installation complete!\n")
    cat("Next steps:\n")
    cat("1. Set up your .Renviron file with database credentials\n")
    cat("2. Run validate_environment() to check configuration\n")
    cat("3. Run test_database_connection() to verify database access\n")
    cat("4. Start the app with shiny::runApp()\n")
  } else {
    cat("\n❌ Some packages failed to install\n")
    cat("Please resolve the installation issues before proceeding\n")
  }
}

# System requirements check
check_system_requirements <- function() {
  cat("\nChecking System Requirements:\n")
  cat("============================\n")
  
  # R version
  r_version <- R.version$major
  r_minor <- R.version$minor
  cat(sprintf("R Version: %s.%s", r_version, r_minor))
  
  if (as.numeric(r_version) >= 4) {
    cat(" ✓\n")
  } else {
    cat(" ⚠️  (R 4.0+ recommended)\n")
  }
  
  # Operating System
  os <- Sys.info()["sysname"]
  cat(sprintf("Operating System: %s ✓\n", os))
  
  # MySQL client libraries
  mysql_available <- tryCatch({
    library(RMySQL)
    TRUE
  }, error = function(e) FALSE)
  
  cat("MySQL Support:")
  if (mysql_available) {
    cat(" ✓\n")
  } else {
    cat(" ✗\n")
    cat("  Install MySQL development libraries:\n")
    cat("  - Ubuntu/Debian: sudo apt-get install libmysqlclient-dev\n")
    cat("  - CentOS/RHEL: sudo yum install mysql-devel\n")
    cat("  - macOS: brew install mysql\n")
  }
}

# Run system check
check_system_requirements()

cat(paste(rep("=", 50), collapse = ""), "\n")
cat("Package installation script completed.\n")
