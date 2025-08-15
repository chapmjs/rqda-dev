# modules/database.R
# Database connection and utility functions

library(DBI)
library(RMySQL)
library(pool)
library(glue)

# Create database connection pool
db_connect_pool <- function(config) {
  # Validate required environment variables
  required_vars <- c("host", "dbname", "username", "password")
  missing_vars <- required_vars[sapply(required_vars, function(x) is.null(config[[x]]) || config[[x]] == "")]
  
  if (length(missing_vars) > 0) {
    stop(paste("Missing required database configuration:", paste(missing_vars, collapse = ", ")))
  }
  
  pool::dbPool(
    drv = RMySQL::MySQL(),
    host = config$host,
    dbname = config$dbname,
    username = config$username,
    password = config$password,
    port = ifelse(is.null(config$port), 3306, config$port),
    encoding = "utf8mb4",
    # Connection pool settings
    minSize = 1,
    maxSize = 10
  )
}

# Create all required tables if they don't exist
create_tables_if_not_exist <- function(pool) {
  cat("Checking and creating database tables...\n")
  
  # Define all table creation statements
  table_statements <- list(
    project = "
      CREATE TABLE IF NOT EXISTS project (
        id INT AUTO_INCREMENT PRIMARY KEY,
        databaseversion VARCHAR(20) DEFAULT '0.2.2',
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        memo TEXT,
        about TEXT,
        owner VARCHAR(255),
        status TINYINT DEFAULT 1,
        project_name VARCHAR(255) NOT NULL,
        created_by VARCHAR(255),
        INDEX idx_owner (owner),
        INDEX idx_status (status)
      )",
    
    users = "
      CREATE TABLE IF NOT EXISTS users (
        id INT AUTO_INCREMENT PRIMARY KEY,
        username VARCHAR(255) UNIQUE NOT NULL,
        email VARCHAR(255) UNIQUE NOT NULL,
        password_hash VARCHAR(255) NOT NULL,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        last_login DATETIME,
        active TINYINT DEFAULT 1,
        INDEX idx_username (username),
        INDEX idx_email (email)
      )",
    
    source = "
      CREATE TABLE IF NOT EXISTS source (
        id INT AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(255) NOT NULL,
        file LONGTEXT NOT NULL,
        memo TEXT,
        owner VARCHAR(255),
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        status TINYINT DEFAULT 1,
        project_id INT,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status),
        INDEX idx_owner (owner),
        FULLTEXT(file, name)
      )",
    
    filecat = "
      CREATE TABLE IF NOT EXISTS filecat (
        catid INT AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(255) NOT NULL,
        memo TEXT,
        owner VARCHAR(255),
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        status TINYINT DEFAULT 1,
        project_id INT,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    freecode = "
      CREATE TABLE IF NOT EXISTS freecode (
        id INT AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(255) NOT NULL,
        memo TEXT,
        owner VARCHAR(255),
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        status TINYINT DEFAULT 1,
        color VARCHAR(7) DEFAULT '#FFFF00',
        project_id INT,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status),
        INDEX idx_name (name)
      )",
    
    codecat = "
      CREATE TABLE IF NOT EXISTS codecat (
        catid INT AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(255) NOT NULL,
        memo TEXT,
        owner VARCHAR(255),
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        status TINYINT DEFAULT 1,
        project_id INT,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    coding = "
      CREATE TABLE IF NOT EXISTS coding (
        rowid INT AUTO_INCREMENT PRIMARY KEY,
        cid INT NOT NULL,
        fid INT NOT NULL,
        seltext TEXT,
        selfirst INT,
        selend INT,
        status TINYINT DEFAULT 1,
        owner VARCHAR(255),
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        memo TEXT,
        project_id INT,
        FOREIGN KEY (cid) REFERENCES freecode(id) ON DELETE CASCADE,
        FOREIGN KEY (fid) REFERENCES source(id) ON DELETE CASCADE,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_cid_fid (cid, fid),
        INDEX idx_project_status (project_id, status),
        INDEX idx_position (fid, selfirst, selend),
        FULLTEXT(seltext)
      )",
    
    treecode = "
      CREATE TABLE IF NOT EXISTS treecode (
        cid INT,
        catid INT,
        memo TEXT,
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        owner VARCHAR(255),
        status TINYINT DEFAULT 1,
        project_id INT,
        PRIMARY KEY (cid, catid),
        FOREIGN KEY (cid) REFERENCES freecode(id) ON DELETE CASCADE,
        FOREIGN KEY (catid) REFERENCES codecat(catid) ON DELETE CASCADE,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    treefile = "
      CREATE TABLE IF NOT EXISTS treefile (
        fid INT,
        catid INT,
        memo TEXT,
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        owner VARCHAR(255),
        status TINYINT DEFAULT 1,
        project_id INT,
        PRIMARY KEY (fid, catid),
        FOREIGN KEY (fid) REFERENCES source(id) ON DELETE CASCADE,
        FOREIGN KEY (catid) REFERENCES filecat(catid) ON DELETE CASCADE,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    annotation = "
      CREATE TABLE IF NOT EXISTS annotation (
        rowid INT AUTO_INCREMENT PRIMARY KEY,
        fid INT NOT NULL,
        position INT,
        annotation TEXT,
        owner VARCHAR(255),
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        status TINYINT DEFAULT 1,
        project_id INT,
        FOREIGN KEY (fid) REFERENCES source(id) ON DELETE CASCADE,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_fid_position (fid, position),
        INDEX idx_project_status (project_id, status)
      )",
    
    cases = "
      CREATE TABLE IF NOT EXISTS cases (
        id INT AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(255) NOT NULL,
        memo TEXT,
        owner VARCHAR(255),
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        status TINYINT DEFAULT 1,
        project_id INT,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    caseAttr = "
      CREATE TABLE IF NOT EXISTS caseAttr (
        variable VARCHAR(255),
        value TEXT,
        caseId INT,
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        owner VARCHAR(255),
        status TINYINT DEFAULT 1,
        project_id INT,
        PRIMARY KEY (variable, caseId),
        FOREIGN KEY (caseId) REFERENCES cases(id) ON DELETE CASCADE,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    caselinkage = "
      CREATE TABLE IF NOT EXISTS caselinkage (
        caseid INT,
        fid INT,
        owner VARCHAR(255),
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        status TINYINT DEFAULT 1,
        project_id INT,
        PRIMARY KEY (caseid, fid),
        FOREIGN KEY (caseid) REFERENCES cases(id) ON DELETE CASCADE,
        FOREIGN KEY (fid) REFERENCES source(id) ON DELETE CASCADE,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    attributes = "
      CREATE TABLE IF NOT EXISTS attributes (
        name VARCHAR(255) PRIMARY KEY,
        status TINYINT DEFAULT 1,
        memo TEXT,
        owner VARCHAR(255),
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        class VARCHAR(50) DEFAULT 'character',
        project_id INT,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    fileAttr = "
      CREATE TABLE IF NOT EXISTS fileAttr (
        variable VARCHAR(255),
        value TEXT,
        fid INT,
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        owner VARCHAR(255),
        status TINYINT DEFAULT 1,
        project_id INT,
        PRIMARY KEY (variable, fid),
        FOREIGN KEY (fid) REFERENCES source(id) ON DELETE CASCADE,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    image = "
      CREATE TABLE IF NOT EXISTS image (
        id INT AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(255),
        imagepath TEXT,
        memo TEXT,
        owner VARCHAR(255),
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        status TINYINT DEFAULT 1,
        project_id INT,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    imageCoding = "
      CREATE TABLE IF NOT EXISTS imageCoding (
        cid INT,
        iid INT,
        x1 INT,
        y1 INT,
        x2 INT,
        y2 INT,
        memo TEXT,
        date DATETIME DEFAULT CURRENT_TIMESTAMP,
        dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        owner VARCHAR(255),
        status TINYINT DEFAULT 1,
        project_id INT,
        FOREIGN KEY (cid) REFERENCES freecode(id) ON DELETE CASCADE,
        FOREIGN KEY (iid) REFERENCES image(id) ON DELETE CASCADE,
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        INDEX idx_project_status (project_id, status)
      )",
    
    project_permissions = "
      CREATE TABLE IF NOT EXISTS project_permissions (
        project_id INT,
        user_id INT,
        permission_level ENUM('owner', 'editor', 'viewer') DEFAULT 'viewer',
        granted_by INT,
        granted_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        PRIMARY KEY (project_id, user_id),
        FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
        FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
        FOREIGN KEY (granted_by) REFERENCES users(id)
      )"
  )
  
  created_tables <- c()
  failed_tables <- c()
  
  # Create tables in dependency order (core tables first)
  table_order <- c("users", "project", "source", "filecat", "freecode", "codecat", 
                   "coding", "treecode", "treefile", "annotation", "cases", 
                   "caseAttr", "caselinkage", "attributes", "fileAttr", 
                   "image", "imageCoding", "project_permissions")
  
  for (table_name in table_order) {
    if (table_name %in% names(table_statements)) {
      tryCatch({
        pool::dbExecute(pool, table_statements[[table_name]])
        cat(sprintf("✓ Table '%s' ready\n", table_name))
        created_tables <- c(created_tables, table_name)
      }, error = function(e) {
        cat(sprintf("✗ Failed to create table '%s': %s\n", table_name, e$message))
        failed_tables <- c(failed_tables, table_name)
      })
    }
  }
  
  # Summary
  cat(sprintf("\nTable creation summary: %d/%d successful\n", 
              length(created_tables), length(table_order)))
  
  if (length(failed_tables) > 0) {
    warning(paste("Failed to create tables:", paste(failed_tables, collapse = ", ")))
    return(FALSE)
  }
  
  return(TRUE)
}

# Execute query with error handling
db_execute_query <- function(pool, query, params = list()) {
  tryCatch({
    if (length(params) > 0) {
      result <- pool::dbGetQuery(pool, query, params = params)
    } else {
      result <- pool::dbGetQuery(pool, query)
    }
    return(result)
  }, error = function(e) {
    warning(paste("Database query error:", e$message))
    return(NULL)
  })
}

# Safe SQL query execution (equivalent to RQDAQuery)
rqda_query <- function(pool, sql, params = list()) {
  db_execute_query(pool, sql, params)
}

# Get next available ID for a table
get_next_id <- function(pool, table_name, id_column = "id") {
  query <- glue("SELECT COALESCE(MAX({id_column}), 0) + 1 AS next_id FROM {table_name}")
  result <- db_execute_query(pool, query)
  if (!is.null(result) && nrow(result) > 0) {
    return(result$next_id[1])
  }
  return(1)
}

# Check if user has permission for project
check_project_permission <- function(pool, user_id, project_id, required_level = "viewer") {
  if (is.null(user_id) || is.null(project_id)) return(FALSE)
  
  query <- "
    SELECT pp.permission_level 
    FROM project_permissions pp 
    WHERE pp.user_id = ? AND pp.project_id = ?
    UNION
    SELECT 'owner' as permission_level
    FROM project p 
    WHERE p.created_by = ? AND p.id = ?
  "
  
  result <- db_execute_query(pool, query, list(user_id, project_id, user_id, project_id))
  
  if (is.null(result) || nrow(result) == 0) return(FALSE)
  
  permission_levels <- c("viewer" = 1, "editor" = 2, "owner" = 3)
  user_level <- max(permission_levels[result$permission_level], na.rm = TRUE)
  required_level_num <- permission_levels[required_level]
  
  return(user_level >= required_level_num)
}

# Get user's projects
get_user_projects <- function(pool, user_id) {
  query <- "
    SELECT DISTINCT p.id, p.project_name, p.date, p.owner, 
           COALESCE(pp.permission_level, 'owner') as permission_level,
           (SELECT COUNT(*) FROM source WHERE project_id = p.id AND status = 1) as file_count,
           (SELECT COUNT(*) FROM coding WHERE project_id = p.id AND status = 1) as coding_count
    FROM project p
    LEFT JOIN project_permissions pp ON p.id = pp.project_id AND pp.user_id = ?
    WHERE p.created_by = ? OR pp.user_id = ?
    ORDER BY p.date DESC
  "
  
  db_execute_query(pool, query, list(user_id, user_id, user_id))
}

# Create new project
create_project <- function(pool, project_name, user_id, owner_name) {
  # Check if project name already exists for this user
  check_query <- "
    SELECT COUNT(*) as count 
    FROM project p 
    LEFT JOIN project_permissions pp ON p.id = pp.project_id 
    WHERE p.project_name = ? AND (p.created_by = ? OR pp.user_id = ?)
  "
  
  existing <- db_execute_query(pool, check_query, list(project_name, user_id, user_id))
  
  if (!is.null(existing) && existing$count[1] > 0) {
    return(list(success = FALSE, message = "Project name already exists"))
  }
  
  # Create new project
  insert_query <- "
    INSERT INTO project (project_name, owner, created_by, date, status) 
    VALUES (?, ?, ?, NOW(), 1)
  "
  
  tryCatch({
    result <- pool::dbExecute(pool, insert_query, list(project_name, owner_name, user_id))
    
    # Get the new project ID
    project_id <- pool::dbGetQuery(pool, "SELECT LAST_INSERT_ID() as id")$id[1]
    
    return(list(success = TRUE, project_id = project_id, message = "Project created successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error creating project:", e$message)))
  })
}

# Get project files
get_project_files <- function(pool, project_id, user_id) {
  if (!check_project_permission(pool, user_id, project_id)) {
    return(NULL)
  }
  
  query <- "
    SELECT id, name, owner, date, 
           CHAR_LENGTH(file) as file_size,
           (SELECT COUNT(*) FROM coding WHERE fid = source.id AND status = 1) as coding_count
    FROM source 
    WHERE project_id = ? AND status = 1 
    ORDER BY date DESC
  "
  
  db_execute_query(pool, query, list(project_id))
}

# Get file content
get_file_content <- function(pool, file_id, user_id, project_id) {
  if (!check_project_permission(pool, user_id, project_id)) {
    return(NULL)
  }
  
  query <- "SELECT id, name, file, memo FROM source WHERE id = ? AND project_id = ? AND status = 1"
  db_execute_query(pool, query, list(file_id, project_id))
}

# Get project codes
get_project_codes <- function(pool, project_id, user_id) {
  if (!check_project_permission(pool, user_id, project_id)) {
    return(NULL)
  }
  
  query <- "
    SELECT f.id, f.name, f.color, f.memo, f.owner, f.date,
           (SELECT COUNT(*) FROM coding c WHERE c.cid = f.id AND c.status = 1) as usage_count,
           cc.name as category_name
    FROM freecode f
    LEFT JOIN treecode tc ON f.id = tc.cid AND tc.status = 1
    LEFT JOIN codecat cc ON tc.catid = cc.catid AND cc.status = 1
    WHERE f.project_id = ? AND f.status = 1
    ORDER BY f.name
  "
  
  db_execute_query(pool, query, list(project_id))
}

# Add new code
add_code <- function(pool, code_name, project_id, user_id, owner_name, color = "#FFFF00", memo = "") {
  if (!check_project_permission(pool, user_id, project_id, "editor")) {
    return(list(success = FALSE, message = "Insufficient permissions"))
  }
  
  # Check if code name already exists in project
  check_query <- "SELECT COUNT(*) as count FROM freecode WHERE name = ? AND project_id = ? AND status = 1"
  existing <- db_execute_query(pool, check_query, list(code_name, project_id))
  
  if (!is.null(existing) && existing$count[1] > 0) {
    return(list(success = FALSE, message = "Code name already exists"))
  }
  
  # Insert new code
  insert_query <- "
    INSERT INTO freecode (name, project_id, owner, color, memo, date, status) 
    VALUES (?, ?, ?, ?, ?, NOW(), 1)
  "
  
  tryCatch({
    result <- pool::dbExecute(pool, insert_query, list(code_name, project_id, owner_name, color, memo))
    code_id <- pool::dbGetQuery(pool, "SELECT LAST_INSERT_ID() as id")$id[1]
    
    return(list(success = TRUE, code_id = code_id, message = "Code created successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error creating code:", e$message)))
  })
}

# Get codings for a file
get_file_codings <- function(pool, file_id, project_id, user_id) {
  if (!check_project_permission(pool, user_id, project_id)) {
    return(NULL)
  }
  
  query <- "
    SELECT c.rowid, c.cid, c.seltext, c.selfirst, c.selend, c.memo as coding_memo,
           f.name as code_name, f.color, c.owner, c.date
    FROM coding c
    JOIN freecode f ON c.cid = f.id
    WHERE c.fid = ? AND c.project_id = ? AND c.status = 1
    ORDER BY c.selfirst
  "
  
  db_execute_query(pool, query, list(file_id, project_id))
}

# Add new coding
add_coding <- function(pool, code_id, file_id, selected_text, start_pos, end_pos, 
                      project_id, user_id, owner_name, memo = "") {
  if (!check_project_permission(pool, user_id, project_id, "editor")) {
    return(list(success = FALSE, message = "Insufficient permissions"))
  }
  
  insert_query <- "
    INSERT INTO coding (cid, fid, seltext, selfirst, selend, project_id, owner, memo, date, status) 
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, NOW(), 1)
  "
  
  tryCatch({
    result <- pool::dbExecute(pool, insert_query, 
                             list(code_id, file_id, selected_text, start_pos, end_pos, 
                                  project_id, owner_name, memo))
    coding_id <- pool::dbGetQuery(pool, "SELECT LAST_INSERT_ID() as id")$id[1]
    
    return(list(success = TRUE, coding_id = coding_id, message = "Coding added successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error adding coding:", e$message)))
  })
}

# Search and auto-code function
auto_code_search <- function(pool, search_term, code_id, project_id, user_id, owner_name, 
                            file_ids = NULL, case_sensitive = FALSE) {
  if (!check_project_permission(pool, user_id, project_id, "editor")) {
    return(list(success = FALSE, message = "Insufficient permissions"))
  }
  
  # Build file filter
  file_filter <- ""
  params <- list(project_id)
  
  if (!is.null(file_ids) && length(file_ids) > 0) {
    file_filter <- paste("AND id IN (", paste(rep("?", length(file_ids)), collapse = ","), ")")
    params <- c(params, file_ids)
  }
  
  # Get files to search
  files_query <- glue("SELECT id, file FROM source WHERE project_id = ? AND status = 1 {file_filter}")
  files <- db_execute_query(pool, files_query, params)
  
  if (is.null(files) || nrow(files) == 0) {
    return(list(success = FALSE, message = "No files found"))
  }
  
  codings_added <- 0
  
  for (i in 1:nrow(files)) {
    file_content <- files$file[i]
    file_id <- files$id[i]
    
    # Perform search
    if (case_sensitive) {
      matches <- gregexpr(search_term, file_content, perl = TRUE)
    } else {
      matches <- gregexpr(search_term, file_content, ignore.case = TRUE, perl = TRUE)
    }
    
    if (matches[[1]][1] != -1) {
      for (match_start in matches[[1]]) {
        match_end <- match_start + attr(matches[[1]], "match.length")[which(matches[[1]] == match_start)] - 1
        selected_text <- substr(file_content, match_start, match_end)
        
        # Check for overlapping codings
        overlap_query <- "
          SELECT COUNT(*) as count FROM coding 
          WHERE fid = ? AND status = 1 AND 
                ((selfirst <= ? AND selend >= ?) OR 
                 (selfirst <= ? AND selend >= ?) OR
                 (selfirst >= ? AND selend <= ?))
        "
        
        overlap_result <- db_execute_query(pool, overlap_query, 
                                         list(file_id, match_start, match_start, 
                                              match_end, match_end, match_start, match_end))
        
        # Only add if no overlap
        if (is.null(overlap_result) || overlap_result$count[1] == 0) {
          result <- add_coding(pool, code_id, file_id, selected_text, match_start, match_end,
                              project_id, user_id, owner_name, "Auto-coded")
          if (result$success) {
            codings_added <- codings_added + 1
          }
        }
      }
    }
  }
  
  return(list(success = TRUE, message = paste("Added", codings_added, "new codings")))
}

# Export project data
export_project_data <- function(pool, project_id, user_id, format = "csv") {
  if (!check_project_permission(pool, user_id, project_id)) {
    return(NULL)
  }
  
  # Main export query - get all codings with related information
  query <- "
    SELECT 
      p.project_name,
      s.name as file_name,
      f.name as code_name,
      cc.name as code_category,
      c.seltext as coded_text,
      c.selfirst as start_position,
      c.selend as end_position,
      c.memo as coding_memo,
      c.owner as coded_by,
      c.date as coding_date,
      s.memo as file_memo,
      f.memo as code_memo
    FROM coding c
    JOIN source s ON c.fid = s.id
    JOIN freecode f ON c.cid = f.id
    JOIN project p ON c.project_id = p.id
    LEFT JOIN treecode tc ON f.id = tc.cid AND tc.status = 1
    LEFT JOIN codecat cc ON tc.catid = cc.catid AND cc.status = 1
    WHERE c.project_id = ? AND c.status = 1
    ORDER BY s.name, c.selfirst
  "
  
  db_execute_query(pool, query, list(project_id))
}

# Database health check
check_database_connection <- function(pool) {
  tryCatch({
    result <- pool::dbGetQuery(pool, "SELECT 1 as test")
    return(list(success = TRUE, message = "Database connection OK"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Database connection failed:", e$message)))
  })
}
