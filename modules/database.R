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
