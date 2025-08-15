# modules/db_helpers.R
# Thin DB access layer using DBI + glue::glue_sql with friendly error messages.

handle_db_error <- function(e) {
  msg <- conditionMessage(e)
  # MySQL duplicate key error code is 1062; map to a friendly message.
  if (grepl("\\b1062\\b", msg)) {
    stop("Already exists. Please use a different name.", call. = FALSE)
  }
  # Access denied
  if (grepl("Access denied", msg, ignore.case = TRUE)) {
    stop("Database access denied. Check DB_USER/DB_PASS.", call. = FALSE)
  }
  # Connection issues
  if (grepl("Can't connect|Connection refused|Unknown MySQL server host", msg, ignore.case = TRUE)) {
    stop("Unable to connect to the database host. Verify DB_HOST/DB_PORT and network access.", call. = FALSE)
  }
  stop(e)
}

db_exec <- function(pool, sql, params = list()) {
  tryCatch({
    if (length(params)) {
      sql <- do.call(glue::glue_sql, c(list(sql, .con = pool), params))
    }
    DBI::dbExecute(pool, sql)
  }, error = handle_db_error)
}

db_get <- function(pool, sql, params = list()) {
  tryCatch({
    if (length(params)) {
      sql <- do.call(glue::glue_sql, c(list(sql, .con = pool), params))
    }
    DBI::dbGetQuery(pool, sql)
  }, error = handle_db_error)
}
