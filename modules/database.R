# modules/database.R — single connection path; prefer SSL, fall back cleanly

suppressPackageStartupMessages({
  library(DBI)
  library(pool)
  library(RMySQL)
})

# Expect logging + helpers to be sourced by app.R:
# - log_info/log_warn/log_error from modules/logging.R
# - db_get/db_exec from modules/db_helpers.R (optional here)

db_connect_pool <- function(config) {
  # Presence is already enforced by get_db_config(); keep a light assert:
  stopifnot(nzchar(config$host), nzchar(config$dbname), nzchar(config$username))

  base_params <- list(
    drv      = RMySQL::MySQL(),
    host     = config$host,
    dbname   = config$dbname,
    username = config$username,
    password = config$password,
    port     = ifelse(is.null(config$port), 3306L, as.integer(config$port)),
    encoding = "utf8mb4",
    minSize  = 1L,
    maxSize  = 10L
  )

  # 1) Try SSL first (REQUIRED)
  try({
    ssl_params <- base_params
    ssl_params$ssl.mode <- "REQUIRED"

    log_info("Attempting SSL DB connection ...")
    p <- do.call(pool::dbPool, ssl_params)

    ssl_check <- try(DBI::dbGetQuery(p, "SHOW STATUS LIKE 'Ssl_cipher'"), silent = TRUE)
    if (inherits(ssl_check, "try-error") || !nrow(ssl_check) || !nzchar(ssl_check$Value[1])) {
      pool::poolClose(p)
      stop("SSL not active; will try non-SSL.")
    }

    log_info("SSL connection established (cipher: ", ssl_check$Value[1], ")")
    return(p)
  }, silent = TRUE)

  # 2) Fallback to non-SSL
  log_warn("SSL unavailable; attempting non-SSL DB connection ...")
  tryCatch({
    p <- do.call(pool::dbPool, base_params)
    log_info("Non-SSL connection established")
    p
  }, error = function(e) {
    log_error("All DB connection attempts failed: ", conditionMessage(e))
    stop("All DB connection attempts failed: ", conditionMessage(e), call. = FALSE)
  })
}

db_disconnect_pool <- function(pool) {
  if (inherits(pool, "Pool")) pool::poolClose(pool)
}
