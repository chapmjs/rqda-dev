# config.R — tiny, strict config reader that fails fast

get_db_config <- function() {
  cfg <- list(
    host     = Sys.getenv("DB_HOST", ""),
    dbname   = Sys.getenv("DB_NAME", ""),
    username = Sys.getenv("DB_USER", ""),
    password = Sys.getenv("DB_PASS", ""),
    port     = as.integer(Sys.getenv("DB_PORT", "3306"))
  )

  required <- c("host", "dbname", "username", "password")
  missing <- required[vapply(required, function(k) !nzchar(cfg[[k]]), logical(1))]
  if (length(missing)) {
    stop(
      sprintf("Missing env: %s",
              paste(sprintf("DB_%s", toupper(missing)), collapse = ", ")),
      call. = FALSE
    )
  }

  cfg
}
