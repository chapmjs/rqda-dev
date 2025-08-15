# modules/logging.R
# Minimal logger for Shiny apps. Controlled by SHINY_LOG_LEVEL = ERROR|WARN|INFO|DEBUG|TRACE
.level_map <- c(ERROR=1L, WARN=2L, INFO=3L, DEBUG=4L, TRACE=5L)

get_log_level <- function() {
  lvl <- toupper(Sys.getenv("SHINY_LOG_LEVEL", "INFO"))
  if (!lvl %in% names(.level_map)) lvl <- "INFO"
  lvl
}

.should_log <- function(level) {
  .level_map[[level]] <= .level_map[[get_log_level()]]
}

.log <- function(level, ...) {
  if (.should_log(level)) {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(ts, sprintf("[%s]", level), ..., "\n")
  }
}

log_error <- function(...) .log("ERROR", ...)
log_warn  <- function(...) .log("WARN",  ...)
log_info  <- function(...) .log("INFO",  ...)
log_debug <- function(...) .log("DEBUG", ...)
log_trace <- function(...) .log("TRACE", ...)
