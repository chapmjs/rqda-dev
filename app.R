# app.R — lean startup, single connection path, leveled logging

# ---- Packages ----
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(pool)
  library(RMySQL)
})

# ---- Sources ----
source("config.R")                     # get_db_config()
source("modules/logging.R")            # log_info/log_warn/log_error/...
source("modules/db_helpers.R")         # db_get()/db_exec()
source("modules/database.R")           # db_connect_pool()/db_disconnect_pool()

# ---- App Options (tweak as needed) ----
options(shiny.autoreload = TRUE)
options(shiny.maxRequestSize = 50 * 1024^2)  # 50MB uploads default
options(stringsAsFactors = FALSE)

# ---- DB Pool (shared across sessions) ----
DB_CONFIG <- get_db_config()

pool <- NULL
init_pool <- function() {
  if (is.null(pool)) {
    log_info("Initializing DB pool...")
    pool <<- db_connect_pool(DB_CONFIG)
    shiny::onStop(function() {
      log_info("Shutting down DB pool...")
      try(db_disconnect_pool(pool), silent = TRUE)
    })
  }
}
init_pool()

# ---- Optional: simple health check you can surface in the UI ----
db_ssl_cipher <- tryCatch({
  val <- dbGetQuery(pool, "SHOW STATUS LIKE 'Ssl_cipher'")
  if (nrow(val) && nzchar(val$Value[1])) val$Value[1] else ""
}, error = function(e) "")

# ---- UI / Server wiring ----
# If your repo already defines ui/server elsewhere, we’ll use those.
# Otherwise we provide a minimal fallback so the app starts.

if (file.exists("ui.R")) {
  source("ui.R", local = TRUE)
}
if (file.exists("server.R")) {
  source("server.R", local = TRUE)
}

if (!exists("ui")) {
  ui <- fluidPage(
    tags$head(tags$title("RQDA (lean startup)")),
    h2("RQDA — Minimal Shell"),
    tags$p("DB host: ", strong(DB_CONFIG$host)),
    tags$p("DB name: ", strong(DB_CONFIG$dbname)),
    tags$p("SSL cipher: ", if (nzchar(db_ssl_cipher)) db_ssl_cipher else "not active"),
    tags$hr(),
    tags$p("This is a minimal UI. If your project defines its own UI/server in files like ",
           code("ui.R"), " and ", code("server.R"), ", those will be used automatically.")
  )
}

if (!exists("server")) {
  server <- function(input, output, session) {
    # Minimal no-op server; your real app logic can live in server.R/modules
    log_info("Session started: ", session$token)
    onSessionEnded(function() log_info("Session ended: ", session$token))
  }
}

# ---- Run App ----
shinyApp(ui = ui, server = server)
