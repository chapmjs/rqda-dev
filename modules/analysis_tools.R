# modules/analysis_tools.R
# Analysis and visualization tools for qualitative data

library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(stringr)

# Source database functions if not already loaded
if (!exists("db_execute_query")) {
  source("modules/database.R")
}

# Analysis Tools UI
analysisToolsUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Analysis navigation and controls
    box(
      title = "Analysis Tools",
      status = "primary",
      solidHeader = TRUE,
      width = 3,
      
      # Analysis type selection
      selectInput(
        ns("analysis_type"),
        "Analysis Type:",
        choices = list(
          "Code Frequency" = "frequency",
          "Code Co-occurrence" = "cooccurrence", 
          "Text Retrieval" = "retrieval",
          "Search & Query" = "search",
          "Coding Density" = "density",
          "Inter-coder Reliability" = "reliability"
        ),
        selected = "frequency"
      ),
      
      hr(),
      
      # Dynamic filters based on analysis type
      uiOutput(ns("analysis_filters")),
      
      hr(),
      
      # Analysis actions
      actionButton(
        ns("run_analysis"),
        "Run Analysis",
        icon = icon("play"),
        class = "btn-success",
        style = "width: 100%; margin-bottom: 10px;"
      ),
      
      actionButton(
        ns("export_results"),
        "Export Results",
        icon = icon("download"),
        class = "btn-info",
        style = "width: 100%; margin-bottom: 10px;"
      ),
      
      actionButton(
        ns("save_analysis"),
        "Save Analysis",
        icon = icon("save"),
        class = "btn-warning",
        style = "width: 100%;"
      ),
      
      # Quick stats
      div(
        style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
        h6("Project Statistics", style = "margin-bottom: 10px;"),
        div(
          style = "font-size: 0.9em;",
          textOutput(ns("total_codes_stat")),
          textOutput(ns("total_codings_stat")),
          textOutput(ns("total_files_stat")),
          textOutput(ns("coding_density_stat"))
        )
      )
    ),
    
    # Main analysis display area
    box(
      title = "Analysis Results",
      status = "success",
      solidHeader = TRUE,
      width = 9,
      
      # Analysis tabs
      tabsetPanel(
        id = ns("analysis_tabs"),
        
        # Visualization tab
        tabPanel(
          "Visualization",
          br(),
          withSpinner(
            plotlyOutput(ns("analysis_plot"), height = "500px")
          )
        ),
        
        # Data table tab
        tabPanel(
          "Data Table",
          br(),
          fluidRow(
            column(12,
              div(
                style = "margin-bottom: 15px;",
                downloadButton(
                  ns("download_table"),
                  "Download Table",
                  class = "btn-outline-primary btn-sm"
                )
              )
            )
          ),
          withSpinner(
            DT::dataTableOutput(ns("analysis_table"))
          )
        ),
        
        # Summary tab
        tabPanel(
          "Summary",
          br(),
          withSpinner(
            uiOutput(ns("analysis_summary"))
          )
        ),
        
        # Query Builder tab (for advanced search)
        tabPanel(
          "Query Builder",
          br(),
          fluidRow(
            column(6,
              h4("Build Complex Queries"),
              
              selectInput(
                ns("query_field"),
                "Search Field:",
                choices = list(
                  "Code Name" = "code_name",
                  "Coded Text" = "coded_text",
                  "File Name" = "file_name",
                  "Memo Content" = "memo"
                )
              ),
              
              selectInput(
                ns("query_operator"),
                "Operator:",
                choices = list(
                  "Contains" = "LIKE",
                  "Equals" = "=",
                  "Starts with" = "STARTS_WITH",
                  "Ends with" = "ENDS_WITH",
                  "Does not contain" = "NOT_LIKE"
                )
              ),
              
              textInput(
                ns("query_value"),
                "Search Value:",
                placeholder = "Enter search term..."
              ),
              
              checkboxInput(
                ns("query_case_sensitive"),
                "Case sensitive",
                value = FALSE
              )
            ),
            
            column(6,
              h4("Query Conditions"),
              
              div(
                id = ns("query_conditions"),
                style = "border: 1px solid #ddd; padding: 15px; min-height: 200px; background-color: #f9f9f9;",
                p("No conditions added yet", style = "color: #666; font-style: italic;")
              ),
              
              br(),
              
              actionButton(
                ns("add_condition"),
                "Add Condition",
                icon = icon("plus"),
                class = "btn-primary btn-sm"
              ),
              
              actionButton(
                ns("clear_conditions"),
                "Clear All",
                icon = icon("trash"),
                class = "btn-outline-danger btn-sm",
                style = "margin-left: 10px;"
              )
            )
          ),
          
          hr(),
          
          fluidRow(
            column(12,
              actionButton(
                ns("run_query"),
                "Execute Query",
                icon = icon("search"),
                class = "btn-success"
              ),
              
              div(
                style = "margin-top: 15px;",
                verbatimTextOutput(ns("generated_sql"))
              )
            )
          )
        )
      )
    )
  )
}

# Analysis Tools Server
analysisToolsServer <- function(input, output, session, db_pool, user_id, current_project_id) {
  ns <- session$ns
  
  # Reactive values
  values <- reactiveValues(
    analysis_data = NULL,
    project_stats = NULL,
    query_conditions = list(),
    current_analysis = NULL
  )
  
  # Load project statistics
  load_project_stats <- function() {
    if (!is.null(current_project_id()) && !is.null(user_id())) {
      stats_query <- "
        SELECT 
          (SELECT COUNT(DISTINCT id) FROM freecode WHERE project_id = ? AND status = 1) as total_codes,
          (SELECT COUNT(DISTINCT rowid) FROM coding WHERE project_id = ? AND status = 1) as total_codings,
          (SELECT COUNT(DISTINCT id) FROM source WHERE project_id = ? AND status = 1) as total_files,
          (SELECT ROUND(AVG(coding_count), 1) FROM (
            SELECT COUNT(*) as coding_count 
            FROM coding 
            WHERE project_id = ? AND status = 1 
            GROUP BY fid
          ) as file_counts) as avg_codings_per_file
      "
      
      stats <- db_execute_query(db_pool, stats_query, 
                               list(current_project_id(), current_project_id(), 
                                    current_project_id(), current_project_id()))
      values$project_stats <- stats
    }
  }
  
  # Load stats when project changes
  observe({
    load_project_stats()
  })
  
  # Render project statistics
  output$total_codes_stat <- renderText({
    if (!is.null(values$project_stats)) {
      paste("Codes:", values$project_stats$total_codes[1])
    } else {
      "Codes: 0"
    }
  })
  
  output$total_codings_stat <- renderText({
    if (!is.null(values$project_stats)) {
      paste("Codings:", values$project_stats$total_codings[1])
    } else {
      "Codings: 0"
    }
  })
  
  output$total_files_stat <- renderText({
    if (!is.null(values$project_stats)) {
      paste("Files:", values$project_stats$total_files[1])
    } else {
      "Files: 0"
    }
  })
  
  output$coding_density_stat <- renderText({
    if (!is.null(values$project_stats)) {
      density <- ifelse(is.na(values$project_stats$avg_codings_per_file[1]), 0, 
                       values$project_stats$avg_codings_per_file[1])
      paste("Avg per file:", density)
    } else {
      "Avg per file: 0"
    }
  })
  
  # Dynamic filters based on analysis type
  output$analysis_filters <- renderUI({
    switch(input$analysis_type,
      "frequency" = div(
        h6("Frequency Analysis Options"),
        checkboxInput(
          ns("freq_include_categories"),
          "Group by categories",
          value = TRUE
        ),
        sliderInput(
          ns("freq_min_count"),
          "Minimum frequency:",
          min = 1, max = 50, value = 1
        )
      ),
      
      "cooccurrence" = div(
        h6("Co-occurrence Analysis"),
        numericInput(
          ns("cooc_min_frequency"),
          "Minimum co-occurrence:",
          value = 2, min = 1, max = 20
        ),
        checkboxInput(
          ns("cooc_same_segment"),
          "Same text segment only",
          value = FALSE
        ),
        sliderInput(
          ns("cooc_proximity"),
          "Proximity (characters):",
          min = 0, max = 1000, value = 100
        )
      ),
      
      "retrieval" = div(
        h6("Text Retrieval Options"),
        selectInput(
          ns("retrieval_codes"),
          "Select codes:",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          ns("retrieval_files"),
          "Select files:",
          choices = NULL,
          multiple = TRUE
        ),
        checkboxInput(
          ns("retrieval_context"),
          "Include context (±50 chars)",
          value = TRUE
        )
      ),
      
      "search" = div(
        h6("Search Options"),
        textInput(
          ns("search_term"),
          "Search term:",
          placeholder = "Enter search term..."
        ),
        checkboxInput(
          ns("search_codes_only"),
          "Search in codes only",
          value = FALSE
        ),
        checkboxInput(
          ns("search_case_sensitive"),
          "Case sensitive",
          value = FALSE
        )
      ),
      
      "density" = div(
        h6("Coding Density Analysis"),
        selectInput(
          ns("density_visualization"),
          "Visualization type:",
          choices = list(
            "Heatmap by File" = "heatmap",
            "Code Distribution" = "distribution",
            "Timeline" = "timeline"
          )
        ),
        checkboxInput(
          ns("density_normalize"),
          "Normalize by file length",
          value = TRUE
        )
      ),
      
      "reliability" = div(
        h6("Inter-coder Reliability"),
        p("Compare coding consistency between different coders.", 
          style = "font-size: 0.9em; color: #666;"),
        selectInput(
          ns("reliability_coders"),
          "Select coders:",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          ns("reliability_method"),
          "Reliability measure:",
          choices = list(
            "Percent Agreement" = "percent",
            "Cohen's Kappa" = "kappa",
            "Krippendorff's Alpha" = "alpha"
          )
        )
      )
    )
  })
  
  # Update choices for retrieval filters
  observe({
    if (!is.null(current_project_id()) && input$analysis_type == "retrieval") {
      # Get codes
      codes_query <- "SELECT id, name FROM freecode WHERE project_id = ? AND status = 1 ORDER BY name"
      codes <- db_execute_query(db_pool, codes_query, list(current_project_id()))
      
      if (!is.null(codes) && nrow(codes) > 0) {
        code_choices <- setNames(codes$id, codes$name)
        updateSelectInput(session, "retrieval_codes", choices = code_choices)
      }
      
      # Get files
      files_query <- "SELECT id, name FROM source WHERE project_id = ? AND status = 1 ORDER BY name"
      files <- db_execute_query(db_pool, files_query, list(current_project_id()))
      
      if (!is.null(files) && nrow(files) > 0) {
        file_choices <- setNames(files$id, files$name)
        updateSelectInput(session, "retrieval_files", choices = file_choices)
      }
    }
  })
  
  # Update choices for reliability analysis
  observe({
    if (!is.null(current_project_id()) && input$analysis_type == "reliability") {
      # Get unique coders
      coders_query <- "SELECT DISTINCT owner FROM coding WHERE project_id = ? AND status = 1 ORDER BY owner"
      coders <- db_execute_query(db_pool, coders_query, list(current_project_id()))
      
      if (!is.null(coders) && nrow(coders) > 0) {
        coder_choices <- setNames(coders$owner, coders$owner)
        updateSelectInput(session, "reliability_coders", choices = coder_choices)
      }
    }
  })
  
  # Run analysis
  observeEvent(input$run_analysis, {
    if (is.null(current_project_id())) {
      showNotification("Please select a project first", type = "error")
      return()
    }
    
    analysis_result <- switch(input$analysis_type,
      "frequency" = run_frequency_analysis(),
      "cooccurrence" = run_cooccurrence_analysis(),
      "retrieval" = run_retrieval_analysis(),
      "search" = run_search_analysis(),
      "density" = run_density_analysis(),
      "reliability" = run_reliability_analysis()
    )
    
    if (!is.null(analysis_result)) {
      values$analysis_data <- analysis_result
      values$current_analysis <- input$analysis_type
      showNotification("Analysis completed successfully!", type = "message")
    }
  })
  
  # Frequency analysis
  run_frequency_analysis <- function() {
    query <- "
      SELECT 
        f.name as code_name,
        f.color,
        COALESCE(cc.name, 'Uncategorized') as category,
        COUNT(c.rowid) as frequency,
        COUNT(DISTINCT c.fid) as files_coded,
        f.memo
      FROM freecode f
      LEFT JOIN coding c ON f.id = c.cid AND c.status = 1
      LEFT JOIN treecode tc ON f.id = tc.cid AND tc.status = 1
      LEFT JOIN codecat cc ON tc.catid = cc.catid AND cc.status = 1
      WHERE f.project_id = ? AND f.status = 1
      GROUP BY f.id, f.name, f.color, cc.name, f.memo
      HAVING frequency >= ?
      ORDER BY frequency DESC
    "
    
    min_freq <- ifelse(is.null(input$freq_min_count), 1, input$freq_min_count)
    result <- db_execute_query(db_pool, query, list(current_project_id(), min_freq))
    
    return(list(
      data = result,
      type = "frequency",
      title = "Code Frequency Analysis"
    ))
  }
  
  # Co-occurrence analysis
  run_cooccurrence_analysis <- function() {
    min_freq <- ifelse(is.null(input$cooc_min_frequency), 2, input$cooc_min_frequency)
    
    query <- "
      SELECT 
        f1.name as code1,
        f2.name as code2,
        COUNT(*) as cooccurrence_count,
        COUNT(DISTINCT c1.fid) as files_with_cooccurrence
      FROM coding c1
      JOIN coding c2 ON c1.fid = c2.fid AND c1.rowid != c2.rowid
      JOIN freecode f1 ON c1.cid = f1.id
      JOIN freecode f2 ON c2.cid = f2.id
      WHERE c1.project_id = ? AND c1.status = 1 AND c2.status = 1
        AND f1.id < f2.id  -- Avoid duplicate pairs
      GROUP BY f1.id, f2.id, f1.name, f2.name
      HAVING cooccurrence_count >= ?
      ORDER BY cooccurrence_count DESC
    "
    
    result <- db_execute_query(db_pool, query, list(current_project_id(), min_freq))
    
    return(list(
      data = result,
      type = "cooccurrence", 
      title = "Code Co-occurrence Analysis"
    ))
  }
  
  # Text retrieval analysis
  run_retrieval_analysis <- function() {
    if (is.null(input$retrieval_codes) || length(input$retrieval_codes) == 0) {
      showNotification("Please select at least one code", type = "warning")
      return(NULL)
    }
    
    code_ids <- paste(input$retrieval_codes, collapse = ",")
    file_filter <- ""
    params <- list(current_project_id())
    
    if (!is.null(input$retrieval_files) && length(input$retrieval_files) > 0) {
      file_filter <- paste("AND s.id IN (", paste(rep("?", length(input$retrieval_files)), collapse = ","), ")")
      params <- c(params, input$retrieval_files)
    }
    
    query <- paste0("
      SELECT 
        f.name as code_name,
        s.name as file_name,
        c.seltext as coded_text,
        c.selfirst as start_position,
        c.selend as end_position,
        c.owner as coded_by,
        c.date as coding_date,
        c.memo as coding_memo
      FROM coding c
      JOIN freecode f ON c.cid = f.id
      JOIN source s ON c.fid = s.id
      WHERE c.project_id = ? AND c.status = 1 
        AND c.cid IN (", code_ids, ")
        ", file_filter, "
      ORDER BY s.name, c.selfirst
    ")
    
    result <- db_execute_query(db_pool, query, params)
    
    return(list(
      data = result,
      type = "retrieval",
      title = "Text Retrieval Results"
    ))
  }
  
  # Search analysis
  run_search_analysis <- function() {
    if (is.null(input$search_term) || input$search_term == "") {
      showNotification("Please enter a search term", type = "warning")
      return(NULL)
    }
    
    search_term <- input$search_term
    case_modifier <- ifelse(input$search_case_sensitive, "", "LOWER")
    search_value <- ifelse(input$search_case_sensitive, search_term, tolower(search_term))
    
    if (input$search_codes_only) {
      query <- paste0("
        SELECT 
          f.name as code_name,
          f.color,
          COUNT(c.rowid) as occurrences,
          COUNT(DISTINCT c.fid) as files_found
        FROM freecode f
        LEFT JOIN coding c ON f.id = c.cid AND c.status = 1
        WHERE f.project_id = ? AND f.status = 1 
          AND ", case_modifier, "(f.name) LIKE ?
        GROUP BY f.id, f.name, f.color
        ORDER BY occurrences DESC
      ")
      params <- list(current_project_id(), paste0("%", search_value, "%"))
    } else {
      query <- paste0("
        SELECT 
          f.name as code_name,
          s.name as file_name,
          c.seltext as coded_text,
          c.selfirst as start_position,
          c.owner as coded_by,
          c.date as coding_date
        FROM coding c
        JOIN freecode f ON c.cid = f.id
        JOIN source s ON c.fid = s.id
        WHERE c.project_id = ? AND c.status = 1 
          AND ", case_modifier, "(c.seltext) LIKE ?
        ORDER BY s.name, c.selfirst
      ")
      params <- list(current_project_id(), paste0("%", search_value, "%"))
    }
    
    result <- db_execute_query(db_pool, query, params)
    
    return(list(
      data = result,
      type = "search",
      title = paste("Search Results for:", search_term)
    ))
  }
  
  # Density analysis
  run_density_analysis <- function() {
    query <- "
      SELECT 
        s.name as file_name,
        f.name as code_name,
        f.color,
        COUNT(c.rowid) as coding_count,
        LENGTH(s.file) as file_length,
        ROUND(COUNT(c.rowid) * 1000.0 / LENGTH(s.file), 2) as density_per_1000_chars
      FROM source s
      LEFT JOIN coding c ON s.id = c.fid AND c.status = 1
      LEFT JOIN freecode f ON c.cid = f.id AND f.status = 1
      WHERE s.project_id = ? AND s.status = 1
      GROUP BY s.id, s.name, f.id, f.name, f.color, s.file
      ORDER BY s.name, coding_count DESC
    "
    
    result <- db_execute_query(db_pool, query, list(current_project_id()))
    
    return(list(
      data = result,
      type = "density",
      title = "Coding Density Analysis"
    ))
  }
  
  # Reliability analysis
  run_reliability_analysis <- function() {
    if (is.null(input$reliability_coders) || length(input$reliability_coders) < 2) {
      showNotification("Please select at least 2 coders for reliability analysis", type = "warning")
      return(NULL)
    }
    
    coders <- paste0("'", input$reliability_coders, "'", collapse = ",")
    
    query <- paste0("
      SELECT 
        c1.fid as file_id,
        s.name as file_name,
        c1.cid as code_id,
        f.name as code_name,
        c1.owner as coder1,
        c2.owner as coder2,
        c1.seltext as text_segment,
        ABS(c1.selfirst - c2.selfirst) as position_difference
      FROM coding c1
      JOIN coding c2 ON c1.fid = c2.fid AND c1.cid = c2.cid 
        AND c1.owner != c2.owner AND c1.rowid < c2.rowid
      JOIN source s ON c1.fid = s.id
      JOIN freecode f ON c1.cid = f.id
      WHERE c1.project_id = ? AND c1.status = 1 AND c2.status = 1
        AND c1.owner IN (", coders, ") AND c2.owner IN (", coders, ")
      ORDER BY s.name, f.name
    ")
    
    result <- db_execute_query(db_pool, query, list(current_project_id()))
    
    return(list(
      data = result,
      type = "reliability",
      title = "Inter-coder Reliability Analysis"
    ))
  }
  
  # Render analysis plot
  output$analysis_plot <- renderPlotly({
    if (is.null(values$analysis_data)) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "Run an analysis to see visualizations",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 16, color = "#666")
          ) %>%
          layout(
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          )
      )
    }
    
    switch(values$current_analysis,
      "frequency" = create_frequency_plot(values$analysis_data$data),
      "cooccurrence" = create_cooccurrence_plot(values$analysis_data$data),
      "retrieval" = create_retrieval_plot(values$analysis_data$data),
      "search" = create_search_plot(values$analysis_data$data),
      "density" = create_density_plot(values$analysis_data$data),
      "reliability" = create_reliability_plot(values$analysis_data$data)
    )
  })
  
  # Create frequency plot
  create_frequency_plot <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(plot_ly())
    }
    
    # Create bar chart
    p <- plot_ly(
      data = data,
      x = ~reorder(code_name, frequency),
      y = ~frequency,
      type = 'bar',
      marker = list(color = ~color),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Frequency: %{y}<br>",
        "Files coded: %{customdata}",
        "<extra></extra>"
      ),
      customdata = ~files_coded
    ) %>%
      layout(
        title = "Code Frequency Distribution",
        xaxis = list(title = "Codes"),
        yaxis = list(title = "Frequency"),
        margin = list(b = 100)
      )
    
    return(p)
  }
  
  # Create co-occurrence plot
  create_cooccurrence_plot <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(plot_ly())
    }
    
    # Create network-style plot or heatmap
    p <- plot_ly(
      data = data,
      x = ~code1,
      y = ~code2,
      z = ~cooccurrence_count,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = ~cooccurrence_count * 3,
        color = ~cooccurrence_count,
        colorscale = 'Viridis',
        showscale = TRUE
      ),
      hovertemplate = paste(
        "<b>%{x}</b> + <b>%{y}</b><br>",
        "Co-occurrences: %{z}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "Code Co-occurrence Network",
        xaxis = list(title = "Code 1"),
        yaxis = list(title = "Code 2")
      )
    
    return(p)
  }
  
  # Create retrieval plot
  create_retrieval_plot <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(plot_ly())
    }
    
    # Create timeline or distribution plot
    code_summary <- data %>%
      group_by(code_name) %>%
      summarise(
        count = n(),
        files = n_distinct(file_name),
        .groups = 'drop'
      )
    
    p <- plot_ly(
      data = code_summary,
      x = ~code_name,
      y = ~count,
      type = 'bar',
      marker = list(color = '#1f77b4'),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Retrieved segments: %{y}<br>",
        "Files: %{customdata}",
        "<extra></extra>"
      ),
      customdata = ~files
    ) %>%
      layout(
        title = "Retrieved Text Segments by Code",
        xaxis = list(title = "Codes"),
        yaxis = list(title = "Number of Segments")
      )
    
    return(p)
  }
  
  # Create search plot
  create_search_plot <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(plot_ly())
    }
    
    # Create appropriate plot based on search results
    if ("occurrences" %in% names(data)) {
      # Code search results
      p <- plot_ly(
        data = data,
        x = ~reorder(code_name, occurrences),
        y = ~occurrences,
        type = 'bar',
        marker = list(color = ~color),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Occurrences: %{y}<br>",
          "Files: %{customdata}",
          "<extra></extra>"
        ),
        customdata = ~files_found
      ) %>%
        layout(
          title = "Search Results Distribution",
          xaxis = list(title = "Codes"),
          yaxis = list(title = "Occurrences")
        )
    } else {
      # Text search results - show by file
      file_summary <- data %>%
        group_by(file_name) %>%
        summarise(count = n(), .groups = 'drop')
      
      p <- plot_ly(
        data = file_summary,
        x = ~file_name,
        y = ~count,
        type = 'bar',
        marker = list(color = '#2ca02c')
      ) %>%
        layout(
          title = "Search Results by File",
          xaxis = list(title = "Files"),
          yaxis = list(title = "Matches")
        )
    }
    
    return(p)
  }
  
  # Create density plot
  create_density_plot <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(plot_ly())
    }
    
    # Create heatmap of coding density
    file_summary <- data %>%
      group_by(file_name) %>%
      summarise(
        total_codings = sum(coding_count, na.rm = TRUE),
        avg_density = mean(density_per_1000_chars, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- plot_ly(
      data = file_summary,
      x = ~file_name,
      y = ~total_codings,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = ~avg_density * 10,
        color = ~avg_density,
        colorscale = 'Blues',
        showscale = TRUE,
        colorbar = list(title = "Density per 1000 chars")
      ),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Total codings: %{y}<br>",
        "Avg density: %{customdata:.2f}",
        "<extra></extra>"
      ),
      customdata = ~avg_density
    ) %>%
      layout(
        title = "Coding Density by File",
        xaxis = list(title = "Files"),
        yaxis = list(title = "Total Codings")
      )
    
    return(p)
  }
  
  # Create reliability plot
  create_reliability_plot <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(plot_ly())
    }
    
    # Calculate agreement statistics
    agreement_summary <- data %>%
      group_by(code_name) %>%
      summarise(
        agreements = n(),
        avg_position_diff = mean(position_difference, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- plot_ly(
      data = agreement_summary,
      x = ~code_name,
      y = ~agreements,
      type = 'bar',
      marker = list(color = '#ff7f0e'),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Agreements: %{y}<br>",
        "Avg position diff: %{customdata:.1f}",
        "<extra></extra>"
      ),
      customdata = ~avg_position_diff
    ) %>%
      layout(
        title = "Inter-coder Agreements by Code",
        xaxis = list(title = "Codes"),
        yaxis = list(title = "Number of Agreements")
      )
    
    return(p)
  }
  
  # Render analysis table
  output$analysis_table <- DT::renderDataTable({
    if (is.null(values$analysis_data)) {
      return(data.frame(Message = "No analysis results to display"))
    }
    
    data <- values$analysis_data$data
    
    # Format data based on analysis type
    if (values$current_analysis == "frequency") {
      display_data <- data[, c("code_name", "category", "frequency", "files_coded")]
      colnames(display_data) <- c("Code", "Category", "Frequency", "Files Coded")
    } else if (values$current_analysis == "cooccurrence") {
      display_data <- data[, c("code1", "code2", "cooccurrence_count", "files_with_cooccurrence")]
      colnames(display_data) <- c("Code 1", "Code 2", "Co-occurrences", "Files")
    } else if (values$current_analysis == "retrieval") {
      display_data <- data[, c("code_name", "file_name", "coded_text", "coded_by", "coding_date")]
      display_data$coded_text <- substr(display_data$coded_text, 1, 100)  # Truncate long text
      display_data$coding_date <- format(as.POSIXct(display_data$coding_date), "%Y-%m-%d %H:%M")
      colnames(display_data) <- c("Code", "File", "Text Segment", "Coded By", "Date")
    } else if (values$current_analysis == "search") {
      if ("occurrences" %in% names(data)) {
        display_data <- data[, c("code_name", "occurrences", "files_found")]
        colnames(display_data) <- c("Code", "Occurrences", "Files Found")
      } else {
        display_data <- data[, c("code_name", "file_name", "coded_text", "coded_by")]
        display_data$coded_text <- substr(display_data$coded_text, 1, 100)
        colnames(display_data) <- c("Code", "File", "Text Segment", "Coded By")
      }
    } else if (values$current_analysis == "density") {
      display_data <- data[, c("file_name", "code_name", "coding_count", "density_per_1000_chars")]
      colnames(display_data) <- c("File", "Code", "Count", "Density per 1000 chars")
    } else if (values$current_analysis == "reliability") {
      display_data <- data[, c("code_name", "file_name", "coder1", "coder2", "position_difference")]
      colnames(display_data) <- c("Code", "File", "Coder 1", "Coder 2", "Position Diff")
    } else {
      display_data <- data
    }
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'lrtip',
        order = list(list(2, 'desc'))  # Sort by third column descending
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  # Render analysis summary
  output$analysis_summary <- renderUI({
    if (is.null(values$analysis_data)) {
      return(
        div(
          style = "text-align: center; padding: 50px; color: #666;",
          icon("chart-bar", "fa-3x text-muted"),
          h4("No Analysis Results", style = "margin-top: 15px;"),
          p("Run an analysis to see detailed summary information.")
        )
      )
    }
    
    data <- values$analysis_data$data
    
    if (is.null(data) || nrow(data) == 0) {
      return(
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          strong(" No Data Found"),
          br(),
          "The analysis completed but returned no results. Try adjusting your filters or criteria."
        )
      )
    }
    
    # Generate summary based on analysis type
    summary_content <- switch(values$current_analysis,
      "frequency" = generate_frequency_summary(data),
      "cooccurrence" = generate_cooccurrence_summary(data),
      "retrieval" = generate_retrieval_summary(data),
      "search" = generate_search_summary(data),
      "density" = generate_density_summary(data),
      "reliability" = generate_reliability_summary(data)
    )
    
    div(
      h4(values$analysis_data$title),
      summary_content
    )
  })
  
  # Generate frequency summary
  generate_frequency_summary <- function(data) {
    total_codes <- nrow(data)
    total_frequency <- sum(data$frequency)
    most_frequent <- data$code_name[which.max(data$frequency)]
    avg_frequency <- round(mean(data$frequency), 1)
    
    div(
      div(
        class = "alert alert-info",
        h5("Frequency Analysis Summary"),
        tags$ul(
          tags$li(paste("Total codes analyzed:", total_codes)),
          tags$li(paste("Total codings:", total_frequency)),
          tags$li(paste("Most frequent code:", most_frequent, "(", max(data$frequency), "times)")),
          tags$li(paste("Average frequency per code:", avg_frequency))
        )
      ),
      
      h5("Key Insights:"),
      tags$ul(
        tags$li(if (total_codes > 0) {
          paste("You have", total_codes, "active codes in your project.")
        }),
        tags$li(if (avg_frequency < 2) {
          "Many codes are used infrequently. Consider consolidating similar codes."
        } else if (avg_frequency > 10) {
          "Codes are used frequently. Your coding scheme appears well-developed."
        } else {
          "Code usage is moderate. Continue developing your analysis."
        }),
        tags$li(if (max(data$frequency) > total_frequency * 0.3) {
          paste("The code '", most_frequent, "' dominates your coding. Consider if it's too broad.")
        })
      )
    )
  }
  
  # Generate co-occurrence summary
  generate_cooccurrence_summary <- function(data) {
    total_pairs <- nrow(data)
    total_cooccurrences <- sum(data$cooccurrence_count)
    strongest_pair <- paste(data$code1[which.max(data$cooccurrence_count)], 
                           "+", data$code2[which.max(data$cooccurrence_count)])
    
    div(
      div(
        class = "alert alert-info",
        h5("Co-occurrence Analysis Summary"),
        tags$ul(
          tags$li(paste("Code pairs with co-occurrences:", total_pairs)),
          tags$li(paste("Total co-occurrences found:", total_cooccurrences)),
          tags$li(paste("Strongest relationship:", strongest_pair))
        )
      ),
      
      h5("Pattern Insights:"),
      tags$ul(
        tags$li(if (total_pairs > 0) {
          paste("Found", total_pairs, "code pairs that appear together in your data.")
        }),
        tags$li(if (total_pairs > 20) {
          "High number of co-occurrences suggests rich, interconnected themes."
        } else if (total_pairs < 5) {
          "Few co-occurrences found. Codes may be quite distinct or need refinement."
        }),
        tags$li("Strong co-occurrences may indicate conceptual relationships worth exploring.")
      )
    )
  }
  
  # Generate retrieval summary
  generate_retrieval_summary <- function(data) {
    total_segments <- nrow(data)
    unique_codes <- length(unique(data$code_name))
    unique_files <- length(unique(data$file_name))
    avg_segment_length <- round(mean(nchar(data$coded_text), na.rm = TRUE), 0)
    
    div(
      div(
        class = "alert alert-info",
        h5("Text Retrieval Summary"),
        tags$ul(
          tags$li(paste("Text segments retrieved:", total_segments)),
          tags$li(paste("Codes involved:", unique_codes)),
          tags$li(paste("Files with segments:", unique_files)),
          tags$li(paste("Average segment length:", avg_segment_length, "characters"))
        )
      ),
      
      h5("Content Overview:"),
      tags$ul(
        tags$li(if (total_segments > 50) {
          "Large volume of coded text - rich data for analysis."
        } else if (total_segments < 10) {
          "Limited coded segments. Consider expanding your coding."
        } else {
          "Moderate amount of coded text for analysis."
        }),
        tags$li(if (avg_segment_length > 200) {
          "Long text segments suggest detailed, contextual coding."
        } else if (avg_segment_length < 50) {
          "Short segments suggest focused, specific coding."
        }),
        tags$li("Use this retrieved text for pattern analysis and theory development.")
      )
    )
  }
  
  # Generate search summary
  generate_search_summary <- function(data) {
    if ("occurrences" %in% names(data)) {
      total_occurrences <- sum(data$occurrences)
      codes_found <- nrow(data)
      
      summary_content <- div(
        div(
          class = "alert alert-info",
          h5("Search Results Summary"),
          tags$ul(
            tags$li(paste("Codes matching search:", codes_found)),
            tags$li(paste("Total occurrences:", total_occurrences))
          )
        )
      )
    } else {
      total_matches <- nrow(data)
      unique_files <- length(unique(data$file_name))
      unique_codes <- length(unique(data$code_name))
      
      summary_content <- div(
        div(
          class = "alert alert-info",
          h5("Text Search Summary"),
          tags$ul(
            tags$li(paste("Text segments found:", total_matches)),
            tags$li(paste("Files with matches:", unique_files)),
            tags$li(paste("Different codes involved:", unique_codes))
          )
        )
      )
    }
    
    return(summary_content)
  }
  
  # Generate density summary
  generate_density_summary <- function(data) {
    files_analyzed <- length(unique(data$file_name))
    total_codings <- sum(data$coding_count, na.rm = TRUE)
    avg_density <- round(mean(data$density_per_1000_chars, na.rm = TRUE), 2)
    max_density_file <- data$file_name[which.max(data$density_per_1000_chars)]
    
    div(
      div(
        class = "alert alert-info",
        h5("Coding Density Summary"),
        tags$ul(
          tags$li(paste("Files analyzed:", files_analyzed)),
          tags$li(paste("Total codings:", total_codings)),
          tags$li(paste("Average density:", avg_density, "codings per 1000 characters")),
          tags$li(paste("Highest density file:", max_density_file))
        )
      ),
      
      h5("Density Insights:"),
      tags$ul(
        tags$li(if (avg_density > 5) {
          "High coding density suggests thorough analysis."
        } else if (avg_density < 1) {
          "Low coding density - consider more detailed coding."
        } else {
          "Moderate coding density indicates balanced analysis."
        }),
        tags$li("Dense files may contain rich, complex content worth deeper analysis."),
        tags$li("Sparse files might need additional coding or contain simpler content.")
      )
    )
  }
  
  # Generate reliability summary
  generate_reliability_summary <- function(data) {
    total_agreements <- nrow(data)
    unique_codes <- length(unique(data$code_name))
    unique_coders <- length(unique(c(data$coder1, data$coder2)))
    avg_position_diff <- round(mean(data$position_difference, na.rm = TRUE), 1)
    
    # Calculate simple agreement percentage (positions within 50 characters)
    close_agreements <- sum(data$position_difference <= 50, na.rm = TRUE)
    agreement_rate <- round((close_agreements / total_agreements) * 100, 1)
    
    div(
      div(
        class = "alert alert-info",
        h5("Inter-coder Reliability Summary"),
        tags$ul(
          tags$li(paste("Total agreements found:", total_agreements)),
          tags$li(paste("Codes with multiple coders:", unique_codes)),
          tags$li(paste("Number of coders:", unique_coders)),
          tags$li(paste("Average position difference:", avg_position_diff, "characters")),
          tags$li(paste("Close agreement rate:", agreement_rate, "%"))
        )
      ),
      
      h5("Reliability Assessment:"),
      tags$ul(
        tags$li(if (agreement_rate > 80) {
          "High reliability - coders are applying codes consistently."
        } else if (agreement_rate > 60) {
          "Moderate reliability - some inconsistency in coding."
        } else {
          "Low reliability - significant differences between coders."
        }),
        tags$li(if (avg_position_diff < 20) {
          "Coders are identifying very similar text boundaries."
        } else if (avg_position_diff > 100) {
          "Large position differences suggest different interpretation of code boundaries."
        }),
        tags$li("Consider coder training or code definition refinement to improve reliability.")
      )
    )
  }
  
  # Query builder functionality
  observeEvent(input$add_condition, {
    if (is.null(input$query_field) || is.null(input$query_operator) || 
        is.null(input$query_value) || input$query_value == "") {
      showNotification("Please fill all query fields", type = "warning")
      return()
    }
    
    new_condition <- list(
      field = input$query_field,
      operator = input$query_operator,
      value = input$query_value,
      case_sensitive = input$query_case_sensitive
    )
    
    values$query_conditions <- append(values$query_conditions, list(new_condition))
    
    # Update UI
    update_query_conditions_display()
    
    # Clear inputs
    updateTextInput(session, "query_value", value = "")
  })
  
  # Clear query conditions
  observeEvent(input$clear_conditions, {
    values$query_conditions <- list()
    update_query_conditions_display()
  })
  
  # Update query conditions display
  update_query_conditions_display <- function() {
    if (length(values$query_conditions) == 0) {
      content <- p("No conditions added yet", style = "color: #666; font-style: italic;")
    } else {
      condition_items <- lapply(1:length(values$query_conditions), function(i) {
        cond <- values$query_conditions[[i]]
        div(
          style = "background-color: #e9ecef; padding: 8px; margin: 5px 0; border-radius: 3px; border-left: 3px solid #007bff;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            span(
              paste(cond$field, cond$operator, paste0('"', cond$value, '"')),
              if (cond$case_sensitive) " (case sensitive)" else ""
            ),
            actionButton(
              ns(paste0("remove_condition_", i)),
              "",
              icon = icon("times"),
              class = "btn-link btn-sm",
              style = "color: #dc3545;",
              onclick = paste0("Shiny.setInputValue('", ns("remove_condition"), "', ", i, ");")
            )
          )
        )
      })
      content <- do.call(tagList, condition_items)
    }
    
    removeUI(
      selector = paste0("#", ns("query_conditions"), " > *"),
      multiple = TRUE
    )
    
    insertUI(
      selector = paste0("#", ns("query_conditions")),
      where = "afterBegin",
      ui = content
    )
  }
  
  # Remove query condition
  observe({
    if (!is.null(input$remove_condition)) {
      condition_index <- input$remove_condition
      if (condition_index <= length(values$query_conditions)) {
        values$query_conditions[[condition_index]] <- NULL
        update_query_conditions_display()
      }
    }
  })
  
  # Generate SQL from query conditions
  output$generated_sql <- renderText({
    if (length(values$query_conditions) == 0) {
      return("Add conditions to see generated SQL query")
    }
    
    base_query <- "SELECT DISTINCT c.*, f.name as code_name, s.name as file_name
FROM coding c
JOIN freecode f ON c.cid = f.id  
JOIN source s ON c.fid = s.id
WHERE c.project_id = ? AND c.status = 1"
    
    conditions <- sapply(values$query_conditions, function(cond) {
      field_map <- list(
        "code_name" = "f.name",
        "coded_text" = "c.seltext", 
        "file_name" = "s.name",
        "memo" = "c.memo"
      )
      
      db_field <- field_map[[cond$field]]
      
      if (cond$operator == "LIKE") {
        if (cond$case_sensitive) {
          paste0(db_field, " LIKE '%", cond$value, "%'")
        } else {
          paste0("LOWER(", db_field, ") LIKE LOWER('%", cond$value, "%')")
        }
      } else if (cond$operator == "NOT_LIKE") {
        if (cond$case_sensitive) {
          paste0(db_field, " NOT LIKE '%", cond$value, "%'")
        } else {
          paste0("LOWER(", db_field, ") NOT LIKE LOWER('%", cond$value, "%')")
        }
      } else if (cond$operator == "STARTS_WITH") {
        if (cond$case_sensitive) {
          paste0(db_field, " LIKE '", cond$value, "%'")
        } else {
          paste0("LOWER(", db_field, ") LIKE LOWER('", cond$value, "%')")
        }
      } else if (cond$operator == "ENDS_WITH") {
        if (cond$case_sensitive) {
          paste0(db_field, " LIKE '%", cond$value, "'")
        } else {
          paste0("LOWER(", db_field, ") LIKE LOWER('%", cond$value, "')")
        }
      } else {
        if (cond$case_sensitive) {
          paste0(db_field, " = '", cond$value, "'")
        } else {
          paste0("LOWER(", db_field, ") = LOWER('", cond$value, "')")
        }
      }
    })
    
    full_query <- paste(base_query, "AND", paste(conditions, collapse = " AND "))
    return(full_query)
  })
  
  # Run custom query
  observeEvent(input$run_query, {
    if (length(values$query_conditions) == 0) {
      showNotification("Please add at least one condition", type = "warning")
      return()
    }
    
    if (is.null(current_project_id())) {
      showNotification("Please select a project first", type = "error")
      return()
    }
    
    # Build and execute query
    sql_query <- output$generated_sql()
    
    tryCatch({
      result <- db_execute_query(db_pool, sql_query, list(current_project_id()))
      
      if (!is.null(result) && nrow(result) > 0) {
        values$analysis_data <- list(
          data = result,
          type = "query",
          title = "Custom Query Results"
        )
        values$current_analysis <- "query"
        showNotification(paste("Query returned", nrow(result), "results"), type = "message")
      } else {
        showNotification("Query returned no results", type = "info")
      }
      
    }, error = function(e) {
      showNotification(paste("Query error:", e$message), type = "error")
    })
  })
  
  # Download analysis table
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("rqda_analysis_", values$current_analysis, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$analysis_data)) {
        write.csv(values$analysis_data$data, file, row.names = FALSE)
      }
    }
  )
  
  # Export results
  observeEvent(input$export_results, {
    if (is.null(values$analysis_data)) {
      showNotification("No analysis results to export", type = "warning")
      return()
    }
    
    # For now, just show notification - could implement various export formats
    showNotification("Export functionality coming soon! Use 'Download Table' for CSV export.", type = "info")
  })
  
  # Save analysis
  observeEvent(input$save_analysis, {
    if (is.null(values$analysis_data)) {
      showNotification("No analysis results to save", type = "warning")
      return()
    }
    
    # This could save analysis configurations for later reuse
    showNotification("Analysis save functionality coming soon!", type = "info")
  })
  
  # Return reactive values for use by other modules
  return(list(
    analysis_data = reactive({ values$analysis_data }),
    project_stats = reactive({ values$project_stats }),
    current_analysis = reactive({ values$current_analysis })
  ))
}
