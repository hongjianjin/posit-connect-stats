library(shiny)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(apexcharter)
library(connectapi)
library(dplyr)
library(shinycssloaders)
#install.packages("fontawesome") to solve 

# Settings live in .env (CONNECT_SERVER, CONNECT_API_KEY, STATS_ADMIN).
# On a server where .env is not deployed, the same variables can be set in the
# environment instead.
if (file.exists(".env")) readRenviron(".env")
for (v in c("CONNECT_SERVER", "CONNECT_API_KEY", "STATS_ADMIN")) {
  do.call(Sys.setenv, setNames(list(trimws(Sys.getenv(v))), v))  # strip stray CR/spaces
}
if (!nzchar(Sys.getenv("CONNECT_SERVER")) || !nzchar(Sys.getenv("CONNECT_API_KEY"))) {
  stop("CONNECT_SERVER and CONNECT_API_KEY must be defined in .env")
}
# Comma/semicolon separated Connect usernames allowed to download user lists
stats_admins <- tolower(trimws(strsplit(Sys.getenv("STATS_ADMIN"), "[,;]")[[1]]))
stats_admins <- stats_admins[nzchar(stats_admins)]

client <- connect()

# Data retention window for the initial fetch; the period selector below
# filters within this window, so it also bounds what "All Time" can show.
Sys.setenv("DAYSBACK"=1095)
days_back <- as.numeric(Sys.getenv("DAYSBACK"))
#days_back <-  as.numeric(difftime(lubridate::today(), "2022-01-01","Days"))
cache_location <- Sys.getenv("MEMOISE_CACHE_LOCATION", tempdir())
message(cache_location)
# Fetch everything since the Connect server was set up (override with STATS_START_DATE in .env)
report_from <- as.Date(Sys.getenv("STATS_START_DATE", "2021-01-01"))
report_to <- lubridate::today()

# TODO: better way to do caching...?
# TODO: add connect server URL to the cache_location
cached_usage_shiny <- memoise::memoise(
  get_usage_shiny,
  cache = memoise::cache_filesystem(cache_location),
  omit_args = c("src") # BEWARE: cache can cross connect hosts if you change connect targets
)

cached_usage_static <- memoise::memoise(
  get_usage_static,
  cache = memoise::cache_filesystem(cache_location),
  omit_args = c("src") # BEWARE: cache can cross connect hosts if you change connect targets
)

# create alternate versions that have a date to invalidate the cache

local_get_users <- function(client, date, limit, ...) {
  connectapi::get_users(client, limit = limit, ...)
}

cached_get_users <- memoise::memoise(
  local_get_users,
  cache = memoise::cache_filesystem(cache_location),
  omit_args = "client"
)

local_get_content <- function(client, date, ...) {
  connectapi::get_content(client, ...)
}

cached_get_content <- memoise::memoise(
  local_get_content,
  cache = memoise::cache_filesystem(cache_location),
  omit_args = "client"
)

# Data Fetch -------------------------------------------------------------

ui <- fluidPage(
  shinyjs::useShinyjs(),
  useShinyalert(),
  tags$head(
    tags$title("IBEX Usage Insights"),
    tags$style(HTML("
      .app-title { text-align: center; font-size: 24px; font-weight: 600;
                   padding: 12px 0 6px 0; }
      .nav-tabs { margin-bottom: 15px; }
    "))
  ),
  div(class = "app-title", "IBEX Usage Insights"),
  tabsetPanel(id = "tabs",
    tabPanel("Stats", value = "stats",
    fluidRow(
      column(4,
        selectInput(
          "period", "Time Period:",
          choices = list(
            "Period" = c(
              "This Month" = "month",
              "Year to Date" = "ytd",
              "Last 6 Months" = "6m",
              "Last 1 Year" = "1y",
              "All Time" = "all"
            )
          ),                       # previous years are added by the server
          selected = "6m"
        )
      ),
      column(8,
        br(),
        textOutput("periodLabel")
      )
    ),
    fluidRow(
      # By Date takes 2/3 of the width; the overview (By Year / By Month) takes 1/3
      div(id = "dateMain", class = "col-sm-8",
        shinycssloaders::withSpinner(
          apexchartOutput("shiny_time")
        )
      ),
      div(id = "dateOverview", class = "col-sm-4",
        apexchartOutput("shiny_year")
      )
    ),
    fluidRow(
      column(3, apexchartOutput("shiny_content")),
      column(3, apexchartOutput("shiny_unique")),
      column(3, apexchartOutput("shiny_viewer")),
      column(3, apexchartOutput("shiny_owner"))
    )
    ,
    fluidRow(width=12,
      column(12, align="center",
        actionButton("btnReset", label = "Reset",icon=icon("sync"), class = "btn-info" ),
        actionButton("btnHelp", label = "Help",icon=icon("question"), class = "btn-info")
      )
      #verbatimTextOutput("verbatim")
    )
    )
    # "Admin" tab is added by the server for users listed in STATS_ADMIN
  )
)

safe_filter <- function(data, min_date = NULL, max_date = NULL) {
  data_prep <- data
  if (!is.null(min_date)) {
    data_prep <- data_prep %>% filter(started >= min_date)
  }
  if (!is.null(max_date)) {
    data_prep <- data_prep %>% filter(started <= max_date + 1)
  }
  
  return(data_prep)
}

# Map a period selector value to a from/to Date range, bounded below by
# `earliest` (the start of the data actually fetched from Connect).
period_bounds <- function(period, today, earliest) {
  # "y2025": a whole calendar year (the current year is covered by "Year to Date")
  if (grepl("^y[0-9]{4}$", period)) {
    yr <- as.integer(substring(period, 2))
    return(list(from = max(as.Date(sprintf("%d-01-01", yr)), earliest),
                to   = min(as.Date(sprintf("%d-12-31", yr)), today)))
  }
  from <- switch(period,
    month = lubridate::floor_date(today, "month"),
    ytd   = lubridate::floor_date(today, "year"),
    "6m"  = seq(today, by = "-6 month", length.out = 2)[2],
    "1y"  = seq(today, by = "-1 year", length.out = 2)[2],
    all   = earliest,
    seq(today, by = "-6 month", length.out = 2)[2]
  )
  list(from = max(as.Date(from), earliest), to = today)
}

# One fixed color per calendar month (Jan..Dec), shared by the By Date chart and the
# By Month overview so the same month has the same color in both.
month_colors <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948",
                  "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC", "#D37295", "#499894")

# One fixed color per calendar year (cycles every 10 years), shared by the All-Time
# By Date chart and the By Year overview so a year has the same color in both.
year_palette <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
                  "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")
year_colors <- function(years) year_palette[as.integer(years) %% length(year_palette) + 1]

# By Date chart. For "All Time" every year gets its own curve (and color),
# overlaid on a Jan-Dec axis; otherwise a single curve over the selected period.
date_chart <- function(counts, type, period) {
  ms <- function(d) as.numeric(as.POSIXct(as.Date(d), tz = "UTC")) * 1000
  ch <- apexchart(auto_update = FALSE) %>%
    ax_chart(type = type) %>%
    ax_title("By Date") %>%
    ax_plotOptions() %>%
    ax_dataLabels(enabled = FALSE) %>%                   # no per-bar/point value labels (slow)
    ax_chart(animations = list(enabled = FALSE))          # skip draw animations
  if (identical(period, "all")) {
    # Stacked bars: one segment per year (earliest year at the bottom, latest on top).
    # Every year gets a value for every calendar day (0 if none) so segments stack aligned.
    grid <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")
    grid_key <- format(grid, "%m-%d")
    grid_ms <- ms(grid)
    counts$year <- format(counts$date, "%Y")
    counts$key <- format(counts$date, "%m-%d")
    years <- sort(unique(counts$year))
    series <- lapply(years, function(y) {
      d <- counts[counts$year == y, ]
      vals <- d$n[match(grid_key, d$key)]
      vals[is.na(vals)] <- 0
      list(name = y, data = purrr::map2(grid_ms, vals, ~ list(.x, .y)))
    })
    ch <- do.call(ax_series, c(list(ch), series))
    ch %>%
      ax_chart(type = "bar", stacked = TRUE) %>%
      ax_colors(year_colors(years)) %>%
      ax_xaxis(type = "datetime", min = ms("2000-01-01"), max = ms("2000-12-31"),
               labels = list(format = "MMM")) %>%
      ax_tooltip(shared = TRUE, intersect = FALSE, x = list(format = "dd MMM")) %>%
      ax_legend(position = "top")
  } else {
    # Daily bars colored by month: one series per month on a full day grid (0-filled)
    if (nrow(counts) == 0) return(ch)
    grid <- seq(min(counts$date), max(counts$date), by = "day")
    grid_ms <- ms(grid)
    grid_ym <- format(grid, "%Y-%m")
    vals_all <- counts$n[match(grid, counts$date)]
    vals_all[is.na(vals_all)] <- 0
    yms <- unique(grid_ym)
    series <- lapply(yms, function(ym) {
      v <- ifelse(grid_ym == ym, vals_all, 0)
      list(name = format(as.Date(paste0(ym, "-01")), "%b %Y"), data = purrr::map2(grid_ms, v, ~ list(.x, .y)))
    })
    cols <- month_colors[as.integer(substr(yms, 6, 7))]
    do.call(ax_series, c(list(ch), series)) %>%
      ax_chart(type = "bar", stacked = TRUE) %>%
      ax_colors(cols) %>%
      ax_xaxis(type = "datetime") %>%
      ax_tooltip(shared = FALSE, x = list(format = "dd MMM yyyy")) %>%
      ax_legend(show = FALSE) %>%
      set_input_selection("time")
  }
}

# "By Unique User": apps ranked by the number of distinct viewers (anonymous visits ignored)
unique_by_app <- function(df, content) {
  df %>%
    filter(!is.na(user_guid)) %>%
    group_by(content_guid) %>%
    summarize(n = n_distinct(user_guid), .groups = "drop") %>%
    left_join(content %>% select(guid, title), by = c(content_guid = "guid")) %>%
    filter(!is.na(title)) %>%
    arrange(desc(n))
}
unique_chart <- function(df) {
  apex(data = head(df, 20), type = "bar", mapping = aes(title, n), auto_update = FALSE) %>%   # full re-draw on every change
    ax_title("By Unique User (Top 20)") %>%
    ax_colors("#8E44AD") %>%            # purple bars
    set_input_click("content")
}

server <- function(input, output, session) {
  
  
  # Data Prep -------------------------------------------------------------
  #data <- reactiveValues()
  
  # Must include "to" or the cache can get weird!!
  data_shiny <- cached_usage_shiny(client, from = report_from, to = report_to, limit = Inf)
  data_shiny$ActiveSecs <- as.numeric(difftime(data_shiny$ended,data_shiny$started,unit='secs'))
  data_shiny <- data_shiny[data_shiny$ActiveSecs>5, ]  # remove visits < 5 seconds
  # data_static <- cached_usage_static(client, from = report_from, to = report_to, limit = Inf) # ~ 3 minutes on a busy server...
  data_content <- cached_get_content(client, date = report_to)
  # Apps without a title would be dropped by the title filters below; fall back to the app name
  data_content$title <- ifelse(is.na(data_content$title) | !nzchar(data_content$title),
                               data_content$name, data_content$title)
  data_users <- cached_get_users(client, date = report_to, limit = Inf)
  df0 <- unique(data_content[,c("guid","title","owner_guid")])
  df1 <- unique(data_users[,c("guid","username")])
  df2 <- unique(data_shiny[,c("content_guid","user_guid")])
  viewerDF <- merge(x = df1, y = df2, by.x = "guid", by.y="user_guid", all.y = TRUE) #Right outer: 
  appDF <- merge(x = df0, y = df1, by.x = "owner_guid", by.y="guid", all.x = TRUE) #Right outer: 
  #---------------------------------------- # 
  #remove developers visits
  tmp <- paste(data_shiny$user_guid,"_",data_shiny$content_guid, sep='')
  combns <- paste(appDF$owner_guid,"_",appDF$guid,sep='')
  data_shiny$DevVists <- tmp %in% combns
  data_shiny <- data_shiny[ data_shiny$DevVists %in% FALSE,]
  #----------------------------------------
  delay_duration <- 500

  # Previous calendar years that have data become Time Period choices
  # (the current year is already "Year to Date").
  yrs <- sort(unique(format(data_shiny$started, "%Y")), decreasing = TRUE)
  yrs <- setdiff(yrs, format(report_to, "%Y"))
  if (length(yrs) > 0) {
    updateSelectInput(session, "period", selected = "6m", choices = list(
      "Period" = c(
        "This Month" = "month", "Year to Date" = "ytd", "Last 6 Months" = "6m",
        "Last 1 Year" = "1y", "All Time" = "all"
      ),
      "Year" = setNames(paste0("y", yrs), yrs)
    ))
  }

  # Clicking a bar in "By Year" selects that year as the Time Period
  observeEvent(input$year_pick, {
    yr <- as.character(input$year_pick)
    updateSelectInput(session, "period",
      selected = if (identical(yr, format(report_to, "%Y"))) "ytd" else paste0("y", yr))
    shinyjs::runjs("Shiny.setInputValue('year_pick', null);")   # so the same bar can be clicked again later
  })

  # Period-selector range, used to filter the By Date chart (not the brush selection)
  period_range <- reactive(period_bounds(input$period, report_to, report_from))

  shiny_content <- debounce(reactive(
    
    data_shiny %>%
      safe_filter(min_date = minTime(), max_date = maxTime()) %>%
      group_by(content_guid) %>%
      tally() %>%
      left_join(
        data_content %>% select(guid, name, title, description),
        by = c(content_guid = "guid")
      ) %>%
      filter(!is.na(title)) %>% 
      arrange(desc(n))
  ), delay_duration)
  
  shiny_viewers <- debounce(reactive(
    data_shiny %>%
      safe_filter(min_date = minTime(), max_date = maxTime()) %>%
      group_by(user_guid) %>%
      tally() %>%
      left_join(
        data_users %>% select(guid, username),
        by = c(user_guid = "guid")
      ) %>%
      arrange(desc(n))
  ), delay_duration)
  
  
  shiny_owners <- debounce(reactive(
    data_shiny %>%
      safe_filter(min_date = minTime(), max_date = maxTime()) %>%
      left_join(
        data_content %>% select(guid, owner_guid),
        by = c(content_guid = "guid")
      ) %>%
      filter(!is.na(owner_guid)) %>%     # remove content that was deleted
      group_by(owner_guid) %>%
      tally() %>%
      left_join(
        data_users %>% select(guid, username),
        by = c(owner_guid = "guid")
      ) %>% 
      arrange(desc(n))
  ), delay_duration)
  
  shiny_over_time <- debounce(reactive(
    data_shiny %>%
      safe_filter(min_date = period_range()$from, max_date = period_range()$to) %>%
        mutate(
        date = lubridate::as_date(lubridate::floor_date(started, "day"))
      ) %>%
      group_by(date) %>%
      tally() %>%
      mutate(
        date_disp = format(date, format="%a %b %d %Y")
      ) %>%
      arrange(date)
  ), delay_duration)
  
  # Observers for Selection ----------------------------------------------------------
  
  # Current focus (all / app / viewer / owner) used by the By Year overview
  year_sel <- reactiveVal(list(f = function(d) d, label = "All Apps"))

  minTime <- reactiveVal(report_from)
  maxTime <- reactiveVal(report_to)
  
  observeEvent(input$period, {
    bounds <- period_bounds(input$period, report_to, report_from)
    minTime(bounds$from)
    maxTime(bounds$to)
  })
  
  output$periodLabel <- renderText({
    bounds <- period_bounds(input$period, report_to, report_from)
    glue::glue("Showing usage from {format(bounds$from, '%b %d, %Y')} to {format(bounds$to, '%b %d, %Y')}")
  })
  
  observeEvent(input$time, {
    input_min <- lubridate::as_date(input$time[[1]]$min)
    input_max <- lubridate::as_date(input$time[[1]]$max)
    # TODO: a way to "deselect" the time series
    if (identical(input_min, input_max)) {
      # treat "equals" as nothing selected
      minTime(report_from)
      maxTime(report_to)
    } else {
      minTime(input_min)
      maxTime(input_max)
    }
  })
  # ----------------------------------------------------------------------
  
  
  observeEvent(input$content, {
    appName <- as.character(input$content)
    year_sel(list(
      f = function(d) d %>% filter(content_guid %in% appDF$guid[appDF$title %in% appName]),
      label = appName
    ))
    selectedGuids <- appDF$guid[appDF$title %in% appName]
    developers <- unique(appDF$owner_guid[appDF$title %in% appName])  # exclude developer's visits
    shiny_over_time1<-reactive(
      data_shiny %>%filter(content_guid %in% selectedGuids ) %>%
        safe_filter(min_date = period_range()$from, max_date = period_range()$to) %>%
        mutate(
          date = lubridate::as_date(lubridate::floor_date(started, "day"))
        ) %>%
        group_by(date) %>%
        tally() %>%
        mutate(
          date_disp = format(date, format="%a %b %d %Y")
        ) %>%
        arrange(date)
    )

    output$shiny_time <- renderApexchart(date_chart(shiny_over_time1(), "line", input$period))
    
    shiny_viewers1 <- reactive(
      data_shiny  %>%filter(content_guid %in% selectedGuids & ! user_guid %in% developers) %>%
        safe_filter(min_date = minTime(), max_date = maxTime()) %>%
        group_by(user_guid) %>%
        tally() %>%
        left_join(
          data_users %>% select(guid, username),
          by = c(user_guid = "guid")
        ) %>%
        arrange(desc(n))
    )
    
    output$shiny_viewer <- renderApexchart(
      apex(
        data = shiny_viewers1() %>% head(20), 
        type = "bar", 
        mapping = aes(username, n)
      ) %>%
        ax_title("By Viewer (Top 20)") %>%
        ax_colors("#2E9E5B") %>%            # green bars
        set_input_click("viewer")
    )
    
  })
  

  # ----------------------------------------------------------------------
  
  observeEvent(input$viewer, {
    viewerName <- as.character(input$viewer)
    
    selectedGuids <- viewerDF$content_guid[viewerDF$username==viewerName]
    userGuids <- unique(unlist(viewerDF$guid[viewerDF$username==viewerName]))
    year_sel(list(
      f = function(d) d %>% filter(content_guid %in% selectedGuids & user_guid %in% userGuids),
      label = viewerName
    ))
    shiny_over_time2<-reactive(
      data_shiny %>%
        filter(content_guid %in% selectedGuids & user_guid %in% userGuids) %>%
        safe_filter(min_date = period_range()$from, max_date = period_range()$to) %>%
        mutate(
          date = lubridate::as_date(lubridate::floor_date(started, "day"))
        ) %>%
        group_by(date) %>%
        tally() %>%
        mutate(
          date_disp = format(date, format="%a %b %d %Y")
        ) %>%
        arrange(date)
    )
    
    output$shiny_time <- renderApexchart(date_chart(shiny_over_time2(), "bar", input$period))
    
    
    shiny_content2 <- debounce(reactive(
      
      data_shiny %>%
        filter(content_guid %in% selectedGuids & user_guid %in% userGuids) %>%
        safe_filter(min_date = minTime(), max_date = maxTime()) %>%
        group_by(content_guid) %>%
        tally() %>%
        left_join(
          data_content %>% select(guid, name, title, description),
          by = c(content_guid = "guid")
        ) %>%
        filter(!is.na(title)) %>% 
        arrange(desc(n))
    ), delay_duration)
    
    output$shiny_content <- renderApexchart(
      apex(
        data = shiny_content2() %>% head(20), 
        type = "bar", 
        mapping = aes(title, n)
      ) %>%
        ax_title("By App (Top 20)") %>%
        set_input_click("content")
    )

    output$shiny_unique <- renderApexchart(
      unique_chart(unique_by_app(
        data_shiny %>%
          filter(content_guid %in% selectedGuids & user_guid %in% userGuids) %>%
          safe_filter(min_date = minTime(), max_date = maxTime()),
        data_content))
    )
    
    
  })
  
  # ----------------------------------------------------------------------
  
  observeEvent(input$owner, {
    ownerName <- as.character(input$owner)
    selectedGuids <- unlist(appDF$guid[appDF$username==ownerName])
    year_sel(list(
      f = function(d) d %>% filter(content_guid %in% selectedGuids),
      label = paste("apps of", ownerName)
    ))
    shiny_over_time1<-reactive(
      data_shiny %>%filter(content_guid %in% selectedGuids) %>%
        safe_filter(min_date = period_range()$from, max_date = period_range()$to) %>%
        mutate(
          date = lubridate::as_date(lubridate::floor_date(started, "day"))
        ) %>%
        group_by(date) %>%
        tally() %>%
        mutate(
          date_disp = format(date, format="%a %b %d %Y")
        ) %>%
        arrange(date)
    )
    
    output$shiny_time <- renderApexchart(date_chart(shiny_over_time1(), "bar", input$period))
    
    shiny_viewers1 <- reactive(
      data_shiny  %>%
        filter(content_guid %in% selectedGuids) %>%
        safe_filter(min_date = minTime(), max_date = maxTime()) %>%
        group_by(user_guid) %>%
        tally() %>%
        left_join(
          data_users %>% select(guid, username),
          by = c(user_guid = "guid")
        ) %>%
        arrange(desc(n))
    )
    
    output$shiny_viewer <- renderApexchart(
      apex(
        data = shiny_viewers1() %>% head(20), 
        type = "bar", 
        mapping = aes(username, n)
      ) %>%
        ax_title("By Viewer (Top 20)") %>%
        ax_colors("#2E9E5B") %>%            # green bars
        set_input_click("viewer")
    )
    
    shiny_content <- debounce(reactive(
      
      data_shiny %>%
        filter(content_guid %in% selectedGuids) %>%
        safe_filter(min_date = minTime(), max_date = maxTime()) %>%
        group_by(content_guid) %>%
        tally() %>%
        left_join(
          data_content %>% select(guid, name, title, description),
          by = c(content_guid = "guid")
        ) %>%
        filter(!is.na(title)) %>% 
        arrange(desc(n))
    ), delay_duration)
    
    output$shiny_content <- renderApexchart(
      apex(
        data = shiny_content() %>% head(20), 
        type = "bar", 
        mapping = aes(title, n)
      ) %>%
        ax_title("By App (Top 20)") %>%
        set_input_click("content")
    )

    output$shiny_unique <- renderApexchart(
      unique_chart(unique_by_app(
        data_shiny %>%
          filter(content_guid %in% selectedGuids) %>%
          safe_filter(min_date = minTime(), max_date = maxTime()),
        data_content))
    )
    
    
  })
  
  
  observeEvent(input$btnReset, {
    #https://rdrr.io/cran/shinyjs/man/refresh.html
    refresh()
    if (F){
    output$shiny_time <- renderApexchart(date_chart(shiny_over_time(), "line", input$period))
    
    output$shiny_content <- renderApexchart(
      apex(
        data = shiny_content() %>% head(20), 
        type = "bar", 
        mapping = aes(title, n)
      ) %>%
        ax_title("By App (Top 20)") %>%
        set_input_click("content")
    )
    
    output$shiny_viewer <- renderApexchart(
      apex(
        data = shiny_viewers() %>% head(20), 
        type = "bar", 
        mapping = aes(username, n)
      ) %>%
        ax_title("By Viewer (Top 20)") %>%
        ax_colors("#2E9E5B") %>%            # green bars
        set_input_click("viewer")
    )
    
    output$shiny_owner <- renderApexchart(
      apex(
        data = shiny_owners() %>% head(20), 
        type = "bar", 
        mapping = aes(username, n)
      ) %>%
        ax_title("By Owner (Top 20)") %>%
        set_input_click("owner")
    )
    }
  })
  # Overview panel: counts by year ("All Time") or by month (other periods) ---------
  output$shiny_year <- renderApexchart({
    sel <- year_sel()
    filt <- sel$f
    d <- as.data.frame(filt(data_shiny))
    if (identical(input$period, "all")) {
      tb <- table(format(d$started, "%Y"))
      xlab <- names(tb)
      bar_cols <- year_colors(names(tb))   # same year colors as the By Date chart
      ttl <- "By Year"
    } else {
      b <- period_range()
      d <- as.data.frame(safe_filter(d, min_date = b$from, max_date = b$to))
      tb <- table(format(d$started, "%Y-%m"))     # sorted chronologically
      xlab <- format(as.Date(paste0(names(tb), "-01")), "%b %Y")
      bar_cols <- month_colors[as.integer(substr(names(tb), 6, 7))]
      ttl <- "By Month"
    }
    validate(need(length(tb) > 0, "No visits"))
    counts <- data.frame(period = xlab, n = as.integer(tb), stringsAsFactors = FALSE)
    ch <- apex(data = counts, type = "column", mapping = aes(period, n), auto_update = FALSE) %>%
      ax_title(paste0(ttl, " - ", sel$label)) %>%
      ax_dataLabels(enabled = TRUE)
    if (identical(input$period, "all")) ch <- ch %>% set_input_click("year_pick")
    {
      # same colors as the By Date chart: per month, or per year for "All Time"
      ch <- ch %>%
        ax_plotOptions(bar = bar_opts(distributed = TRUE)) %>%
        ax_colors(bar_cols) %>%
        ax_legend(show = FALSE)
    }
    ch
  })

  # "Admin" tab: only added for users listed in STATS_ADMIN ---------------------
  observeEvent(is_admin(), {
    req(is_admin())
    appendTab("tabs", tabPanel("Admin", value = "admin",
      h3("Admin"),
      p("Download the top 50 users for an app. Uses the Time Period selected on the Stats tab."),
      uiOutput("adminPanel")
    ))
  }, once = TRUE)

  # Admin: top 50 user list download ------------------------------------------
  is_admin <- reactive({
    # session$user is the logged-in Connect username (NULL when not logged in / local run)
    # STATS_ADMIN=* in .env means everyone (for local testing only!)
    "*" %in% stats_admins ||
      (!is.null(session$user) && tolower(session$user) %in% stats_admins)
  })

  top_users_csv <- function(guid) {
    b <- period_range()
    data_shiny %>%
      filter(content_guid == guid) %>%
      safe_filter(min_date = b$from, max_date = b$to) %>%
      left_join(
        data_users %>% select(any_of(c("guid", "username", "email", "first_name", "last_name"))),
        by = c(user_guid = "guid")
      ) %>%
      group_by(across(any_of(c("username", "email", "first_name", "last_name")))) %>%
      summarize(
        sessions = n(),
        total_time_minutes = round(sum(ActiveSecs, na.rm = TRUE) / 60, 1),
        .groups = "drop"
      ) %>%
      filter(!is.na(username)) %>%
      arrange(desc(total_time_minutes)) %>%
      head(50)
  }

  # Selected app for the download dialog
  dl_sel <- reactiveVal(NULL)   # list(guid=, title=)
  dl_registered <- character()  # click observers already created

  output$adminPanel <- renderUI({
    req(is_admin())
    apps <- shiny_content()
    req(nrow(apps) > 0)
    div(
      style = "max-height:260px; overflow-y:auto; margin:10px 15px; padding:8px 12px; border:1px solid #ddd; border-radius:4px;",
      tags$strong("Top 50 user lists (selected period)"),
      tags$ul(style = "list-style:none; padding-left:0; margin:6px 0 0 0;",
        lapply(seq_len(nrow(apps)), function(i) {
          guid <- apps$content_guid[i]
          ttl <- apps$title[i]
          id <- paste0("dlsel_", gsub("[^A-Za-z0-9]", "_", guid))
          if (!id %in% dl_registered) {
            dl_registered <<- c(dl_registered, id)
            observeEvent(input[[id]], {
              req(is_admin())
              dl_sel(list(guid = guid, title = ttl))
              showModal(modalDialog(
                title = ttl, easyClose = TRUE, size = "s",
                p("Top 50 users by time spent, for the selected period."),
                downloadButton("dl_top_users", "Download CSV", class = "btn-info"),
                footer = modalButton("Close")
              ))
            }, ignoreInit = TRUE)
          }
          tags$li(ttl, " ", actionLink(id, "[download top 50 user list]"))
        })
      )
    )
  })

  output$dl_top_users <- downloadHandler(
    filename = function() {
      sel <- dl_sel()
      paste0("top50_users_", gsub("[^A-Za-z0-9_-]+", "_", sel$title), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(is_admin(), dl_sel())
      write.csv(top_users_csv(dl_sel()$guid), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  observeEvent(input$btnHelp,{
    shinyalert(title="Help Information", size="m",closeOnClickOutside = TRUE,
    text=paste0(
    "1.	click bar in App panel  to show statistics for the selected App.\n\n",
    "2.	click bar in Viewer panel to show statistics for the selected User.\n\n",
    "3.	click bar in Owner panel to show statistics for App(s) of the selected Developer.\n\n",
    "4.	zoom-in Date panel: hold down the left mouse button, move the mouse,\nand release the button when the desired days are selected.\n\n",
    "5.	click [Reset] button to go back to the default setting.\n\n",
    "6. click [Help] button to show this message. Click [OK] or Outside to exit.\n\n",
    "*App fetches new data from IBEX server daily when the first visit happens.*\n"
    ))

  })
  # Graph output ----------------------------------------------------------
  # line
  
  output$shiny_time <- renderApexchart(date_chart(shiny_over_time(), "line", input$period))
  
  output$shiny_unique <- renderApexchart(
    unique_chart(unique_by_app(
      data_shiny %>% safe_filter(min_date = minTime(), max_date = maxTime()),
      data_content))
  )

  output$shiny_content <- renderApexchart(
    apex(
      data = shiny_content() %>% head(20), 
      type = "bar", 
      mapping = aes(title, n)
    ) %>%
      ax_title("By App (Top 20)") %>%
      set_input_click("content")
  )
  
  output$shiny_viewer <- renderApexchart(
    apex(
      data = shiny_viewers() %>% head(20), 
      type = "bar", 
      mapping = aes(username, n)
    ) %>%
      ax_title("By Viewer (Top 20)") %>%
      ax_colors("#2E9E5B") %>%            # green bars
      set_input_click("viewer")
  )
  
  output$shiny_owner <- renderApexchart(
    apex(
      data = shiny_owners() %>% head(20), 
      type = "bar", 
      mapping = aes(username, n)
    ) %>%
      ax_title("By Owner (Top 20)") %>%
      set_input_click("owner")
  )
  
  #output$verbatim <- renderText(capture.output(str(input$time), str(minTime()), str(maxTime()), str(input$content), str(input$viewer), str(input$owner)))
  
}

shinyApp(ui = ui, server = server)