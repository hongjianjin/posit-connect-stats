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
report_from <- lubridate::today() - lubridate::ddays(days_back)
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

ui <- dashboardPage(
  dashboardHeader(
    title = "IBEX Shiny App Usage", titleWidth = 0
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML("
      .main-sidebar, .sidebar-toggle { display: none !important; }
      .wrapper, .content-wrapper, .right-side { margin-left: 0 !important; background-color: #ecf0f5 !important; }
      .main-header .navbar { margin-left: 0 !important; }
      .main-header .logo { display: none !important; }
      .main-header .navbar:before { content: 'IBEX Shiny App Usage'; color: #fff; font-size: 20px; line-height: 50px; padding-left: 15px; }
    "))),
    fluidRow(
      column(4,
        selectInput(
          "period", "Time Period:",
          choices = c(
            "This Month" = "month",
            "Year to Date" = "ytd",
            "Last 6 Months" = "6m",
            "Last 1 Year" = "1y",
            "All Time" = "all"
          ),
          selected = "6m"
        )
      ),
      column(8,
        br(),
        textOutput("periodLabel")
      )
    ),
    fluidRow(
      shinycssloaders::withSpinner(
        apexchartOutput("shiny_time")
      )
    ),
    fluidRow(
      box(
        apexchartOutput("shiny_content"),
        width = 4
      ),
      box(
        apexchartOutput("shiny_viewer"),
        width = 4
      ),
      box(
        apexchartOutput("shiny_owner"),
        width = 4
      )
    )
    ,
    uiOutput("adminPanel"),
    fluidRow(width=12,
      column(12, align="center",
        shinyjs::useShinyjs(),
        useShinyalert(),
        actionButton("btnReset", label = "Reset",icon=icon("sync"), class = "btn-info" ),
        actionButton("btnHelp", label = "Help",icon=icon("question"), class = "btn-info")
    
      )
      #verbatimTextOutput("verbatim")
    )
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

server <- function(input, output, session) {
  
  
  # Data Prep -------------------------------------------------------------
  #data <- reactiveValues()
  
  # Must include "to" or the cache can get weird!!
  data_shiny <- cached_usage_shiny(client, from = report_from, to = report_to, limit = Inf)
  data_shiny$ActiveSecs <- as.numeric(difftime(data_shiny$ended,data_shiny$started,unit='secs'))
  data_shiny <- data_shiny[data_shiny$ActiveSecs>5, ]  # remove visits < 5 seconds
  # data_static <- cached_usage_static(client, from = report_from, to = report_to, limit = Inf) # ~ 3 minutes on a busy server...
  data_content <- cached_get_content(client, date = report_to)
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

    output$shiny_time <- renderApexchart(
      apexchart(auto_update = FALSE) %>%
        ax_chart(type = "line") %>%
        ax_title("By Date") %>%
        ax_plotOptions() %>%
        ax_series(list(
          name = "Count",
          data = purrr::map2(shiny_over_time1()$date_disp, shiny_over_time1()$n, ~ list(.x,.y))
        )) %>%
        ax_xaxis(
          type = "datetime"
        )
         %>% set_input_selection("time")
    )
    
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
        set_input_click("viewer")
    )
    
  })
  

  # ----------------------------------------------------------------------
  
  observeEvent(input$viewer, {
    viewerName <- as.character(input$viewer)
    
    selectedGuids <- viewerDF$content_guid[viewerDF$username==viewerName]
    userGuids <- unique(unlist(viewerDF$guid[viewerDF$username==viewerName]))
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
    
    output$shiny_time <- renderApexchart(
      apexchart(auto_update = FALSE) %>%
        ax_chart(type = "bar") %>%
        ax_title("By Date") %>%
        ax_plotOptions() %>%
        ax_series(list(
          name = "Count",
          data = purrr::map2(shiny_over_time2()$date_disp, shiny_over_time2()$n, ~ list(.x,.y))
        )) %>%
        ax_xaxis(
          type = "datetime"
        )
      %>% set_input_selection("time")
    )
    
    
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
    
    
  })
  
  # ----------------------------------------------------------------------
  
  observeEvent(input$owner, {
    ownerName <- as.character(input$owner)
    selectedGuids <- unlist(appDF$guid[appDF$username==ownerName])
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
    
    output$shiny_time <- renderApexchart(
      apexchart(auto_update = FALSE) %>%
        ax_chart(type = "bar") %>%
        ax_title("By Date") %>%
        ax_plotOptions() %>%
        ax_series(list(
          name = "Count",
          data = purrr::map2(shiny_over_time1()$date_disp, shiny_over_time1()$n, ~ list(.x,.y))
        )) %>%
        ax_xaxis(
          type = "datetime"
        )
      %>% set_input_selection("time")
    )
    
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
    
    
  })
  
  
  observeEvent(input$btnReset, {
    #https://rdrr.io/cran/shinyjs/man/refresh.html
    refresh()
    if (F){
    output$shiny_time <- renderApexchart(
      apexchart(auto_update = FALSE) %>%
        ax_chart(type = "line") %>%
        ax_title("By Date") %>%
        ax_plotOptions() %>%
        ax_series(list(
          name = "Count",
          data = purrr::map2(shiny_over_time()$date_disp, shiny_over_time()$n, ~ list(.x,.y))
        )) %>%
        ax_xaxis(
          type = "datetime"
        ) %>%
        set_input_selection("time")
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
  # Admin: top 50 user list download ------------------------------------------
  is_admin <- reactive({
    # session$user is the logged-in Connect username (NULL when not logged in / local run)
    # STATS_ADMIN=* in .env means everyone (for local testing only!)
    "*" %in% stats_admins ||
      (!is.null(session$user) && tolower(session$user) %in% stats_admins)
  })

  # One real download link per app (each gets its own download output)
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
          id <- paste0("dl_", gsub("[^A-Za-z0-9]", "_", guid))
          output[[id]] <- downloadHandler(
            filename = function() {
              paste0("top50_users_", gsub("[^A-Za-z0-9_-]+", "_", ttl), "_", Sys.Date(), ".csv")
            },
            content = function(file) {
              req(is_admin())
              write.csv(top_users_csv(guid), file, row.names = FALSE)
            },
            contentType = "text/csv"
          )
          tags$li(ttl, " ", downloadLink(id, "[download top 50 user list]"))
        })
      )
    )
  })

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
  
  output$shiny_time <- renderApexchart(
    apexchart(auto_update = FALSE) %>%
      ax_chart(type = "line") %>%
      ax_title("By Date") %>%
      ax_plotOptions() %>%
      ax_series(list(
        name = "Count",
        data = purrr::map2(shiny_over_time()$date_disp, shiny_over_time()$n, ~ list(.x,.y))
      )) %>%
      ax_xaxis(
        type = "datetime"
      ) %>%
      set_input_selection("time")
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