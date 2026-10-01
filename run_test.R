library(shiny)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(apexcharter)
library(connectapi)
library(dplyr)
library(shinycssloaders)
#install.packages("fontawesome") to solve 

Sys.setenv("CONNECT_SERVER" = "http://ibex.stjude.org/")
## ACTION REQUIRED: Make sure to have your API key ready
Sys.setenv("CONNECT_API_KEY" = "bDCdvgfvOxLObYzw7eAiIegiAgVj6uDo")

client <- connect()

Sys.setenv("DAYSBACK"=180)
days_back <- as.numeric(Sys.getenv("DAYSBACK"))
#days_back <-  as.numeric(difftime(lubridate::today(), "2022-01-01","Days"))
cache_location <- Sys.getenv("MEMOISE_CACHE_LOCATION", tempdir())
message(cache_location)
mydate <- lubridate::today() # as.Date("2024-12-31") # 
report_from <- mydate - lubridate::ddays(days_back)
report_to <- mydate
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


table(all_content[3])
# 2. Identify your target application by name (or use the GUID directly)

all_content <- cached_get_content(client, mydate)
table(all_content[3])
target_app_name <- "RNAseqV2.3X" 
target_app <- all_content %>% dplyr::filter(name == target_app_name) %>% dplyr::pull(guid)

if (length(target_app) == 0) stop("Application not found.")

# 3. Fetch the usage data using your memoized function
# Note: days_back needs to be defined (e.g., days_back <- 30)
usage_data <- cached_usage_shiny(
  client, 
  content_guid = target_app, 
  from = report_from, 
  to = report_to
)

# 4. Process the data to find the Top 20 Users
top_20_users <- usage_data %>%
 dplyr::left_join(cached_get_users(client, mydate, limit = Inf), by = c("user_guid" = "guid")) %>%
  dplyr::group_by(email) %>%
  dplyr::summarize(
    total_sessions = dplyr::n(),
    # Duration is usually in seconds in the Connect API
    total_time_minutes = sum(as.numeric(difftime(ended, started, units = "mins")), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(desc(total_time_minutes)) %>%
  dplyr::slice_head(n = 20)

# 5. Output the result
message(paste("Top 20 users for", target_app_name, "since", report_from))
print(as.data.frame(top_20_users))
cat(paste(top_20_users$email,collapse = ";"))

#====================================================
usage_data_all <- cached_usage_shiny(
  client, 
  from = report_from, 
  to = report_to
)

# 3. Fetch user metadata to map GUIDs to real names
all_users <- cached_get_users(client, mydate, limit = Inf)

# 4. Process and Aggregate
top_50_users <- usage_data_all %>%
  # Join to get user details
  dplyr::left_join(all_users, by = c("user_guid" = "guid")) %>%
  # Group by user (using username/display_name for clarity)
  dplyr::group_by(email) %>%
  dplyr::summarize(
    unique_apps_visited = dplyr::n_distinct(content_guid),
    total_sessions = dplyr::n(),
    total_time_hours = sum(as.numeric(difftime(ended, started, units = "hours")), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Filter out potential NAs (anonymous users in open apps)
  dplyr::filter(!is.na(email)) %>% 
  # Keep only repeat users (2 or more sessions)
  dplyr::filter(total_sessions >= 2) %>%
  # Sort by time spent
  dplyr::arrange(desc(total_time_hours)) %>%
  # Get top 50
  dplyr::slice_head(n = 50)

# 5. Output
message(paste("Top 50 Server Users since", report_from))
print(as.data.frame(top_50_users))

cat(paste(top_50_users$email,collapse = ";"))
