# Why is an app / owner missing from the dashboard?
# Usage: set `guid` (and optionally `owner_pattern`), then source this file from the app folder.
library(connectapi); library(dplyr)

guid          <- "00a08617-dce7-497b-b922-f463c434c58b"
owner_pattern <- "lei"        # part of the owner's username / name, case-insensitive
days_back     <- 1095

if (file.exists(".env")) readRenviron(".env")
for (v in c("CONNECT_SERVER", "CONNECT_API_KEY")) do.call(Sys.setenv, setNames(list(trimws(Sys.getenv(v))), v))
client <- connect()

cat("\n== 1. Is the app returned by get_content()? ==\n")
content <- get_content(client)
app <- content %>% filter(guid == !!guid)
cat("rows:", nrow(app), "\n")
if (nrow(app)) print(app %>% select(any_of(c("guid", "name", "title", "owner_guid", "access_type", "app_mode"))))

cat("\n== 2. Owner ==\n")
users <- get_users(client, limit = Inf)
print(users %>% filter(grepl(owner_pattern, paste(username, first_name, last_name), ignore.case = TRUE)) %>%
        select(any_of(c("guid", "username", "first_name", "last_name", "email"))))
if (nrow(app)) print(users %>% filter(guid == app$owner_guid) %>% select(any_of(c("guid", "username"))))

cat("\n== 3. Usage records for the app ==\n")
usage <- get_usage_shiny(client, content_guid = guid, from = Sys.Date() - days_back, to = Sys.Date(), limit = Inf)
cat("raw visits:", nrow(usage), "\n")
if (nrow(usage)) {
  usage$secs <- as.numeric(difftime(usage$ended, usage$started, units = "secs"))
  cat("visits longer than 5 s:", sum(usage$secs > 5, na.rm = TRUE), "\n")
  if (nrow(app)) {
    cat("visits by the owner (removed as 'developer' visits):", sum(usage$user_guid == app$owner_guid, na.rm = TRUE), "\n")
    cat("visits left after both filters:", sum(usage$secs > 5 & usage$user_guid != app$owner_guid, na.rm = TRUE), "\n")
  }
}
