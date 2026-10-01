# IBEX Shiny App Usage

A Shiny dashboard that shows how the Shiny apps on the IBEX Posit Connect server are used: visits over time, and the top apps, viewers and owners. Admins can also download a top-50 user list for each app.

## Files

| File | Purpose |
|------|---------|
| `app.R` | The dashboard (UI + server). |
| `run_test.R` | Stand-alone script for pulling top users from the command line. |
| `.env` | Local settings and secrets. **Not committed** (see `.gitignore`). |
| `.env.example` | Template for `.env`. |

## Setup

1. Install the R packages:

   ```r
   install.packages(c(
     "shiny", "shinyjs", "shinyalert", "shinydashboard", "apexcharter",
     "connectapi", "dplyr", "shinycssloaders", "memoise", "lubridate",
     "purrr", "glue", "fontawesome"
   ))
   ```

2. Copy `.env.example` to `.env` and fill in the values:

   ```
   CONNECT_SERVER=http://ibex.stjude.org/
   CONNECT_API_KEY=your-api-key
   STATS_ADMIN=admin1,admin2
   ```

   | Variable | Meaning |
   |----------|---------|
   | `CONNECT_SERVER` | URL of the Posit Connect server. |
   | `CONNECT_API_KEY` | API key of a Connect user who can read usage data (typically an administrator or publisher). |
   | `STATS_ADMIN` | Connect usernames, separated by commas or semicolons, who may download the top-user lists. `*` means everyone: **use only for local testing**. |

3. Run the app from this folder:

   ```r
   shiny::runApp()
   ```

## Using the dashboard

- **Time Period** (This Month, Year to Date, Last 6 Months, Last 1 Year, All Time) filters all charts. Data is fetched for the last 1095 days, which is the limit for "All Time".
- **By Date** shows visits per day for the selected period.
- **By App / By Viewer / By Owner** show the top 20. Click a bar to focus the other charts on that app, viewer or owner. Visits shorter than 5 seconds and visits by an app's own developer are excluded.
- Drag across **By Date** to zoom in on specific days.
- **Reset** returns to the default view. **Help** shows these instructions.
- New data is fetched from the server on the first visit each day.

### Top 50 user list (admins only)

For users listed in `STATS_ADMIN`, a **Top 50 user lists** panel appears below the charts with one entry per app in the selected period. Click **[download top 50 user list]** for an app, then **Download CSV** in the dialog. The file contains username, email, name, number of sessions and total minutes, sorted by time spent.

An admin is identified by their Connect login, so the app must require login in Connect's access settings. When running locally nobody is logged in, so use `STATS_ADMIN=*` to test.

## Deploying to Posit Connect

1. Publish the app (for example with RStudio or `rsconnect`).
2. Make sure the three settings are available to the app: either include `.env` in the bundle or set `CONNECT_SERVER`, `CONNECT_API_KEY` and `STATS_ADMIN` in the app's Vars panel.
3. Set `STATS_ADMIN` to real usernames before publishing, not `*`.
4. Set the app's access to require login, then restart the app process after each deployment.

## Security notes

- Never commit `.env` or paste API keys into code. Rotate any key that was ever committed or shared.
- The top-user download contains email addresses, so keep `STATS_ADMIN` limited.

## Troubleshooting

| Symptom | Fix |
|---------|-----|
| `Argument N can't be empty` | A function call in the UI has a stray trailing comma. |
| Admin panel does not appear on the server | The app does not require login, or the Connect username differs from `STATS_ADMIN`. |
| Changes do not appear on the server | Redeploy and restart the app process. |
