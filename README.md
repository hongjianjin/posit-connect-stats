# Posit Connect Usage Insights

A Shiny dashboard that shows how the Shiny apps on a Posit Connect server are used: visits over time, and the top apps, viewers and owners. Admins can also download a top-50 user list for each app.

## Highlights

- **Click any bar to drill down.** You can click each bar in **By App**, **By Unique User**, **By Viewer** and **By Owner**, and in **By Year** when the Time Period is **All Time**, to dynamically show the specific details:
  - clicking an app, viewer or owner focuses the date chart, the overview and the other panels on it;
  - clicking a year in By Year switches the Time Period to that year.
- Press **Reset** to return to the default view.

## Files

| File | Purpose |
|------|---------|
| `app.R` | The dashboard (UI + server). |
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
   CONNECT_SERVER=https://connect.example.org/
   CONNECT_API_KEY=your-api-key
   STATS_ADMIN=admin1,admin2
   STATS_START_DATE=2021-01-01
   ```

   | Variable | Meaning |
   |----------|---------|
   | `CONNECT_SERVER` | URL of the Posit Connect server. |
   | `CONNECT_API_KEY` | API key of a Connect **administrator**, so every app and its usage is visible (see the FAQ). |
   | `STATS_ADMIN` | Connect usernames, separated by commas or semicolons, who may download the top-user lists. `*` means everyone: **use only for local testing**. |
   | `STATS_START_DATE` | Optional. First day of data to load, as `YYYY-MM-DD`. Defaults to `2021-01-01`. |

3. Run the app from this folder:

   ```r
   shiny::runApp()
   ```

## Using the dashboard

The page has tabs along the top: **Stats** (the default, described below) and **Admin** (shown only to users in `STATS_ADMIN`; see "Top 50 user list").

- **Time Period** filters all charts. Choices: This Month, Year to Date, Last 6 Months, Last 1 Year, All Time, and one entry for each previous calendar year that has data (for example 2025, 2024). The current year is covered by Year to Date. Data is fetched from 2021-01-01 (the Connect server setup); set `STATS_START_DATE` in `.env` to change the start date.
- In **All Time**, click a bar in **By Year** to switch the Time Period to that year (the current year's bar selects Year to Date). To go back, choose All Time in the dropdown or press Reset.
- The top row is split 2/3 + 1/3:
  - **By Date** (left, 2/3) shows visits per day.
    - For periods other than All Time it is a bar chart with bars colored by month. Drag across it to zoom in on specific days.
    - For **All Time** it is a stacked bar chart over January to December, with one segment per year: the earliest year is at the bottom and the current year on top. Each year has a fixed color, shared with the By Year overview.
  - **Overview** (right, 1/3) shows vertical bars.
    - **By Month** for periods other than All Time, using the same month colors as By Date.
    - **By Year** for All Time, using the same year colors as By Date.
- **By App / By Unique User / By Viewer / By Owner** show the top 20. **By Viewer** uses green bars. **By Unique User** (purple bars) ranks apps by the number of distinct viewers rather than total visits; anonymous visits are ignored. Click a bar to focus the date and overview charts, and the other panels, on that app, viewer or owner; the overview title shows the selection, for example `By Year - <App Name>` or `By Month - <App Name>` (`All Apps` by default). Visits shorter than 5 seconds and visits by an app's own developer are excluded.
- **Reset** returns to the default view. **Help** shows these instructions.
- New data is fetched from the server on the first visit each day.
- Value labels and animations are turned off on the By Date chart to keep it fast.

### Top 50 user list (admins only)

For users listed in `STATS_ADMIN`, an **Admin** tab appears next to the Stats tab. It lists one entry per app in the Time Period selected on the Stats tab. Click **[download top 50 user list]** for an app to open a dialog, then click **Download CSV**. The file contains username, email, name, number of sessions and total minutes, sorted by time spent, for the selected period.

An admin is identified by their Connect login, so the app must require login in Connect's access settings. When running locally nobody is logged in, so use `STATS_ADMIN=*` to test.

## Deploying to Posit Connect

1. Publish the app (for example with RStudio or `rsconnect`).
2. Make sure the three settings are available to the app: either include `.env` in the bundle or set `CONNECT_SERVER`, `CONNECT_API_KEY` and `STATS_ADMIN` in the app's Vars panel.
3. Set `STATS_ADMIN` to real usernames before publishing, not `*`.
4. Set the app's access to require login, then restart the app process after each deployment.

```
setwd("/path/to/this/app")
rsconnect::deployApp(
    appDir = ".",
    appId = "<content-guid>",
    account = "<rsconnect-account-name>",
    server = "<connect-server-name>",
    forceUpdate = TRUE
)

```

## Security notes

- Never commit `.env` or paste API keys into code. Rotate any key that was ever committed or shared.
- The top-user download contains email addresses, so keep `STATS_ADMIN` limited.

## Troubleshooting

| Symptom | Fix |
|---------|-----|
| `Argument N can't be empty` | A function call in the UI has a stray trailing comma. |
| Admin panel does not appear on the server | The app does not require login, or the Connect username differs from `STATS_ADMIN`. |
| Changes do not appear on the server | Redeploy and restart the app process. |

## FAQ

**An app, and its owner, are missing from the dashboard. Why?**

The dashboard reads Connect through the API key in `.env` (`CONNECT_API_KEY`), so it only sees apps and usage data that the key's user is allowed to see. If an app has not been shared with that user, Connect shows "This content hasn't been shared with you" for the key's user, and the dashboard cannot list the app. Its owner is also missing from **By Owner**, because there is no content record to link the owner to.

Two ways to fix it:

1. **Use an administrator's API key (recommended).** An admin key sees every app and its usage, so no app is missed. Put the key in `CONNECT_API_KEY` and restart the app.
2. **Ask the owner to share the app** with the key's user as a viewer or collaborator. This fixes only that app.

After changing the key, Connect data is cached daily, so the missing app may not appear until the next day or until the cache is cleared.

**An app is still missing even though the key can see it.**

Check these:
- **Developer visits are excluded.** Visits by an app's own owner are removed. An app that only its owner opens has no visits left.
- **Visits under 5 seconds are excluded.**
- **By App, By Viewer and By Owner show only the top 20** for the selected Time Period. Try All Time.
- To check a specific app directly, use `connectapi` in an R console: `connectapi::get_content(client)` to confirm the key can see it, and `connectapi::get_usage_shiny(client, content_guid = <guid>)` to inspect its raw visit records.
