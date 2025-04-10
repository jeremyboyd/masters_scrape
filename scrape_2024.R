# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Scrape Masters leaderboard data every 60 seconds and write to a
# Google sheet.

# Packages
library(tidyverse)
library(RSelenium)          # Selenium server
library(rvest)              # Web scraping
library(googlesheets4)
library(httpuv)
library(lubridate)

# Interactive authorization puts token in secrets. This only has to be done
# once when the script is first run.
# gs4_auth(email = "kenyonboyd@gmail.com", cache = ".secrets")
# gs4_deauth()

# Scrape this page
url <- "https://www.masters.com/en_US/scores/index.html"
# url <- "https://2022.masters.com/en_US/scores/index.html"

# Authorize access to Google sheets
gs4_auth(email = "kenyonboyd@gmail.com",
         cache = ".secrets")

# Start Docker
system("open --background -a Docker", wait = TRUE)
Sys.sleep(40)
message("Finished starting Docker.")

# Start Selenium container on Docker
system("docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1",
       wait = TRUE)
message("Finished starting Selenium server.")

# Connect to server
remDr <- remoteDriver(port = 4445L)
Sys.sleep(5)
remDr$open()
Sys.sleep(5)
message("Finished connecting to server.")

# Navigate to URL and wait for page to load
remDr$navigate(url)
Sys.sleep(5)
message(paste0("Finished navigating to ", url, "."))



remDr$navigate("http://www.bbc.co.uk")
remDr$getCurrentUrl()
remDr$getPageSource()
webElems <- remDr$findElements(using = "xpath", value = '//*[contains(concat( " ", @class, " " ), concat( " ", "fJHhtM", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "oinwT", " " ))]')
html <- remDr$getPageSource() %>%
    .[[1]] %>%
    read_html() %>%
    html_text2()

b <- ChromoteSession$new()
b$view()
b$Page$navigate("https://www.masters.com/en_US/scores/index.html")


# This function is supposed to convert javascript pages to html automatically
# Try to read in leaderboard object using its xpath. Looks like we're getting a nodeset back from read_html_live(), but not able to find elements.
live <- rvest::read_html_live("https://www.masters.com/en_US/scores/index.html")  |>
    html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "center_cell", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "navigation_down_arrow", " " ))]')

# This is working to capture the two "over" elements
over <- rvest::read_html_live("https://www.masters.com/en_US/scores/index.html")  |>
    html_elements(css = ".over") |>
    html_text2()

# Data elements. Selectorgadget is showing all data elements in the leaderboard are marked with .data. So why isn't this working when .over does?

# Extract .data elements
data.elem <- data.live$html_elements(css = ".data")

# Convert to text. This works, but coming from the over/under board
lb.string <- data.elem |>
    html_text2()

# Can I extract leaderboard with this method?
elem.lb <- data.live$html_elements(css = "#leaderboard")

# Can't directly parse table from leaderboad
table.lb <- elem.lb |>
    html_table()

# Can I get drop downs? Yes.
menu.elem <- data.live$html_elements(css = ".center_cell .navigation_down_arrow")
menu.text <- menu.elem |>
    html_text2()

# View of page. Looks like it's possible to manually change to traditional leaderboard from view
data.live$view()

# Click drop down, then click 2nd option (traditional)
# data.live$click(css = ".center_cell .navigation_down_arrow", n_clicks = 1)
# data.live$click(css = ".option:nth-child(2) .option-name", n_clicks = 1)


# This object is live version of the page
page_live <- rvest::read_html_live("https://www.masters.com/en_US/scores/index.html")

# Check out live view and manually switch leaderboad to Traditional view if
# necessary.
page_live$view()

# Scrape leaderboard every 60 seconds and write to Google sheet
while (TRUE) {
    
    # User message
    message(paste("Initiating leaderboard refresh at", Sys.time()))
    
    # Read in text from live page
    leader_char <- page_live$html_elements(".data") |>
        html_text2()
    
    # Convert character string to a table by first finding the indices of player
    # names. Player names are considered to be at least two alpha characters,
    # but not MC (missed cut), WD (withdrawn), or GMT (Greenwich Mean Time,
    # giving tee times for players who haven't started yet). Requiring at least
    # two characters allows us to miss T (used for ties, like tied for third,
    # "T3") and F (finished).
    name_idx <- which(str_detect(leader_char, "^[A-Za-z]{2}") &
                          !str_detect(leader_char, "^(MC|WD|GMT)"))
    
    # Based on where player names occur, we can compute the indices for the
    # start and end of each row in the table.
    start <- name_idx - 1
    end <- start + 9
    
    # Iterate over start & end indices to extract rows and combine into a table
    leader_tab <- map2(start, end, function(s, e) {
        as_tibble(leader_char[s:e]) }) %>%
        list_cbind() %>%
        t() %>%
        as_tibble()
    
    # Add column names
    colnames(leader_tab) <- c("place", "player", "total_under", "thru", "today_under", "R1", "R2", "R3", "R4", "total_score")
    
    # If the place is either MC or WD then only keep place, player, R1, R2, R3,
    # and total score.
    mc_wd <- leader_tab %>%
        filter(place %in% c("MC", "WD")) %>%
        select(place, player, R1 = thru, R2 = today_under, R3 = R2,
               total_score = R3)
    
    # Players who haven't started have a start time with "GMT" in the thru col
    not_started <- leader_tab %>%
        filter(!place %in% c("MC", "WD"),
               str_detect(thru, "GMT")) %>%
        select(place:total_under, R1 = today_under, R2 = R1, R3 = R2, R4 = R3,
               total_score = R4) %>%
        mutate(across(R1:total_score, ~ if_else(.x == "", NA_character_, .x)))
    
    # If the place isn't MC or WD and thru doesn't have "GMT" then player has
    # started today's round.
    started <- leader_tab %>%
        filter(!place %in% c("MC", "WD", ""),
               !str_detect(thru, "GMT"))
    
    # Final leaderboard
    leaderboard <- bind_rows(
        started,
        not_started,
        mc_wd) %>%
        
        # Convert to numeric
        mutate(
            across(c("total_under", "today_under"),
                   ~ if_else(.x == "E", "0", .x)),
            across(
                c("total_under", "today_under", R1:total_score),
                ~ as.integer(.x)),
            
            # Add a datetime stamp. Google sheets converts all datetimes to UTC,
            # so subtract six hours to show mountain time.
            last_updated = Sys.time() - hours(6),
            
            # Need these columns to properly sort the leaderboard
            place2 = as.integer(str_extract(place, "[0-9]+")),
            thru2 = if_else(
                thru == "F"
                ,99
                ,as.integer(str_extract(thru, "[0-9]+")))) %>%
        arrange(place2, desc(thru2)) %>%
        select(-place2, -thru2)
    
    # Write to Google sheet. If there's an error, print message & continue.
    tryCatch(
        write_sheet(
            data = leaderboard,
            ss = "https://docs.google.com/spreadsheets/d/1bf5BZ46053tmu45wDafrFUM3nx2aD97VNnEPjn8sy4o/edit#gid=0",
            sheet = "leaderboard"),
        error = function(e) {
            message("Error in write_sheet()")
            print(e) })
    
    # Pause 60 seconds before running loop again
    message("Waiting for next loop...")
    Sys.sleep(60)
}
