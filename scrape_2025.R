# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Simpler method of getting Masters leaderboard data. Instead of
# scraping, download the json file that feeds into the leaderboard.

# Packages
library(tidyverse)
library(httr2)
library(jsonlite)

# Authorize access to Google sheets
gs4_auth(email = "kenyonboyd@gmail.com",
         cache = ".secrets")

# Presumably once the tournament starts there will be a scores.json to download.
# Will need to check this at tee off on Thursday. Until that happens I can work
# with the scores.json fro 2024.
url <- "https://www.masters.com/en_US/scores/feeds/2025/scores.json"
# url <- "https://www.masters.com/en_US/scores/feeds/2024/scores.json"

# Download leaderboard data every 60 seconds, format, and write to sheet
while (TRUE) {
    
    # User message
    message(paste("Initiating leaderboard refresh at", Sys.time()))

    # Default user-agent for httr2 indicates that request is coming from
    # httr2/curl/libcurl and not a browser. This gets blocked by the Masters site.
    # So pretend to be a browser using HTTP/1.1. HTTP/1.1 is simpler, doesn't look
    # like anything out of the ordinary and so gets accepted by masters.
    resp <- request(url) |>
        req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/122.0.0.0 Safari/537.36") |>
        req_options(http_version = 1.1) |>  # Force HTTP/1.1
        req_perform()
    
    # Extract data
    data <- resp |> resp_body_json()
    
    # Loop over players to organize their data into leaderboard
    leaderboard <- map(1:length(data$data$player), function(player) {
        
        # Data list for current player
        d <- data$data$player[player][[1]]
        
        # Change any null values for round scores to ""
        if(is.null(d$round1$total)) {
            r1 <- ""
        } else { r1 <- d$round1$total }
        if(is.null(d$round2$total)) {
            r2 <- ""
        } else { r2 <- d$round2$total }
        if(is.null(d$round3$total)) {
            r3 <- ""
        } else { r3 <- d$round3$total }
        if(is.null(d$round4$total)) {
            r4 <- ""
        } else { r4 <- d$round4$total }
        
        # Table
        tibble(
            amateur = d$amateur,
            place = d$pos,
            player = d$display_name,
            total_under = d$topar,
            thru = d$thru,
            today_under = d$today,
            R1 = r1,
            R2 = r2,
            R3 = r3,
            R4 = r4,
            total_score = d$total,
            last_updated = Sys.time()) |>
            mutate(
                
                # Change E scores to 0
                across(matches("_under$"), ~ if_else(.x == "E", "0", .x)),
                
                # Make all score cols integer. Do it at this stage so that when
                # rbind occurs all of the rows being bound are same data type.
                # Otherwise you can get an error in cases like the following:
                # when R1 is empty string for players who haven't finished &
                # integer for those who have.
                across(matches("_under$|^R|total_score"),
                       ~ as.integer(.x))) }) |>
        list_rbind() |>
        
        # Change datetime formatting to make it more readable & keep Google from
        # messing with it when writing to Google sheet.
        mutate(last_updated = as.character(ymd_hms(last_updated)),
               last_updated = str_remove(last_updated, "\\..+"),
               last_updated = paste(last_updated, "MDT"),
               
               # Add amateur status indicator to player name
               player = if_else(amateur == TRUE, paste(player, "(A)"),
                                player)) |>
        select(place:last_updated)
    
    # Write to Google sheet. If there's an error, print message & continue.
    tryCatch(
        write_sheet(
            data = leaderboard,
            ss = "https://docs.google.com/spreadsheets/d/10XAgbnCOw3FAcGrTO91-Igr_b-SLBA-4mmVf19Hx1kM/edit?gid=0#gid=0",
            sheet = "leaderboard"),
        error = function(e) {
            message("Error in write_sheet()")
            print(e) })
    
    # Pause 60 seconds before running loop again
    message("Waiting for next loop...")
    Sys.sleep(60)
}
