library(tidyverse)
library(httr2)
library(rvest)
library(stringr)

league_ID <- "8ee61976f8e24ce29d07edb3d6b9fd2a/"
url_start <- "https://app.musicleague.com/l/"
url_base <- paste0(url_start, league_ID)
cookie <- paste0("app.musicleague.com=", read_file("cookies.txt"))

# Extract round id ----
rounds_df <-
  request(paste0(url_base, "-/rounds")) %>%
  req_headers(Cookie = cookie) %>%
  req_perform() %>% 
  resp_body_string() %>% 
  read_html() %>% 
  html_elements("div.league-round-item") %>% 
  map_dfr(function(node) {
    
    xdata <- html_attr(node, "x-data")
    
    round_number <- node %>%
      html_element("span.card-text.text-body-tertiary") %>%
      html_text(trim = TRUE) %>%
      stringr::str_extract("\\d+") %>%
      as.integer()
    
    round_name <- node %>%
      html_element("h5.card-title") %>%
      html_text(trim = TRUE)
    
    tibble(
      round_id = html_attr(node, "id"),
      round_number = round_number,
      round_name = round_name,
      status = xdata %>%
        stringr::str_extract("status:\\s*'[^']+'") %>%
        stringr::str_extract("'[^']+'") %>%
        stringr::str_remove_all("'")
    )
  })

# scrapping ----
all_votes <-
  rounds_df %>%
  filter(status == "COMPLETE") %>%
  mutate(url = paste0(url_base, round_id, "/-/results")) %>%
  pmap_dfr(function(round_id, round_number, round_name, status, url) {
    
    doc <- request(url) |>
      req_headers(Cookie = cookie) |>
      req_perform() |>
      resp_body_string() |>
      read_html()

    doc %>%
      html_elements("div.card.mb-4") %>%
      map_dfr(function(node) {

        # --- submission-level ---
        submitter <- node %>%
          html_element(".card.mt-3 h6.fw-semibold") %>%
          html_text(trim = TRUE)
        
        submission_text <- node %>%
          html_element(".card-body .text-break") %>%
          html_text2()

        song <- node %>%
          html_element(".card-title a") %>%
          html_text(trim = TRUE)

        artist <- node %>%
          html_elements(".card-text") %>%
          .[[1]] %>%
          html_text(trim = TRUE)

        album <- node %>%
          html_elements(".card-text") %>%
          .[[2]] %>%
          html_text(trim = TRUE)

        score <- node %>%
          html_element("h3") %>%
          html_text(trim = TRUE) %>%
          as.numeric()

        # --- votes ---
        votes <- node %>%
          html_elements(".card-footer .row.align-items-start")

        map_dfr(votes, function(v) {

          tibble(
            # round_id = round_id,
            round_number = round_number,
            round_name = round_name,
            
            submitter = submitter,
            song = song,
            artist = artist,
            album = album,
            score = score,
            submission_text = submission_text,
            
            voter = v %>% html_element("b") %>% html_text(trim = TRUE),
            vote  = v %>% html_element("h6") %>% html_text(trim = TRUE) %>% as.numeric(),
            comment = v %>% html_element("span") %>% html_text(trim = TRUE)
          )

        })

      })
    
  })

saveRDS(all_votes, "all_votes.rds")