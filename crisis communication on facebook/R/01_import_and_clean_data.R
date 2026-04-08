# this script was prepared as part of a postdoctoral application to the university of amsterdam
# it shows a compact cleaning workflow for public facebook data from norwegian politicians
# much of the workflow builds on routines developed during the doctoral dissertation

library(tidyverse)
library(readxl)
library(janitor)
library(stringr)

# note
# the original platform export is not shared in this repository
# if the raw file is available locally, place it in the data folder and update the file name below

input_file <- "data/facebook_politik.xlsx"

facebook <-
  read_xlsx(input_file) %>%
  select(1:24) %>%
  clean_names() %>%
  filter(page_category == "POLITICIAN") %>%
  filter(page_admin_top_country == "NO")

facebook_first_lockdown <-
  facebook %>%
  filter(
    post_created_date >= as.Date("2020-03-12"),
    post_created_date <= as.Date("2020-05-12")
  )

facebook_first_lockdown <-
  facebook_first_lockdown %>%
  mutate(
    page_name = str_squish(page_name),
    page_name = str_remove(page_name, " -.*$"),
    user_name = na_if(user_name, ""),
    user_name = if_else(
      is.na(user_name) & page_name == "Trygve Slagsvold Vedum",
      "trygve",
      user_name
    )
  )

facebook_first_lockdown <-
  facebook_first_lockdown %>%
  mutate(
    likes_at_posting = as.numeric(likes_at_posting),
    followers_at_posting = as.numeric(followers_at_posting),
    post_created_date = as.Date(post_created_date),
    facebook_id = as.character(facebook_id)
  )

facebook_first_lockdown %>%
  glimpse()

# optional export for the next step
# write_csv(facebook_first_lockdown, "data/facebook_first_lockdown.csv")
