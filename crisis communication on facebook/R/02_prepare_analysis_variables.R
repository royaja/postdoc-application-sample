# this script was prepared as part of a postdoctoral application to the university of amsterdam
# it prepares a smaller set of variables for the analysis sample
# the variable logic follows the broader dissertation workflow in a reduced form

library(tidyverse)
library(stringr)

input_file <- "data/facebook_first_lockdown.csv"

facebook_first_lockdown <-
  read_csv(input_file, show_col_types = FALSE)

covid_terms <- c(
  "covid",
  "korona",
  "corona",
  "pandemi",
  "smitte",
  "smittevern",
  "karantene",
  "isolasjon",
  "restriksjon",
  "restriksjoner",
  "nedstenging",
  "stengning",
  "test",
  "testing",
  "fhi",
  "folkehelseinstituttet",
  "helsedirektoratet",
  "avstand",
  "meter",
  "tiltak"
)

covid_pattern <- paste(covid_terms, collapse = "|")

facebook_prepared <-
  facebook_first_lockdown %>%
  mutate(
    post_type_simple = case_when(
      type == "Status" ~ "Status post",
      type == "Link" ~ "Link post",
      type == "Photo" ~ "Photo post",
      type %in% c("Native Video", "Video", "YouTube") ~ "Video post",
      type %in% c("Live Video Complete", "Live Video Scheduled") ~ "Live video",
      TRUE ~ "Other"
    ),
    log_total_interactions = log1p(total_interactions),
    log_followers_at_posting = log1p(followers_at_posting),
    weekday = weekdays(as.Date(post_created_date)),
    message_lower = str_to_lower(as.character(message)),
    covid_keyword = if_else(
      str_detect(coalesce(message_lower, ""), covid_pattern),
      1,
      0
    )
  )

facebook_prepared %>%
  glimpse()

facebook_prepared %>%
  count(post_type_simple, sort = TRUE)

# optional export for script 03
# write_csv(facebook_prepared, "data/facebook_prepared.csv")
