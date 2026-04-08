# this script was prepared as part of a postdoctoral application to the university of amsterdam
# it produces descriptive summaries, figures, and two simple association models
# much of the workflow builds on coding routines developed during the doctoral dissertation

library(tidyverse)
library(broom)
library(MASS)
library(stringr)

scandi_cols <- c(
  blue  = "#6C8EAD",
  sage  = "#9CAF88",
  sand  = "#D8C7A1",
  rose  = "#C48A7A",
  slate = "#8C9AA5"
)

theme_scandi <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 11),
      legend.position = "top"
    )
}

strategy_vars <- c(
  "defensive",
  "excellence",
  "framing_crisis",
  "self_enhancement",
  "framing_organization",
  "routine_communication",
  "accommodative",
  "interorganizational_relationships",
  "action"
)

strategy_labels <- c(
  defensive = "Defensive",
  excellence = "Excellence",
  framing_crisis = "Framing the crisis",
  self_enhancement = "Self-enhancement",
  framing_organization = "Framing the organization",
  routine_communication = "Routine communication",
  accommodative = "Accommodative",
  interorganizational_relationships = "Interorganizational relationships",
  action = "Action"
)

input_file <- "data/facebook_prepared.csv"

facebook_prepared <-
  read_csv(input_file, show_col_types = FALSE)

facebook_lockdown <-
  facebook_prepared %>%
  filter(
    post_created_date >= as.Date("2020-03-12"),
    post_created_date <= as.Date("2020-05-12")
  ) %>%
  mutate(
    across(
      all_of(strategy_vars),
      ~ replace_na(as.numeric(.x), 0)
    )
  )

overall_summary <-
  facebook_lockdown %>%
  summarise(
    n_posts = n(),
    n_actors = n_distinct(actor_name),
    mean_interactions = mean(total_interactions, na.rm = TRUE),
    median_interactions = median(total_interactions, na.rm = TRUE),
    sd_interactions = sd(total_interactions, na.rm = TRUE),
    mean_followers = mean(followers_at_posting, na.rm = TRUE),
    median_followers = median(followers_at_posting, na.rm = TRUE)
  )

posts_by_actor <-
  facebook_lockdown %>%
  count(actor_name, sort = TRUE)

posts_by_type <-
  facebook_lockdown %>%
  count(post_type_simple, sort = TRUE)

strategy_summary <-
  facebook_lockdown %>%
  pivot_longer(
    cols = all_of(strategy_vars),
    names_to = "strategy",
    values_to = "code_present"
  ) %>%
  mutate(
    strategy_label = recode(strategy, !!!strategy_labels)
  ) %>%
  group_by(strategy, strategy_label) %>%
  summarise(
    n_posts = sum(code_present == 1, na.rm = TRUE),
    share_of_posts = mean(code_present == 1, na.rm = TRUE) * 100,
    mean_interactions_when_present = mean(total_interactions[code_present == 1], na.rm = TRUE),
    mean_interactions_when_absent = mean(total_interactions[code_present == 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(share_of_posts))

strategy_long <-
  facebook_lockdown %>%
  pivot_longer(
    cols = all_of(strategy_vars),
    names_to = "strategy",
    values_to = "code_present"
  ) %>%
  mutate(
    strategy_label = recode(strategy, !!!strategy_labels),
    code_present = if_else(code_present == 1, "Present", "Not present")
  ) %>%
  group_by(strategy, strategy_label, code_present) %>%
  summarise(
    mean_interactions = mean(total_interactions, na.rm = TRUE),
    .groups = "drop"
  )

covid_summary <-
  facebook_lockdown %>%
  group_by(covid_keyword) %>%
  summarise(
    n_posts = n(),
    mean_interactions = mean(total_interactions, na.rm = TRUE),
    median_interactions = median(total_interactions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    covid_label = case_when(
      covid_keyword == 1 ~ "COVID-related",
      covid_keyword == 0 ~ "No COVID keyword"
    )
  )

plot_strategy_share <-
  strategy_summary %>%
  ggplot(aes(x = reorder(strategy_label, share_of_posts), y = share_of_posts)) +
  geom_col(fill = scandi_cols["blue"], width = 0.7) +
  coord_flip() +
  labs(
    title = "Share of posts using each coded strategy",
    x = "",
    y = "Percent of posts"
  ) +
  theme_scandi()

plot_strategy_interactions <-
  strategy_long %>%
  ggplot(aes(x = reorder(strategy_label, mean_interactions), y = mean_interactions, fill = code_present)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Not present" = scandi_cols["sand"],
      "Present" = scandi_cols["sage"]
    )
  ) +
  labs(
    title = "Average interactions by coded strategy",
    x = "",
    y = "Average interactions",
    fill = ""
  ) +
  theme_scandi()

plot_posts_by_actor <-
  posts_by_actor %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(actor_name, n), y = n)) +
  geom_col(fill = scandi_cols["slate"], width = 0.7) +
  coord_flip() +
  labs(
    title = "Number of posts by actor",
    subtitle = "Top 10 actors during the first lockdown period",
    x = "",
    y = "Number of posts"
  ) +
  theme_scandi()

plot_post_type <-
  facebook_lockdown %>%
  group_by(post_type_simple) %>%
  summarise(
    mean_interactions = mean(total_interactions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(post_type_simple, mean_interactions), y = mean_interactions)) +
  geom_col(fill = scandi_cols["rose"], width = 0.7) +
  coord_flip() +
  labs(
    title = "Average interactions by post type",
    x = "",
    y = "Average interactions"
  ) +
  theme_scandi()

plot_covid_keyword <-
  covid_summary %>%
  ggplot(aes(x = covid_label, y = mean_interactions, fill = covid_label)) +
  geom_col(width = 0.7) +
  scale_fill_manual(
    values = c(
      "COVID-related" = scandi_cols["blue"],
      "No COVID keyword" = scandi_cols["sage"]
    )
  ) +
  labs(
    title = "Average interactions by COVID keyword",
    x = "",
    y = "Average interactions",
    fill = ""
  ) +
  theme_scandi()

model_1 <-
  lm(
    log_total_interactions ~ post_type_simple + weekday + log_followers_at_posting,
    data = facebook_lockdown
  )

model_2 <-
  lm(
    log_total_interactions ~ post_type_simple + weekday + log_followers_at_posting +
      covid_keyword + defensive + excellence + framing_crisis + self_enhancement +
      framing_organization + routine_communication + accommodative +
      interorganizational_relationships + action,
    data = facebook_lockdown
  )

model_nb <-
  glm.nb(
    total_interactions ~ post_type_simple + weekday + log_followers_at_posting +
      covid_keyword + defensive + excellence + framing_crisis + self_enhancement +
      framing_organization + routine_communication + accommodative +
      interorganizational_relationships + action,
    data = facebook_lockdown
  )

model_2_tidy <-
  tidy(model_2, conf.int = TRUE) %>%
  filter(term != "(Intercept)")

model_nb_tidy <-
  tidy(model_nb, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)")

model_term_labels <- c(
  log_followers_at_posting = "Followers at posting (log)",
  covid_keyword = "COVID-related post",
  defensive = "Defensive",
  excellence = "Excellence",
  framing_crisis = "Framing the crisis",
  self_enhancement = "Self-enhancement",
  framing_organization = "Framing the organization",
  routine_communication = "Routine communication",
  accommodative = "Accommodative",
  interorganizational_relationships = "Interorganizational relationships",
  action = "Action"
)

model_2_tidy <-
  model_2_tidy %>%
  mutate(term_label = recode(term, !!!model_term_labels))

model_nb_tidy <-
  model_nb_tidy %>%
  mutate(term_label = recode(term, !!!model_term_labels))

plot_model_2 <-
  model_2_tidy %>%
  filter(
    !str_detect(term, "weekday"),
    !str_detect(term, "post_type_simple")
  ) %>%
  ggplot(aes(x = estimate, y = reorder(term_label, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(color = scandi_cols["rose"], size = 2.5) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    color = scandi_cols["rose"]
  ) +
  labs(
    title = "What is linked to higher Facebook interactions",
    subtitle = "Linear model with coded strategy variables",
    x = "Coefficient estimate",
    y = ""
  ) +
  theme_scandi()

plot_model_nb <-
  model_nb_tidy %>%
  filter(
    !str_detect(term, "weekday"),
    !str_detect(term, "post_type_simple")
  ) %>%
  ggplot(aes(x = estimate, y = reorder(term_label, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  geom_point(color = scandi_cols["blue"], size = 2.5) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    color = scandi_cols["blue"]
  ) +
  labs(
    title = "What is linked to higher Facebook interactions",
    subtitle = "Negative binomial model with coded strategy variables",
    x = "Incidence rate ratio",
    y = ""
  ) +
  theme_scandi()

# optional exports
# write_csv(overall_summary, "data/overall_summary.csv")
# write_csv(posts_by_actor, "data/posts_by_actor.csv")
# write_csv(posts_by_type, "data/posts_by_type.csv")
# write_csv(strategy_summary, "data/strategy_summary.csv")
# write_csv(model_2_tidy, "data/model_linear_tidy.csv")
# write_csv(model_nb_tidy, "data/model_negative_binomial_tidy.csv")
# ggsave("figures/01_share_of_posts_by_strategy.png", plot_strategy_share, width = 8, height = 5.5, dpi = 300)
# ggsave("figures/02_average_interactions_by_strategy.png", plot_strategy_interactions, width = 8, height = 6.5, dpi = 300)
# ggsave("figures/03_number_of_posts_by_actor.png", plot_posts_by_actor, width = 8, height = 5.5, dpi = 300)
# ggsave("figures/04_average_interactions_by_post_type.png", plot_post_type, width = 8, height = 5, dpi = 300)
# ggsave("figures/05_average_interactions_by_covid_keyword.png", plot_covid_keyword, width = 7, height = 5, dpi = 300)
# ggsave("figures/06_linear_model_coefficients.png", plot_model_2, width = 8, height = 6, dpi = 300)
# ggsave("figures/07_negative_binomial_model_coefficients.png", plot_model_nb, width = 8, height = 6, dpi = 300)
