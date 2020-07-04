# strict inclusion 1 means I don't include the inclusion criteria:
# 'the issue has at least one action by the owner'


## ----library ---------------------------------------------------------
# library(tidyverse)
library(ggplot2)
library(tidyr) # 1.0.0
library(readr)
library(purrr)
library(dplyr)
library(stringr)
# additional tidyverse-adjacent packages
library(lubridate)
library(glue)

## ---- githubIssueCollection -------------------------------------------
library(githubIssueCollection)

## ---- import ----------------------------------------------------------
## In your code, run
issues <- readRDS("../data/working-issues/issues.rds")
#issues <- githubIssueCollection::example_issues


## ---- events__issue-opened ------------------------------------------------
events_opened <-
  issues$issue %>%
  mutate(event = "opened") %>%
  select(
    owner, repo,
    issue_id = id,
    event_at = created_at,
    event,
    user_association = author_association,
    user_id
  )

## ---- events__issue-commented ---------------------------------------------
events_commented <-
  issues$comment %>%
  mutate(event = "comment") %>%
  select(
    owner, repo, issue_id,
    event_at = created_at,
    event,
    user_association = author_association,
    user_id
  )

## ---- events__contributor-association -------------------------------------
contributor_association <-
  issues$contributors %>%
  mutate(user_association = if_else(owner == login, "OWNER", "CONTRIBUTOR")) %>%
  select(owner, repo, user_id = id, user_association)


## ---- events__author-association ------------------------------------------
author_association <-
  issues[c("issue", "comment")] %>%
  map_dfr(
    . %>% select(owner, repo, user_id, user_association = author_association)
  ) %>%
  distinct()

## ---- events__user-association --------------------------------------------
user_association <- dplyr::union(
  contributor_association,
  author_association
)


## ---- events --------------------------------------------------------------
events <-
  issues$event %>%
  select(
    owner, repo, issue_id,
    event_at = created_at,
    event,
    user_id = actor_id
  ) %>%
  left_join(user_association, by = c("owner", "repo", "user_id")) %>%
  replace_na(list(user_association = "NONE"))



## ---- tidy-events ---------------------------------------------------------
events_tidy <-
  bind_rows(
    events,
    events_commented,
    events_opened
  ) %>%
  arrange(issue_id, event_at)



## ---- classify-issues -----------------------------------------------------
owner_issues <-
  issues$issue %>%
  filter(author_association %in% c("OWNER", "COLLABORATOR")) %>%
  distinct(issue_id = id)

pull_request_issues <-
  issues$issue %>%
  filter(is_pull_request) %>%
  distinct(issue_id = id)

has_one_owner_action <-
  events_tidy %>%
  filter(user_association %in% c("OWNER", "MEMBER", "COLLABORATOR"), event != "closed") %>%
  distinct(issue_id)



## ---- select-issue-events -------------------------------------------------
events_tidy_selected <-
  events_tidy %>%
  anti_join(owner_issues, by = "issue_id") %>%
  anti_join(pull_request_issues, by = "issue_id")# %>%
  #semi_join(has_one_owner_action, by = "issue_id")


## ---- repos-today ----------------------------------------------
# Ensure that issues are opened until "today"
repos_today <-
  issues$`_repos` %>%
  select(owner, repo, event_at = checked_at) %>%
  mutate(value = 0, event_at = ymd_hms(event_at))


## ---- issues-with-points ------------------------------------------------
# Assign +1 to opened issues -1 to closed issues
issues_with_points <-
  events_tidy_selected %>%
  filter(event %in% c("opened", "reopened", "closed")) %>%
  distinct(owner, repo, issue_id, event, event_at) %>%
  mutate(
    event_at = ymd_hms(event_at),
    value = case_when(
      event == "closed" ~ -1,
      str_detect(event, "open") ~ +1
    )
  ) %>%
  bind_rows(repos_today)


## ---- issues-open-timeline ----------------------------------------------
issues_open_timeline <-
  issues_with_points %>%
  arrange(event_at) %>%
  group_by(owner, repo) %>%
  mutate(
    value = cumsum(value),
    event_at_index = as.numeric(difftime(event_at, min(event_at), units = "days"))
  ) %>%
  ungroup()


## ---- issues-open-daily -------------------------------------------------
issues_open_daily <-
  issues_with_points %>%
  mutate(
    event_at = ymd_hms(event_at),
    event_at = case_when(
      event == "opened" ~ floor_date(event_at, "day"),
      event == "closed" ~ ceiling_date(event_at, "day"),
      TRUE ~ event_at
    )
  ) %>%
  arrange(event_at) %>%
  group_by(owner, repo) %>%
  complete(
    event_at = seq(min(event_at), max(event_at), by = "day"),
    fill = list(value = 0)
  ) %>%
  group_by(owner, repo, event_at) %>%
  summarize(value = sum(value)) %>%
  mutate(
    value = cumsum(value),
    event_at_index = time_length(interval(min(event_at), event_at), "day"),
    event_at_index = as.numeric(event_at_index),
    event_since_today = -max(event_at_index) + event_at_index
  ) %>%
  ungroup()

## ---- plot-issue-timeline, fig.width=16, fig.height=9--------------------
issues_open_timeline %>%
  mutate(repo = glue::glue("{owner}/{repo}")) %>%
  ggplot() +
  aes(event_at, value, color = repo, group = repo) +
  geom_step(show.legend = FALSE) +
  facet_wrap(vars(repo), scales = "free") +
  ylim(0, NA) +
  theme_minimal() +
  labs(
    x = "Days Since First Issue",
    y = NULL,
    title = "Number of Open Issues Created by End-Users"
  )



write.csv(issues_open_daily, "../data/for-analysis-ts/daily_issues.csv")
write.csv(issues_open_timeline, "../data/for-analysis-ts/daily_plot.csv")
