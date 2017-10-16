library(tidyverse)
library(lubridate)
library(here)

#------------------------EDA--------------------------------
metadata <- read_csv(here("Data/RC_2010-01_metadata.csv"),
                     col_types = "cccciiciiiiil") %>%
    replace_na(list(edited = TRUE)) %>%
    mutate(
        collection_gap = retrieved_on - created_utc,
        created_utc = as_datetime(created_utc)
    )

#scores are considered reliable if the comment was collected after allowed_gap
allowed_gap <- 60*60*24*3

by_subreddit <- metadata %>%
    group_by(subreddit_id) %>%
    summarise(
        n_names = length(unique(subreddit)),
        n_posts = n(),
        n_posters = length(unique(author)),
        avg_posts = n_posts/n_posters,
        names = paste(unique(subreddit), collapse = " ")
    ) %>%
    filter(n_posters > 2000) %>%
    arrange(desc(avg_posts))

by_author <- metadata %>%
    group_by(author, subreddit) %>%
    summarise(
        n_posts = n()
    ) %>%
    ungroup() %>% group_by(author) %>%
    summarise(
        avg_posts = mean(n_posts),
        med_posts = median((n_posts)),
        n_posts = sum(n_posts),
        n_subreddits = length(unique(subreddit))
    ) %>%
    filter(n_posts > 20, n_subreddits > 1) %>%
    arrange(desc(avg_posts))
