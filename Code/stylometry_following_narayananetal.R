library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

file <- "../Data/RC_2017-02"
# n_authors <- 5000
#scores are considered reliable if the comment was collected after allowed_gap
allowed_gap <- 60*60*24*3
main_subreddit <- "nfl"
options(tibble.print_max = Inf) 
#------------------------EDA--------------------------------
metadata <- read_csv(paste0(file, "_metadata.csv"),
                     col_types = "cccciiciiiiic") %>%
    replace_na(list(edited = TRUE)) %>%
    mutate(
        collection_gap = retrieved_on - created_utc,
        score = ifelse(collection_gap > allowed_gap, score, NA),
        edited = ifelse(edited == "False", NA, as.numeric(edited)),
        edited_gap = edited - created_utc,
        created_utc = as_datetime(created_utc)
    ) 


features <- read_csv(paste0(file, "_features.csv")) %>%
    filter(id %in% metadata$id) 

id_cols <- c("id", "subreddit_id", "author", "plot", "subreddit")
feature_cols <- colnames(features) %>% setdiff(id_cols)

