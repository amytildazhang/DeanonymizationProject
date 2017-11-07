library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(RANN) #for k nearest neighbors
file <- "../Data/RC_2017-02"
# n_authors <- 5000
#scores are considered reliable if the comment was collected after allowed_gap
allowed_gap <- 60*60*24*3
main_subreddit <- "nfl"
options(tibble.print_max = Inf) 
#------------------------read in data--------------------------------
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
    filter(id %in% metadata$id) %>%
    inner_join(by = c("id", "author", "subreddit")) %>%

id_cols <- c("id", "subreddit_id", "author", "plot", "subreddit")
feature_cols <- colnames(features) %>% setdiff(id_cols)

#--------------------toy example k-NN--------------------------------
features[is.na(features)] <- 0
neighbors <- nn2(features[1:1000,] %>% select(feature_cols),
    k = 10, treetype = "kd", searchtype = "standard", eps = 1e-5)

#Steps:
# 1. split into train/validation/test
# 2. use score over validation set to choose k
# 3. apply to test set and get metrics
# 4. So we need some kind of shared code for metrics?

#--------------------toy example --------------------------------
