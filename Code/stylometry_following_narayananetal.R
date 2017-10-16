library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(binr)
library(here)

file <- "Data/RC_2010-01"
n_authors <- 5000
#subreddits <- c("NFL", "Patriots", "falcons", "nflstreams")
#scores are considered reliable if the comment was collected after allowed_gap
allowed_gap <- 60*60*24*3
subreddits <- c("AskReddit", "relationship_advice", "atheism", "IAmA", "politics")

#------------------------EDA--------------------------------
metadata <- read_csv(here(paste0(file, "_metadata.csv")),
                     col_types = "cccciiciiiiil") %>%
    filter(subreddit %in% subreddits) %>%
    replace_na(list(edited = TRUE)) %>%
    mutate(
        collection_gap = retrieved_on - created_utc,
        created_utc = as_datetime(created_utc),
        score = ifelse(collection_gap > allowed_gap, score, NA)
    ) 


#include authors that are in the main subreddit (1st) and at least one of the others
authors <- metadata %>%
    group_by(author) %>%
    summarise(
        in_main = subreddits[1] %in% subreddit,
        in_others = any(subreddits[-1] %in% subreddit),
        n_posts = n()
    ) %>%
    filter(in_main, in_others) %>%
    arrange(desc(n_posts))

# 
# by_subreddit <- metadata %>%
#     group_by(subreddit_id) %>%
#     summarise(
#         n_names = length(unique(subreddit)),
#         n_posts = n(),
#         n_posters = length(unique(author)),
#         avg_posts = n_posts/n_posters,
#         names = paste(unique(subreddit), collapse = " ")
#     ) %>%
#     filter(n_posters > 2000) %>%
#     arrange(desc(avg_posts)) 
# 
# by_author <- metadata %>%
#     group_by(author, subreddit) %>%
#     summarise(
#         n_posts = n()
#     ) %>%
#     ungroup() %>% group_by(author) %>%
#     summarise(
#         avg_posts = mean(n_posts),
#         med_posts = median((n_posts)),
#         n_posts = sum(n_posts),
#         n_subreddits = length(unique(subreddit))
#     ) %>%
#     filter(n_posts > 20, n_subreddits > 1) %>%
#     arrange(desc(avg_posts))

authors <- authors[2:(n_authors + 1),]
metadata <- filter(metadata, author %in% authors$author)

features <- read_csv(here(paste0(file, "_features.csv"))) %>%
    filter(id %in% metadata$id) %>%
    left_join(metadata %>% select(id, subreddit), by = "id")

id_cols <- c("id", "subreddit_id", "author", "plot")
feature_cols <- colnames(features) %>% setdiff(id_cols)




#replace all NAs in feature_cols with 0s
toreplace <- setNames(lapply(vector("list", length(feature_cols)), 
    function(x) x <- 0), feature_cols)
features <- features %>% 
    replace_na(toreplace)
authorsumm <- features %>% 
    group_by(author) %>%
    summarise_at(feature_cols, mean, na.rm = TRUE) %>%
    ungroup()



plotprob <- 300000/nrow(metadata) #maximum number of points that can reasonably be plotted
cat("% of points that will be plotted is", plotprob, "\n")
training_set <- features %>% 
    filter(subreddit %in% subreddits[1]) %>%
    group_by(author) %>%
    mutate(
        plot = rbinom(1, n(), plotprob)
    ) %>%
    ungroup()

#select top 20 features by information gain using shannon entropy
#plot EDA for these, rather htan all 300...
shannon_bin <- function(ftname, df) {
    bins <-  bins(df %>% pull(ftname), 20, max.breaks = 20)
    newdf <- df %>% select_(ftname, "id") %>%
        arrange_(ftname) %>%
        mutate(bin = NA) %>%
        select(-1)
    
    j = 0 
    letter = 1
    for (i in bins$binct) {
        newdf$bin[j:(j+i)] <- letters[letter] 
        letter <- letter + 1; j <- j + i
    }

    colnames(newdf)[2] <- ftname
    return(newdf)
}

shannon_entropy <- function(fct) {
    occurrence <- table(fct) + .01
    prob <- occurrence/sum(occurrence)
    sum(prob * log(prob))
}

info_gain <- function(ftname, df) {
    fctorized <- shannon_bin(ftname, df) %>%
        full_join(df %>% select(author, id), by = "id") %>%
        mutate(joint = interaction(2, 3))

    shannon_entropy(fctorized %>% pull(ftname)) + 
    shannon_entropy(fctorized %>% pull(author)) -
    shannon_entropy(interaction(fctorized %>% pull(ftname), fctorized %>% pull(author)))
}

feature_infogain <- tibble(
    feature = feature_cols,
    info_gain = map_dbl(feature_cols, function(ftname) info_gain(ftname, training_set))
    ) %>%
    arrange(desc(info_gain))


#loop through and plot EDA (for top 20 and bottom 20?)
#feature dispersion across all authors
#feature dispersion within authors across subreddits

i <- 1
ft <- feature_cols[i]


#compare feature distribution across authors
#provides support that features help distinguish authors
authorsumm <- authorsumm %>% arrange_(ft) %>% mutate(order = 1:n())
features <- features %>% 
    left_join(authorsumm[,c("author", "order")], by = "author")


ggplot() +
    geom_point(data = features, aes_string(x = "order", y = ft),
               alpha = 0.1, size = 1) +
    geom_line(data = authorsumm, aes_string(x = "order", y = ft), 
              color = "#C4A20A", alpha = 0.7, size = 1) +
    theme_minimal() +
    labs(title = paste("Distribution of", ft, "across Reddit users"),
         xlab = "Reddit user id #",
         ylab = ft) +
    ggsave(here(paste0("Output/EDA_authors_", ft, ".png")))


#compare distribution of features across authors and subreddits
#provides support that features are topic-independent
features <- features %>% left_join(metadata[,c("id", "subreddit")], by = "id") 
subredditsumm <- features %>%
    group_by(author, subreddit, order) %>%
    summarise_at(feature_cols, mean, na.rm = TRUE) %>%
    ungroup()

ggplot() +
    geom_point(data = features, aes_string(x = "order", y = ft),
               alpha = 0.1, size = 1) +
    geom_line(data = subredditsumm, aes_string(x = "order", y = ft),
              color = "#0980B2",  size = 0.8) +
    geom_line(data = authorsumm, aes_string(x = "order", y = ft),
              color = "#C4A20A", size = 0.8) +
    theme_minimal() +
    labs(title = paste("Distribution of", ft, "across subreddits and users"),
         xlab = "Reddit user id #",
         ylab = ft) +
    facet_grid(subreddit ~ .) +
    ggsave(here(paste0("Output/EDA_subreddits_", ft, ".png")),
        height = 10, width = 7, units = "in")

