library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(binr)
library(purrr)
library(magrittr)

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

subreddits <- unique(metadata$subreddit)

#include authors that are in the main subreddit (1st) and at least one of the others
authors <- metadata %>%
    group_by(author) %>%
    summarise(
        in_main = main_subreddit %in% subreddit,
        in_others = any(setdiff(subreddits, main_subreddit) %in% subreddit),
        subreddits = paste(unique(subreddit), collapse = " "),
        n_posts = n()
    ) %>%
    filter(in_main, in_others) %>%
    arrange(desc(n_posts)) %T>%
    write_csv(paste0(file, "_shared_authors.csv"))

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

metadata <- filter(metadata, author %in% authors$author)

features <- read_csv(paste0(file, "_features.csv")) %>%
    filter(id %in% metadata$id) 

id_cols <- c("id", "subreddit_id", "author", "plot", "subreddit")
feature_cols <- colnames(features) %>% setdiff(id_cols)



#replace all NAs in feature_cols with 0s
toreplace <- setNames(lapply(vector("list", length(feature_cols)), 
    function(x) x <- 0), feature_cols)
features <- features %>% 
    replace_na(toreplace) #%>%
    # mutate_at(feature_cols, as.numeric)



plotprob <- min(30000/nrow(metadata), 1) #maximum number of points that can reasonably be plotted
cat("% of points that will be plotted is", plotprob, "\n")
training_set <- features %>% 
    filter(subreddit %in% main_subreddit) %>%
    group_by(author) %>%
    mutate(
        plot = rbinom(1, n(), plotprob)
    ) %>%
    ungroup()

features <- features %>%
mutate(
    plot = rbinom(1, n(), plotprob)
    )

#select top 20 features by information gain using shannon entropy
#plot EDA for these, rather htan all 300...
shannon_bin <- function(ftidx, df) {
    ft <- as.numeric(df %>% pull(ftidx))
    if (length(unique(ft)) == 1) return(df %>% select(id, ftidx))
    bins <-  bins(ft, 20, max.breaks = 20)
    newdf <- df %>% select(ftidx, id) %>%
        arrange(df[[ftidx]]) %>%
        mutate(bin = NA) %>%
        select(-1)
    
    j = 0 
    letter = 1
    for (i in bins$binct) {
        newdf$bin[j:(j+i)] <- letters[letter] 
        letter <- letter + 1; j <- j + i
    }

    colnames(newdf)[2] <- colnames(df)[ftidx]
    return(newdf)
}

shannon_entropy <- function(fct) {
    occurrence <- table(fct) + .01
    prob <- occurrence/sum(occurrence)
    sum(prob * log(prob))
}

#Takes: Dataframe and the column index of the feature of interest within the dataframe
#Calculates information gain across authors, so it needs a group column named 'author'
#Tbl manipulation means an 'id' column so that we can match numbers together is necessary
#returns: scalar value of information gain for that column
info_gain <- function(ftidx, df) {
    fctorized <- shannon_bin(ftidx, df) %>%
        full_join(df %>% select(author, id), by = "id") 

    shannon_entropy(fctorized %>% pull(2)) + 
    shannon_entropy(fctorized %>% pull(author)) -
    shannon_entropy(interaction(fctorized %>% pull(2), fctorized %>% pull(author)))
}


cat("Calculating information gain of feature in column 'x' of the training subreddit: \n")
#calculate information gain for all columns specified as feature_cols, save as CSV file 
feature_infogain <- tibble(
    idx = which(colnames(training_set) %in% feature_cols),
    info_gain = map_dbl(idx, function(ftidx) info_gain(ftidx, training_set)),
    feature = feature_cols[idx],
    n_unique = map_dbl(idx, function(ftidx) features %>% pull(ftidx) %>% unique %>% length)
    )  %>%
    arrange(info_gain) %T>%
    write_csv(paste0(file, "_feature_infogain.csv"))








#------------------------------EDA
#compare feature distribution across authors, and across subreddits
#provides support that features help distinguish authors and also transfer across subreddits

#dataframe that calculates mean value of all feature_cols, grouped by author
authorsumm <- training_set %>% 
    group_by(author) %>%
    summarise_at(feature_cols, mean, na.rm = TRUE) %>%
    ungroup()

for (i in 1:20){ #save plots of the top 20 features according to information gain
    print(i)
    ft <- feature_infogain$feature[i]
  
    #sort authors so they are in ascending order based on their mean valuefor feature of interest
    authorsumm <- authorsumm %>% arrange_(ft) %>% mutate(order = 1:n())
    training_set <- training_set %>%  
        left_join(authorsumm[,c("author", "order")], by = "author")


    #compare distribution of features across authors within the training set
    ggplot() +
        geom_point(data = training_set %>% filter(plot != 0), aes_string(x = "order", y = ft),
                   alpha = 0.1, size = 1) +
        geom_line(data = authorsumm, aes_string(x = "order", y = ft), 
                  color = "#C4A20A", alpha = 0.7, size = 1) +
        theme_minimal() +
        labs(title = paste("Distribution of", ft, "across Reddit users"),
                subtitle = "Training set",
             xlab = "Reddit user id #",
             ylab = ft) +
        ylim(0, 2 * max(authorsumm$ft, na.rm  = TRUE)) +
        ggsave(paste0("../Output/EDA_authors_", ft, ".png"))


    #compare distribution of features across authors and subreddits
    #provides support that features are topic-independent
    features <- features %>% left_join(authorsumm %>% select(author, order), by = "author")
    subredditsumm <- features %>%
        group_by(author, subreddit, order) %>%
        summarise_at(feature_cols, mean, na.rm = TRUE) %>%
        ungroup()

    ggplot() +
        geom_point(data = features %>% filter(plot != 0), 
            aes_string(x = "order", y = ft),
                   alpha = 0.1, size = 1) +
        geom_line(data = subredditsumm, aes_string(x = "order", y = ft),
                  color = "#0980B2",  size = 0.8) +
        geom_line(data = authorsumm, aes_string(x = "order", y = ft),
                  color = "#C4A20A", size = 0.8) +
        theme_minimal() +
        labs(title = paste("Distribution of", ft, "across subreddits and users"),
             xlab = "Reddit user id #",
             ylab = ft) +
        facet_grid(subreddit ~ ., scales = "free_y") +
        ggsave(paste0("../Output/EDA_subreddits_", ft, ".png"),
            height = 10, width = 7, units = "in")

    training_set <- training_set %>% select(-order)
    features <- features %>% select(-order)
}


