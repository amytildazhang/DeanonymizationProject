#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(purrr)
library(lubridate)
library(RANN) #for k nearest neighbors

#-------------------------set parameters-------------------------------
args = commandArgs(trailingOnly=TRUE)

if (length(args) > 0) {
    # limit data to specific subreddits?
    message("Received command line arguments")
    all <- args[1] == 1 
    subset_f <- args[2]
    nn <- as.integer(args[3])
    gt <- as.integer(args[4])
} else {
    all <- FALSE
    subset_f <- "RC_50a_subset.csv"
    nn <- 250
    gt <- 6
}


# nn <- list(
#     "RC_50a_subset.csv" = 5,
#     "RC_600w_subset.csv" = 250,
#     "RC_8000w_subset.csv" = 50,
#     "RC_1500w_subset.csv" = 100,
#     "RC_5000a_subset.csv" = 100,
#     "RC_5000w_subset.csv" = 100
#     )

n_cluster <- 18

file <- "../Data/RC_2017-02"
# path to folder with subsets?
subsetfolder <- "../Output/Subsetting/"
outputfolder <- "../Output/"

#------------------------specify which comments/authors to use---------------------------

options(tibble.print_max = Inf) 
tweets <- fread(paste0(subsetfolder, subset_f), select = c("id", "author")) 
subset_author <- unique(tweets$author)
tweets <- unique(tweets$id)


#------------------------read in and subset data--------------------------------

# we'll read our very large feature files in chunk by chunk
# and read records from each chunk into memory only if they satisfy
# a certain condition


extract_records <- function(chunk, pos) {
    return(chunk %>% filter(author %in% subset_author))
}

# metadata <- read_csv_chunked(paste0(file, "_metadata.csv"),
#                              DataFrameCallback$new(extract_records), 
#                              col_types = "cccciiii")

# features <- read_csv_chunked(paste0(file, "_features.csv"), 
#                      DataFrameCallback$new(extract_records))

features <- do.call("rbind", lapply(1:3, function(idx) {
    read_csv_chunked(paste0(file, "_combinedFeaturesSubset", idx, ".csv"), 
                     DataFrameCallback$new(extract_records)) #%>%
    # filter(id %in% metadata$id) %>%
    # inner_join(metadata, by = c("id", "author", "subreddit"))
}))


feature_cols <- read_csv(paste0(outputfolder, "top_features.csv"))$feature_name
id_cols <- c("id", "author", "subreddit")
features <- features[,c(id_cols, feature_cols)]

features[is.na(features)] <- 0

message("Data read in, now normalizing and removing features with all 0s")


#----------------------Normalization--------------------------------

# remove columns that are all 0
all_zeros <- apply(features[,feature_cols], 2, function(colm){
    if (typeof(colm) != "double") colm <- as.numeric(colm)
    if (sum(colm != 0) > 0)  return(FALSE)
    return(TRUE)
})
features <- features[,-c(which(colnames(features) %in% feature_cols[all_zeros]))]
feature_cols <- feature_cols[!all_zeros]

# feature-mean-nonzero 
features[,feature_cols] <- apply(features[,feature_cols], 2, function(colm){
    if (typeof(colm) != "double") colm <- as.numeric(colm)
    return(colm/mean(colm[colm != 0]))
})

# row-norm
features[,feature_cols] <- apply(features[,feature_cols], 1, function(row_f){
    return(row_f/sum(row_f^2))

})


# Remaining steps
# 1. Create hold-out set
# 2. 5-fold cross-validation over training set to select tuning parameters
#   - kNN
#   - Logistic regression with lasso
# 3. Calculate performance measures
# 4. Test performance over hold-out set

message("Normalization done, now clusterizing and starting nearest neighbors")
print(paste("# clusters:", n_cluster))
savefolder <- gsub("Subsetting", "kNN", subsetfolder)

#-------------------- Set up for cross-validation--------------------------------
# 1. Create hold-out set
# ----------------------
set.seed(1117)
ho_idx <- as.logical(rbinom(nrow(features), 1, 0.2))
ho_set <- features[ho_idx, ]
features <- features[!ho_idx, ]
n_posts <- nrow(features)
message("Successfully created holdout set")

# check set difference of authors
message("Users who are in the test set but not in the training set: ")
savefile <- gsub(".csv$", "_diff_test-only.csv", subset_f)
diff <- setdiff(ho_set$author, features$author)
tibble(author = diff) %>% write_csv(paste0(savefolder, savefile))
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))

message("Users who are in the training set but not in the test set: ")
savefile <- gsub(".csv$", "_diff_train-only.csv", subset_f)
diff <- setdiff(features$author, ho_set$author)
tibble(author = diff) %>% write_csv(paste0(savefolder, savefile))
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))


authors <- unique(features$author)
n_authors <- length(authors)

#   2a. kNN
#   ---------------------------------------------

# we're going to vary over size of neighbors and gap threshold

# initialize results matrix; will have ranks of correct authors
# results <- array(NA, dim = c(n_posts, length(nbhoods), length(gap_thres), n_authors)) 

ho_set[is.na(ho_set)] <- 0
features[is.na(features)] <- 0


# run nearest neighbors
neighbors <- nn2(features[,feature_cols],
                 query = ho_set[,feature_cols],
                 k = nn, treetype = "kd", 
                 searchtype = "standard", eps = 1e-5)
neighbors <- neighbors$nn.idx
        
       
# save the rank of true author in our results vector
results <- sapply(1:nrow(ho_set), function(idx){
    author_ranks <- features$author[neighbors[idx,]] %>% table %>% sort(decreasing = T)
    true_author <- ho_set$author[idx]
    nm_authors <- names(author_ranks)
    names(author_ranks) <- NULL
    
    # get gap statistic
    gap <- author_ranks[1] - author_ranks[2]
    
    # output only if author is listed and gap is higher than threshold
    if (true_author %in% nm_authors){
        if (gap > gt |  length(author_ranks) == 1) {
            return(which(nm_authors == true_author))
        } else {
            return(NA)
        }
    } else {
        return(Inf)
    }
})
# save results over validation set
savefile <- gsub(".csv$", "_test-results.csv", subset_f)

tibble(
    author = ho_set$author,
    rank = results 
    ) %>%
    write_csv(paste0(savefolder, savefile))



# # select method
# if (n_authors > 50) {
#  # final <- apply(results, c(2,3), function(col) sum(col <= 20 & col > 0, na.rm = T))

# # final <- rbind(n_top20, n_top10, n_correct)    
# } else {
#     final <- apply(results, c(2,3), function(col) sum(col <= 5 & col > 0, na.rm = T))
# }
# n_top20 <- apply(results, c(2,3), function(col) sum(col <= 20 & col > 0, na.rm = T))
# n_top10 <- apply(results, c(2,3), function(col) sum(col <= 10 & col > 0, na.rm = T))
# n_correct <- apply(results, c(2,3), function(col) sum(col == 1, na.rm = T))

# final <- rbind(n_top20, n_top10, n_correct)



# savefile <- gsub(".csv$", "_results.csv", subset)

# colnames(final) <- gap_thres
# rownames(final) <- NULL
# results_validation <- as.data.frame(final) %>% 
#     cbind(data.frame(
#         neighborhood = rep(nbhoods, 3),
#         method = rep(c("top20", "top10", "top1"), each = length(nbhoods))
#     )) %>%
#     gather(key = "gap_threshold", value = "number", 1:length(gap_thres)) %>%
#     spread(key = method, value = number) %>%
#     write_csv(paste0(savefolder, savefile))


# get precision/recall of various 
# results_validation <- results_validation %>%
#     mutate(
#         precision_20 = map2_dbl(neighborhood, gap_threshold, function(nn, gt){
#             dat <- 
#         })
#     )
# 
# })
# 3. Test performance over hold-out set


