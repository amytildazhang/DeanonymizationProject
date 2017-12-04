#!/usr/bin/env Rscript

library(readr)
library(glmnet)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(snow)
library(RANN) #for k nearest neighbors

#-------------------------set parameters-------------------------------
args = commandArgs(trailingOnly=TRUE)

if (length(args) > 0) {
    # limit data to specific subreddits?
    message("Received command line arguments")
    subreddits <- args[1] == 1 
    subset_f <- args[2]
} else {
    subreddits <- FALSE
    subset_f <- "RC_600w_subset.csv"
}

n_cluster <- 18

file <- "../Data/RC_2017-02"
# path to folder with subsets?
subsetfolder <- "../Output/Subsetting/"
outputfolder <- "../Output/"

#------------------------specify which comments/authors to use---------------------------

options(tibble.print_max = Inf) 
subset <- read_csv(paste0(subsetfolder, subset_f)) 
subset_authors <- subset$author %>% unique
if (subreddits) subset_comments <- subset$id

#------------------------read in and subset data--------------------------------

# we'll read our very large feature files in chunk by chunk
# and read records from each chunk into memory only if they satisfy
# a certain condition

extract_records <- function(chunk, pos) {
    if (subreddits) return(chunk %>% filter(id %in% subset_comments))
    return(chunk %>% filter(author %in% subset_authors))
}

# metadata <- read_csv_chunked(paste0(file, "_metadata.csv"),
#                              DataFrameCallback$new(extract_records), 
#                              col_types = "cccciiii")

# features <- read_csv_chunked(paste0(file, "_features.csv"), 
#                      DataFrameCallback$new(extract_records))

features <- do.call("rbind", lapply(1:3, function(set) {
    read_csv_chunked(paste0(file, "_combinedFeaturesSubset", set, ".csv"), 
                     DataFrameCallback$new(extract_records)) #%>%
    # filter(id %in% metadata$id) %>%
    # inner_join(metadata, by = c("id", "author", "subreddit"))
}))

id_cols <- c("id", "subreddit_id", "author", "plot", "subreddit")
feature_cols <- colnames(features) %>% setdiff(id_cols)
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

#-------------------- Set up for cross-validation--------------------------------

cl <- makeCluster(n_cluster)

# 1. Create hold-out set
# ----------------------
set.seed(1117)
ho_idx <- as.logical(rbinom(nrow(features), 1, 0.2))
# ho_set <- features[ho_idx, ]
features <- features[!ho_idx, ]
n_posts <- nrow(features)
message("Successfully created holdout set")

# check set difference of authors
message("Users who are in the test set but not in the training set: ")
diff <- setdiff(ho_set$author, features$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))

message("Users who are in the training set but not in the test set: ")
diff <- setdiff(features$author, ho_set$author)
message(ifelse(length(diff) > 0, paste(diff, collapse = " "), "None"))


# 2. 5-fold cross-validation over training set to select tuning parameters
# ---------------------------------------------
folds <- sample(1:5, size = n_posts, replace = TRUE) #create fold

authors <- unique(features$author)
n_authors <- length(authors)


#   2b. Logistic regression with lasso
#   ---------------------------------------------

# gap threshold values?

n_lambdas <- 100
gap_thres <- c(1, 2, 3, 5, 8, 10)/100
# we're going to vary over the gap threshold and 100 possible tuning parameters
# we'll allow glmnet to select the 100 possible tuning parameters
# this is done in a one-vs-all manner so we also iterate over authors

max_weight <- sapply(authors, function(author) sum(features$author == author))  %>% max


# find lambdas, using glmnet's strategy
labels <- features$author %>% as.matrix
features <- as.matrix(features[,feature_cols])

lambdas <- sapply(authors, # for each author get max lambda value
                  function(author) {
                    aut <- as.integer(labels == author)
                      colSums(features * aut) %>% # dot product b/w author and all predictors
                          abs %>% max %>% # max dot product
                          `*`( max_weight / n_posts * 5 / 4)
                  }) # max lambda value
                

lambdas <- seq(from = 0.005 * max(lambdas), to = max(lambdas), length.out = n_lambdas)
# ranks <- array(NA, dim = c(n_posts, n_lambdas, length(gap_thres), n_authors))
# results <- array(0, dim = c(n_posts, n_lambdas, length(gap_thres)))

clusterCall(cl, function(){
    library(glmnet)
    library(magrittr)
})
clusterExport(cl, list("responses", "weights", "lambdas", "features",
                       "gap_thres"))

#sklearn.linear_model import LogisticRegression
use_condaenv("py36", required=TRUE)
rlsc <- import("sklearn.linear_model")
rlsc <- rlsc$LogisticRegression(multi_class = 'ovr', 
                                solver = 'saga', 
                                penalty = 'l1')

# save results over validation set
savefolder <- gsub("Subsetting", "BGLM_Lasso/Tuning", subsetfolder)
csv_lambdanames <- paste0("l-", lambdas)
csv_lambdanames <- rep(0, n_lambdas)
for (fold in 1:5) {
    print(paste("Working on fold", fold))
    test_idx <- folds == fold

     # predictions <- rep(0, dim = c(sum(test_idx), n_authors, n_lambdas))
    parLapply(1:n_authors, function(aut_idx){
        response <- as.integer(labels == authors[aut_idx])[!test_idx]
        if (sum(response) > 1) {
            weight <- sum(response) * response 
            weight[weight == 0] <- 1
            # weight <- rep(1, sum(!test_idx))
            fit <- rlsc$fit(X = features[!test_idx], 
                            y = as.matrix(labels[!test_idx]), sample_weight = weight)
            fit <- glmnet(x = features[!test_idx,], 
                          y = response, lambda = lambdas,
                          family = "binomial", weights = weight)
            pred <- predict(fit, features[test_idx,], type = "response")
            colnames(pred) <- paste0("l-", fit$lambda)
            clusterExport(cl, list("pred", "test_idx", "subset_f", "savefolder"))
            invisible(parLapply(cl, 1:sum(test_idx), function(p_idx){
                savefile <- gsub(".csv$", "_author_probs", which(test_idx)[p_idx], ".csv", subset_f)
                if (length(setdiff(csv_lambdanames, colnames(pred))) > 0) {
                    
                } else {
                    cbind(tibble(
                        author = authors[aut_idx],
                        true_author = labels[which(test_idx)[p_idx]]
                    ), 
                    as_tibble(t(pred[p_idx,]))) %>%
                        write_csv(paste0(savefolder, savefile), append = TRUE)
                }
                return(NA)
            }))
        }
    }))


    
    # clusterExport(cl, list("predictions", "authors", "n_lambdas", "results"))
    # # get rank of true author
    # author_ranks <- parLapply(cl, 1:sum(test_idx), function(p_idx){
    #     true_author <- labels[test_idx][p_idx]

    #     by_post <- do.call("cbind", lapply(predictions, function(author){
    #             if (is.na(author)) return(0)
    #             author[p_idx,] %>% as.vector #1 x n_lambda
    #         }))  #n_lambda x n_author

    #      sapply(1:n_lambdas, function(lambda){
    #         if (lambda > nrow(by_post)) return(rep(NA, ncol(by_post)))
    #         lambda <- by_post[lambda,] #1 x n_author
    #         l_order <- order(lambda, decreasing = TRUE)
    #         if (lambda[which(l_order == 1)] - lambda[which(l_order == 2)] < gap_thres[t]) {
    #             return(NA)
    #         }
    #         return(l_order[which(authors == true_author)])
    #         }) #n_author x n_lambda
    #     })

    # for(p_idx in 1:sum(test_idx)) {
    #     results[which(test_idx)[p_idx],,t] <- author_ranks[[p_idx]]
    #     }
}







# cbind(labels, 
#     do.call("cbind", lapply(1:length(gap_thres), function(gt){
#         iter_gt <- results[,,gt]
#         colnames(iter_gt) <- paste0("lambda-", lambdas, "_gt-", gt)
#         iter_gt
#     })) %>% as_tibble)  %>%
#     write_csv(paste0(savefolder, savefile))


# map2_df(rep(1:n_lambdas, length(gap_thres)), rep(1:length(gap_thres), each = n_lambdas), function(lamb, gt){
#     iter_r <- ranks[,lamb,gt,]
#     colnames(iter_r) <- authors
#     as_tibble(iter_r) %>%
#         mutate(
#             lambda = lambdas[lamb],
#             gap = gap_thres[gt],
#             true_author = labels
#             )
# }) %>%
#     write_csv(paste0(savefolder, savefile))


# n_top20 <- apply(ranks, c(2,3), function(col) sum(col <= 20 & col > 0, na.rm = T))
# n_top10 <- apply(ranks, c(2,3), function(col) sum(col <= 10 & col > 0, na.rm = T))
# n_correct <- apply(ranks, c(2,3), function(col) sum(col == 1, na.rm = T))

# final <- rbind(n_top20, n_top10, n_correct)

stopCluster(cl)
# 3. Test performance over hold-out set

# 
# 
# ggplot(features, aes(x = info_gain)) + 
#     geom_histogram(binwidth = 0.05) +
#     xlab("Information gain") +
#     ylab("# features") +
#     theme_minimal() +
#     geom_vline(aes(xintercept = 0.1467852), color = "blue")
# 
