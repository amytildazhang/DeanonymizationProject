#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(snow)
library(RANN) #for k nearest neighbors

#-------------------------set parameters-------------------------------
args = commandArgs(trailingOnly=TRUE)

if (length(args) == 3) {
    # limit data to specific subreddits?
    message("Received command line arguments")
    subreddits <- args[1] == 1 
    subset_f <- args[2]
} else {
    subreddits <- FALSE
    subset_f <- "50authors.csv"
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


features <- do.call("rbind", lapply(1:3, function(set) {
    read_csv_chunked(paste0(file, "_combinedFeatures.csv"), 
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
ho_set <- features[ho_idx, ]
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

#   2a. kNN
#   ---------------------------------------------

# we're going to vary over size of neighbors and gap threshold
nbhoods <- c(2, 5, 10, 20, 50, 100, 150, 200, 250, 500)
nbhoods <- nbhoods[nbhoods < length(subset_authors)]

gap_thres <- c(1, 2, 3, 5, 10, 20, 30, 40, 50)
gap_thres <- gap_thres[gap_thres < max(nbhoods) / 3]

# initialize results matrix; will have ranks of correct authors
results <- array(NA, dim = c(n_posts, length(nbhoods), length(gap_thres), n_authors)) 
colnames(results) <- nbhoods
rownames(results) <- features$author

clusterExport(cl, list("features", "gap_thres", "authors"))
clusterCall(cl, function(){
    library(dplyr)
})



# look over neighborhood size and fold
for (t in 1:length(gap_thres))  for (fold in 1:5) {
    print(paste("Working on NN gap threshold", t, "in fold", fold))
    test_idx <- folds == fold

    # run nearest neighbors
    neighbors <- nn2(features[!test_idx,feature_cols],
                     query = features[test_idx,feature_cols],
                     k = max(nbhoods), treetype = "kd", 
                     searchtype = "standard", eps = 1e-5)
    
    for (k in 1:length(nbhoods)) {
        
        # skip iterations with #neighbors < gap threshold
        if (gap_thres[t] >= nbhoods[k]) next
        
        # get neighborhood
        nbh <- neighbors$nn.idx[ ,1:nbhoods[k]]
        
        
        clusterExport(cl, list("nbh", "t", "k", "test_idx"))
        
        # save the rank of true author in our results vector
        results[test_idx, k, t,] <- parSapply(cl, 1:sum(test_idx), function(idx){
            author_ranks <- features$author[nbh[idx,]] %>% table %>% sort(decreasing = T)
            true_author <- features$author[test_idx][idx]
            nm_authors <- names(author_ranks)
            names(author_ranks) <- NULL
            
            # get gap statistic
            gap <- author_ranks[1] - author_ranks[2]
            
            # output only if author is listed and gap is higher than threshold
            if (gap > gap_thres[t] | length(author_ranks == 1)) {
                return(sapply(authors, function(aut) {
                    ifelse(aut %in% nm_authors, author_ranks[nm_authors == aut], NA)
                }))
            } else {
                return(NA)
            }
        })
    }
    
}



# save results over validation set
savefolder <- gsub("Subsetting", "kNN/Tuning", subsetfolder)
savefile <- gsub(".csv$", "_results.csv", subset_f)

map2_df(rep(1:length(nbhoods), length(gap_thres)), rep(1:length(gap_thres), each = length(nbhoods)), function(nn, gt){
    iter_r <- results[,nn,gt,]
    colnames(iter_r) <- authors
    as_tibble(iter_r) %>%
        mutate(
            NN = nbhoods[nn],
            gap = gap_thres[gt],
            true_author = features$author
            )
}) %>%
    write_csv(paste0(savefolder, savefile))

# # select method
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

#   2b. Logistic regression with lasso
#   ---------------------------------------------

# gap threshold values?

n_lambdas <- 100
gap_thres <- c(1, 2, 3, 5, 8, 10, 15)/100
# we're going to vary over the gap threshold and 100 possible tuning parameters
# we'll allow glmnet to select the 100 possible tuning parameters
# this is done in a one-vs-all manner so we also iterate over authors

responses <- sapply(authors, function(author) as.numeric(features$author == author))

# find lambdas, using glmnet's strategy
f_matrix <- as.matrix(features[,feature_cols])
weights <- apply(responses, 2, 
                 function(a_col) {
                     round(length(a_col)/sum(a_col)) 
                 })
lambdas <- sapply(1:ncol(responses), # for each author get max lambda value
                  function(a_idx) {
                      colSums(f_matrix*responses[,a_idx]) %>% # dot product b/w author and all predictors
                          abs %>% max %>% # max dot product
                          `*`( weights[a_idx] / n_posts * 5 / 4)
                  }) # max lambda value
                

lambdas <- seq(from = 0.005 * max(lambdas), to = max(lambdas), length.out = n_lambdas)
ranks <- array(NA, dim = c(n_posts, n_lambdas, length(gap_thres), n_authors))

clusterCall(cl, function(){
    library(glmnet)
    library(magrittr)
})
clusterExport(cl, list("responses", "weights", "f_matrix", "lambdas", "features",
                       "gap_thres"))


for (t in 1:length(gap_thres)) for (fold in 1:5) {
    print(paste("Working on logistic reg gap threshold", t, "in fold", fold))
    test_idx <- folds == fold
    
    predictions <- rep(0, dim = c(sum(test_idx), n_authors, n_lambdas))
    clusterExport(cl, list("test_idx"))
    predictions <- parLapply(cl, 1:n_authors, function(aut_idx){
        response <- as.vector(responses[!test_idx,aut_idx])
        if (sum(response) > 1) {
            weight <- weights[aut_idx] * response 
            weight[weight == 0] <- 1
            # weight <- rep(1, sum(!test_idx))
            fit <- glmnet(x = f_matrix[!test_idx,], 
                          y = response, lambda = lambdas,
                          family = "binomial", weights = weight)
            pred <- predict(fit, f_matrix[test_idx,], type = "response")
            return(pred) # n_response x n_lambda
        }
        return(NA)
        })




    
    clusterExport(cl, list("predictions", "authors", "n_lambdas", "ranks"))
    # get rank of true author
    author_ranks <- parLapply(cl, 1:sum(test_idx), function(p_idx){
        by_post <- do.call("cbind", lapply(predictions, function(author){
                if (is.na(author)) return(0)
                author[p_idx,] %>% as.vector #1 x n_lambda
            }))  #n_lambda x n_

         sapply(1:n_lambdas, function(lambda){
            if (lambda > nrow(by_post)) return(rep(NA, ncol(by_post)))
            lambda <- by_post[lambda,] #1 x n_author
            l_order <- order(lambda, decreasing = TRUE)
            if (lambda[which(l_order == 1)] - lambda[which(l_order == 2)] < gap_thres[t]) {
                return(rep(NA, ncol(by_post)))
            }
            return(l_order)
            }) #n_author x n_lambda
        })

    for(p_idx in 1:sum(test_idx)) {
        ranks[which(test_idx)[p_idx],,t,] <- author_ranks[[p_idx]]
        }
}




# save results over validation set
savefolder <- gsub("Subsetting", "BGLM_Lasso/Tuning", subsetfolder)
savefile <- gsub(".csv$", "_results.csv", subset_f)

map2_df(rep(1:n_lambdas, length(gap_thres)), rep(1:length(gap_thres), each = n_lambdas), function(lamb, gt){
    iter_r <- ranks[,lamb,gt,]
    colnames(iter_r) <- authors
    as_tibble(iter_r) %>%
        mutate(
            lambda = lambdas[lamb],
            gap = gap_thres[gt],
            true_author = features$author
            )
}) %>%
    write_csv(paste0(savefolder, savefile))


# n_top20 <- apply(ranks, c(2,3), function(col) sum(col <= 20 & col > 0, na.rm = T))
# n_top10 <- apply(ranks, c(2,3), function(col) sum(col <= 10 & col > 0, na.rm = T))
# n_correct <- apply(ranks, c(2,3), function(col) sum(col == 1, na.rm = T))

# final <- rbind(n_top20, n_top10, n_correct)

stopCluster(cl)
# 3. Test performance over hold-out set


