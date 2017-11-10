library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(RANN) #for k nearest neighbors

#-------------------------set parameters-------------------------------
# limit data to specific subreddits?
subreddits <- FALSE

# path to feature files?
file <- "../Data/RC_2017-02"

# path to folder with subsets?
subsetfolder <- "../Output/Subsetting/"
# which subset are we using?
list.files(subsetfolder)
i <- 5

# where to save results?
outputfolder <- "../Output/"

#------------------------specify which comments/authors to use---------------------------

options(tibble.print_max = Inf) 
subset <- read_csv(paste0(subsetfolder, data_subsets[i])) 
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

metadata <- read_csv_chunked(paste0(file, "_metadata.csv"),
                             DataFrameCallback$new(extract_records), 
                             col_types = "cccciiii")


features <- read_csv_chunked(paste0(file, "_features.csv"), 
                     DataFrameCallback$new(extract_records)) %>%
    filter(id %in% metadata$id) %>%
    inner_join(metadata, by = c("id", "author", "subreddit"))

id_cols <- c("id", "subreddit_id", "author", "plot", "subreddit")
feature_cols <- colnames(features) %>% setdiff(id_cols)
features[is.na(features)] <- 0

#-------------------- k-NN--------------------------------

# steps
# 1. Create hold-out set
# 2. 5-fold cross-validation over training set to select tuning parameters
# 3. Calculate performance measures
# 4. Test performance over hold-out set



# 1. Create hold-out set
# ----------------------
set.seed(1117)
ho_idx <- as.logical(rbinom(nrow(features), 1, 0.2))
ho_set <- features[ho_idx, ]
features <- features[!ho_idx, ]


# 2. 5-fold cross-validation over training set to select tuning parameters
# ---------------------------------------------
folds <- sample(1:5, size = nrow(features), replace = TRUE) #create fold

# we're going to vary over size of neighbors and gap threshold
nbhoods <- c(2, 5, 10, 20, 50, 100, 150, 200, 250, 500)
nbhoods <- nbhoods[nbhoods < length(subset_authors)]

gap_thres <- c(1, 2, 3, 5, 10, 20, 30, 40, 50)
gap_thres <- gap_thres[gap_thres < max(nbhoods) / 3]

# initialize results matrix; will have ranks of correct authors
results <- array(0, dim = c(nrow(features), length(nbhoods), length(gap_thres))) 
colnames(results) <- nbhoods
rownames(results) <- features$author

# look over neighborhood size and fold
for (t in 1:length(gap_thres))  for (i in 1:5) {
    test_idx <- folds == i

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

        # save the rank of true author in our results vector
        results[test_idx, k, t] <- sapply(1:sum(test_idx), function(idx){
            authors <- features$author[nbh[idx,]] %>% table
            true_author <- features$author[test_idx][idx]
            
            # get gap statistic
            gap <- authors[1] - authors[2]
            
            # output only if author is listed and gap is higher than threshold
            if (true_author %in% names(authors) & 
                (gap > gap_thres[t] | length(authors == 1))) {
                return(which(names(authors) == true_author))
            } else {
                return(NA)
            }
        })
    }
    
}


# select method
n_top20 <- apply(results, c(2,3), function(col) sum(col <= 20, na.rm = T))
n_top10 <- apply(results, c(2,3), function(col) sum(col <= 10, na.rm = T))
n_correct <- apply(results, c(2,3), function(col) sum(col == 1, na.rm = T))

final <- (3*n_correct + 2*n_top10 + n_top20) / 6
colnames(final) <- paste0("t_", gap_thres)
rownames(final) <- paste0("k_", nbhoods)


# get precision/recall of various 

# 3. Test performance over hold-out set


#--------------------toy example --------------------------------
