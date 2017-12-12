#-----------------------------------------------------------------------------#
# File description:
# I/O
# Input: reddit_data/RC_feb17_subset.csv
# Output: reddit_data/AskReddit_TheDonald.csv reddit_data/worldnews-news.csv
#-----------------------------------------------------------------------------#

options(java.parameters = "-Xmx20000m")
library('mallet')
library('stringr')
library('data.table')
library('textmineR')
library('parallel')
library('pbapply')

set.seed(1117)

setwd("~/Dropbox/5_SoDA502/project")
#load("reddit_data/worldnews-news.rdata")
#retain <- fread("reddit_data/5000authors.csv")
filepath <- "./data_502"
files <- list.files(path = filepath, full.names = T)

#which file do we want to work on?
files.order <- 5

current.file <- files[files.order]

df2 <- fread(current.file)

#df2 <- subset(df2, author %in% c(retain$author))

#pre-processing
df2$body <- str_replace_all(df2$body, "[^A-Za-z ]", "")
df2$body <- gsub("\\s+"," ", df2$body)
df2$body <- tolower(df2$body)
df2 <- df2[!df2$body=="",]

#fwrite(df2, file = "reddit_data/df2_5000authors.csv")

#The stopwords file is purposefully empty; removing stopwords is already done in preprocessing
mallet.instances <- mallet.import(id.array = make.unique(rep("text", nrow(df2))),
                                  text.array = df2$body,
                                  stoplist.file = "./reddit_data/stopwords.txt",
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

# LDA training with standard parameters
ntopics <- 1000
topic.model <- MalletLDA(num.topics = ntopics)
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
topic.model$setAlphaOptimization(20, 50)
topic.model$train(200)
topic.model$maximize(10)

# document-topic and topic-word matrices
doc.topics <- mallet.doc.topics(topic.model, smoothed = T, normalized = T)
topic.words <- mallet.topic.words(topic.model, smoothed = T, normalized = T)

save(doc.topics, file = paste0("results/doctopicm_", str_split(current.file, "_")[[1]][3], ".rdata"))

# training test set random split
#tt_split <- sample(c(0,1), nrow(df2), T, prob = c(.2, .8))

folds <- 5

tt_split <- sample(c(1:folds), nrow(df2), T)
save(tt_split, file = paste0("results/split_", str_split(current.file, "_")[[1]][3], ".rdata"))

accuracies <- list()
rankings <- list()
first.pred <- list()

for(k in 1:folds){

  test.ind <- which(tt_split == k)
  train.ind <- which(tt_split != k)
  
  #training <- doc.topics[tt_split == 0,]
  #testing <- doc.topics[tt_split == 1,]
  #training.authors <- df2$author[tt_split == 0]
  #testing.authors <- as.character(df2$author[tt_split == 1])
  
  training <- doc.topics[train.ind,]
  testing <- doc.topics[test.ind,]
  training.authors <- df2$author[train.ind]
  testing.authors <- as.character(df2$author[test.ind])
  
  # training/testing dataset
  #training <- doc.topics[df2$subreddit == "worldnews",]
  #testing <- doc.topics[df2$subreddit == "news",]
  
  #training.authors <- df2$author[df2$subreddit == "worldnews"]
  #testing.authors <- as.character(df2$author[df2$subreddit == "news"])
  
  training.author.agg <- aggregate(training, by = list(training.authors), FUN = mean)
  training.author.agg2 <- as.matrix(training.author.agg[,-1])
  rownames(training.author.agg2) <- training.author.agg$Group.1
  
  #HellingerDist(testing[1,], as.numeric(training.author.agg[1,2:101]))
  
  #CalcHellingerDist(testing[1,], as.numeric(training.author.agg[1,2:101]))
  
  #predictions <- character(nrow(testing))
  #for(i in 1:nrow(testing)){
  #  test.results <- apply(training.author.agg2, 1, CalcHellingerDist, testing[i,])
  #  predictions[i] <- names(sort(test.results, decreasing = F))[1]
    #print(names(sort(test.results, decreasing = T))[1])
  #}
  
  #unlist(predictions)
  
  ####
  #print(mean(predictions==testing.authors))
  #accuracies[[i]] <- mean(predictions==testing.authors)
  
  # Parallelized:
  hell <- function(i){
    test.results <- apply(training.author.agg2, 1, CalcHellingerDist, testing[i,])
    test.results <- sort(test.results, decreasing = F)
    predicted <- names(test.results)[1]
    correct.rank <- which(names(test.results) == testing.authors[i])
    #conditional in case an author in the test set isn't in the training set
    if(length(correct.rank) == 0){correct.rank <- 0}
    return(c(correct.rank, predicted))
    #return(correct.rank)
  }
  
  cl <- makeForkCluster(detectCores()-2)
  test <- pblapply(1:length(testing.authors), hell, cl = cl) #length(testing.authors)
  stopCluster(cl)
  
  
  hell.out <- unlist(test)
  correct.ranks <- as.numeric(hell.out[seq(1, length(hell.out), 2)])
  predicted.authors <- hell.out[seq(2, length(hell.out), 2)]
  
  hell.out <- do.call(rbind, test)
  correct.ranks <- as.numeric(hell.out[,1])
  
  #accuracy
  print(mean(correct.ranks==1))
  accuracies[[k]] <- mean(correct.ranks==1)
  rankings[[k]] <- correct.ranks
  first.pred[[k]] <- predicted.authors

}

save(rankings, file = paste0("results/rankings_", str_split(current.file, "_")[[1]][3], ".rdata"))
save(accuracies, file = paste0("results/accuracies_", str_split(current.file, "_")[[1]][3], ".rdata"))
save(first.pred, file = paste0("results/first.pred_", str_split(current.file, "_")[[1]][3], ".rdata"))

