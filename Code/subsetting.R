library(tidyverse)
library(data.table)
library(magrittr)


known_bots <- read_csv("../Data/bot_list.csv") %>% pull(Username)


file <- "../Data/RC_2017-02"
metadata <- read_csv(paste0(file, "_metadata.csv")) %>% #get metadata
    #join with word length feature
    full_join(fread(paste0(file, "_features.csv"), sep = ",", header = T,
                    select = c("id", "length_words"), stringsAsFactors = F),
              by = c("id")) %>%
    filter(!is.na(subreddit), !(author %in% known_bots))


set1 <- c("AskReddit", "The_Donald")
set2 <- c("worldnews", "news")

#filter authors who 
# - are not in the test set
# - have word lengths that are too consistent



set1_authors <- intersect(unique(metadata$author[metadata$subreddit == set1[1]]),
                       unique(metadata$author[metadata$subreddit == set1[2]]))
length(set1_authors)

set2_authors <- intersect(unique(metadata$author[metadata$subreddit == set2[1]]),
                          unique(metadata$author[metadata$subreddit == set2[2]]))
length(set2_authors)


set1_posts <- metadata %>% filter(subreddit %in% set1, 
                                  author %in% set1_authors) 

set2_posts <- metadata %>% filter(subreddit %in% set2, 
                                  author %in% set2_authors) 




# AskReddit -> The_Donald
set1_delete <- set1_posts %>% filter(subreddit %in% set1[1]) %>%
    group_by(author) %>% summarise(n_posts = n(),
                                   n_words = sum(length_words))
table(set1_delete$n_posts)
table(set1_delete$n_words)
set1_delete <- set1_delete  %>%
    filter(n_posts <= 5 | n_words <= 500)

set1_posts <- set1_posts %>% 
    filter(!(author %in% c("AutoModerator", set1_delete$author)))
length(unique(set1_posts$author))

delete_again <- set1_posts %>% group_by(author) %>%
    summarise(word95 = quantile(length_words, probs = 0.95),
              word05 = quantile(length_words, probs = 0.05),
              n_posts = n()) %>%
    mutate(diff = word95 - word05) %>%
    arrange(diff) %T>% View %>%
    filter(diff < 6)

write_csv(set1_posts %>% select(id, author, subreddit),
          "../Data/comments_AskReddit-TheDonald.csv")




# worldnews -> news
set2_delete <- set2_posts %>% filter(subreddit %in% set2[1]) %>%
    group_by(author) %>% summarise(n_posts = n(),
                                   n_words = sum(length_words))
table(set2_delete$n_posts)
table(set2_delete$n_words)
set2_delete <- set2_delete  %>%
    filter(n_posts <= 5 | n_words <= 500)

set2_posts <- set2_posts %>% 
    filter(!(author %in% c("AutoModerator", set2_delete$author)))
length(unique(set2_posts$author))

delete_again <- set2_posts %>% group_by(author) %>%
    summarise(word95 = quantile(length_words, probs = 0.95),
              word05 = quantile(length_words, probs = 0.05),
              n_posts = n()) %>%
    mutate(diff = word95 - word05) %>%
    arrange(diff) %T>% View %>%
    filter(diff < 6)

set2_posts <- set2_posts %>%
    filter(!(author %in% delete_again$author))

write_csv(set2_posts %>% select(id, author, subreddit),
          "../Data/comments_worldnews-news.csv")




# ---- get subsets over varying length of words nad author

#filter to authors w/in slice 

metadata <- metadata %>% select(id, author, length_words) %>%
    filter(author != "AutoModerator")


total_words <- metadata %>% group_by(author) %>% 
    summarise(n_words = sum(length_words),
              n_posts = n()) 

#word length quantiles
wq <- quantile(total_words$n_words, 
               probs = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.91, 0.92,
                         0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 0.995,
                         0.997, 1))

#remove authors with total words <= 500
remove_author <- total_words %>% 
    filter(n_words < 500 | 
               n_words > 9580 | 
               n_posts < 6) %>% #99.7 quantile 
    pull(author)
filtered <- metadata %>%
    filter(!(author %in% remove_author))


#now filter based on word diff
delete_again <- filtered %>% group_by(author) %>%
    summarise(word95 = quantile(length_words, probs = 0.95),
              word05 = quantile(length_words, probs = 0.05)) %>%
    mutate(diff = word95 - word05) %>%
    arrange(diff) %T>% View %>%
    write_csv("../Output/word_diff.csv")

delete_again <- delete_again %>% 
    filter(diff < 6) 

filtered <- filtered %>%
    filter(!(author %in% delete_again$author))


#now slice across word length
length(unique(filtered$author))
total_words <- filtered %>% group_by(author) %>% 
    summarise(n_words = sum(length_words),
              n_posts = n()) 

#word length quantiles
wq <- quantile(total_words$n_words, 
               probs = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.91, 0.92,
                         0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 0.995,
                         0.997, 1))

set.seed(1117)
#randomly select 5000 authors with total words between 600 and 700
total_words %>% filter(n_words >= 600, n_words <= 700) %>%
    sample_n(5000) %>%
    write_csv("../Data/600-700words_5000authors.csv")

#train on 1000-1500 words?
total_words %>% filter(n_words >= 1200, n_words <= 1700) %>%
    sample_n(5000) %>%
    write_csv("../Data/1200-1700words_5000authors.csv")


total_words %>% filter(n_words >= 4000, n_words <= 8000) %>%
    sample_n(5000) %>%
    write_csv("../Data/4000-8000words_5000authors.csv")


#slice across authors
authors50 <- total_words %>% 
    filter(n_words >= 1200, n_words <= 1700) %>%
    sample_n(50) %T>%
    write_csv("../Data/50authors.csv")


total_words %>% 
    sample_n(5000) %>%
    write_csv("../Data/5000authors.csv")


total_words %>% 
    write_csv("../Data/all_authors.csv")

