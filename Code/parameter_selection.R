library(tidyverse)
library(magrittr)

results <- list.files(".")[-c(1:2)]


idx <- 6
results[idx]
df <- read_csv(results[idx]) 
colnames(df)[1] <- "author"

long_df <- gather(df, key = "params", value = "rank", 2:ncol(df))
max(long_df$rank[!is.infinite(long_df$rank)])


author_posts <- df %>% group_by(author) %>%
    summarise(n_posts = n()) %>%
    arrange(author)

weight <- as.factor(df$author)
levels(weight) <- as.character(author_posts$n_posts)
weight <- as.numeric(as.character(weight))


rank_summaries <- apply(df[,-1], 2, function(col){
    sapply(1:100, function(i){
        valid <- col == i
        return(sum(rep(1, sum(valid))/weight[valid]))
    })
}) 

top <- apply(rank_summaries, 1, function(row){
    colnames(df)[-1][which.max(row)]
}) %>% unique

cdfs <- as_tibble(rank_summaries[,top]) %>%
    mutate(rank = 1:nrow(rank_summaries)) %>%
    gather(key = "params", value = "weight", 1:length(top)) %>%
    group_by(params) %>%
    mutate(
        cdf_weight = cumsum(weight)
    ) 

cdfs %>%
    ggplot(aes(x = rank, y = cdf_weight, color = params)) +
    geom_line() +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 100, by = 5))

filter(cdfs, rank < 20) %>%
    ggplot(aes(x = rank, y = cdf_weight, color = params)) +
    geom_line() +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 100, by = 5))


results[idx]



subsets <- list.files(".")[-c(1,2,10)]
file <- fread(subsets[1], select=c("author")) %>%
    group_by(author) %>%
    summarise(n_posts = n())
