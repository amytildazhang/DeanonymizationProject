#-----------------------------------------------------------------------------#
# File description:
# Load part of the reddit 02/2017 data 
# and do a bipartite projection on the authors-subreddit 'network'
# Input: RC_2017-02.bz2
# Output:
#-----------------------------------------------------------------------------#

library('tidyverse')
library('jsonlite')
library('igraph')
library('slam')
library('pbapply')
library('lsa')
library('tm')
library('corrplot')

# read in data
con = file("reddit_data/RC_2017-02.bz2", "r")
df <- readLines(con, n = 500000)
close(con)
df <- fromJSON(sprintf("[%s]", paste(df,collapse=",")))

# convert to tibble, and remove authors we don't care about
r <- as.tibble(df) %>%
  select(author, body, created_utc, subreddit) %>%
  filter(!author %in% c("[deleted]", "AutoModerator"))

# Remove all but the 100 subreddits with the most posts
popular.subreddits <- sort(table(r$subreddit), decreasing = T)[1:500] %>% 
  names()
r <- filter(r, subreddit %in% popular.subreddits)

# Remove all authors with less than 10 posts
#frequent.posters <- table(r$author)[table(r$author)>=5]
#frequent.posters <- names(frequent.posters)
#r <- filter(r, author %in% frequent.posters)

# create an 'edgelist' between author and subreddit
r.edgelist <- select(r, author, subreddit)
r.edgelist <- unique(r.edgelist)

#retain only authors who post in more than one subreddit
eclectic.posters <- r.edgelist$author[duplicated(r.edgelist$author)]
r.edgelist <- filter(r.edgelist, author %in% eclectic.posters)


# BIPARTITE NETWORK STUFF:

# the two nodes of the bipartite network
nodes.authors <- sort(unique(r.edgelist$author))
nodes.subreddits <- sort(unique(r.edgelist$subreddit))

#create the network
g <- igraph::graph.empty()
g <- igraph::add.vertices(g,nv=length(nodes.subreddits),
                          attr=list(name=nodes.subreddits,
                                    type=rep(FALSE,length(nodes.subreddits))))
g <- igraph::add.vertices(g,nv=length(nodes.authors),
                          attr=list(name=nodes.authors,
                                    type=rep(TRUE,length(nodes.authors))))

# turn the edgelist into a vector
edgeListVec <- as.vector(t(as.matrix(data.frame(S1=r.edgelist$subreddit,
                                                S2=r.edgelist$author))))
g <- igraph::add.edges(g, edgeListVec)

# make undirected
g <- igraph::as.undirected(g)

# do bipartite projection by hand
# (igraph has a function for this but the resultant matrix is binary)
# use sparse matrices as much as possible
g.m <- as_incidence_matrix(g)
subreddits.names <- rownames(g.m)
g.m <- as.simple_triplet_matrix(g.m)

# bipartite projection
subreddit.proj.adj.m <- tcrossprod_simple_triplet_matrix(g.m)
# we don't care about the diagonal or one of the triangluar parts of the matrix
diag(subreddit.proj.adj.m) <- 0
subreddit.proj.adj.m[lower.tri(subreddit.proj.adj.m)] <- 0

# turn back into a sparse matrix
# the column/row indices are helpful for the next step
subreddit.proj.adj.m <- slam::as.simple_triplet_matrix(subreddit.proj.adj.m)

# dataframe for the counts of crossposting people per subreddit
linked.subreddits <- data.frame(count = subreddit.proj.adj.m$v,
                                nodeSet1 = subreddit.proj.adj.m$i,
                                nodeSet2 = subreddit.proj.adj.m$j)

# sort by crossposting people count
linked.subreddits <- linked.subreddits[order(linked.subreddits$count, decreasing = T),]

#turn the subreddit numbers back into the names
linked.subreddits$nodeSet1Name <- subreddits.names[linked.subreddits$nodeSet1]
linked.subreddits$nodeSet2Name <- subreddits.names[linked.subreddits$nodeSet2]


# The above dataframe is an edgelist for a one-mode network. Plot it:

# use only the top 50 connections
linked.subreddits2 <- linked.subreddits[1:50,]

#write top projection subreddits to file
write_csv(linked.subreddits2[,4:5], '../Output/subreddit_projection.txt')

# create an edgelist matrix
el <- as.matrix(cbind(linked.subreddits2$nodeSet1Name,
                      linked.subreddits2$nodeSet2Name))
# create the graph
g <- graph.edgelist(el)
# weight the edges according to the crossposting people count
E(g)$weight=as.numeric(linked.subreddits2$count)
# make the graph undirected
g <- igraph::as.undirected(g)

# Plot the graph, with edge weights determining width of the lines
png('../Output/subreddit_projection.png', width = 1000, height = 1000)
plot(g,layout = layout.fruchterman.reingold,
     edge.width = E(g)$weight/min(E(g)$weight))
dev.off()

#------------------------------------------------------------------------------

# Cosine similarity matrix for subreddits

# Subreddits in the graph above
subreddits <- unique(c(linked.subreddits2$nodeSet1Name, linked.subreddits2$nodeSet2Name))

# Randomly sample 20 posts from each of the subreddits and concatenate them
ConcatenateSubreddits <- function(i){
  df$body[df$subreddit == subreddits[i]] %>%
    sample(20) %>%
    paste0(collapse = " ")
}
subreddit.samples <- pbsapply(1:length(subreddits), ConcatenateSubreddits)

# create corpus, then term-document matrix
crps <- Corpus(VectorSource(subreddit.samples))
tdm <- TermDocumentMatrix(crps)
tdm <- as.matrix(tdm)

# cosine similarity
cosine.mat <- cosine(tdm)
colnames(cosine.mat) <- subreddits
rownames(cosine.mat) <- subreddits

# correlation plot
png('../Output/cosine_corrplot.png', width = 1000, height = 1000)
corrplot(cosine.mat, diag = F, order = "FPC", tl.col = "black")
dev.off()
