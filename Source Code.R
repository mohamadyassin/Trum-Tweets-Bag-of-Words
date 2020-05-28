#Install packages

df_1 <- read.csv('Trump Tweets 2010-2015.csv', stringsAsFactors = F)
df_2 <- read.csv('Trump Tweets 2016-2020.csv', stringsAsFactors = F)
getwd()
#Create char vector out of df
tweet1 <- df_1$text
#test
head(tweet1)
str(tweet1)
class(tweet1)
#this char vector includes 20,512 tweets between 2010 and 2015

tweet2 <- df_2$text
#test
head(tweet2)
str(tweet2)
class(tweet2)
#this char vector includes 19,692 tweets between 2016 and May 21, 2020

#make a vector source
vecSource1 <- VectorSource(tweet1)
vecSource2 <- VectorSource(tweet2)

#make a volatile corpus
tweet_corp1 <- VCorpus(vecSource1)
tweet_corp2 <- VCorpus(vecSource2)

#if you're working with a data frame, fix column names to pass it to 
#DataframeSource(). I prefer this method when I want to preserve the metadata
# df_tweet1 <- as.data.frame(Trump_Tweets_2010_2015)
# df_tweet2 <- as.data.frame(Trump_Tweets_2016_2020)
# 
# names(df_tweet1)[1] <- "doc_id"
# names(df_tweet1)[2] <- "text"
# 
# names(df_tweet2)[1] <- "doc_id"
# names(df_tweet2)[2] <- "text"
# 
# df_source1 <- DataframeSource(df_tweet1)
# df_source2 <- DataframeSource(df_tweet2)
# 
# #convert to VCorpus
# df_corpus1 <- VCorpus(df_source1)
# df_corpus2 <- VCorpus(df_source2)


#cleanup corpus
clean_corpus <- function(corpus){
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Remove @ signs
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "@", replacement = "")
  # Remove ! signs
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "!", replacement = "")
  # Remove / signs
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "/", replacement = "")
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # Remove stopwords
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), "donaldtrump", "trump", "president","donald", "realdonaldtrump"))
  return(corpus)
}

tweet_corp1[[14455]][1]
tweet_corp2[[370]][1]
clean_corpu1[[14455]][1]
clean_corpu2[[3700]][1]



#function didn't work for some reason. To save some debugging time for later
#we will apply cleanup functions individually

# Transform to lower case
clean_corpu1 <- tm_map(tweet_corp1, content_transformer(tolower))
# Remove punctuation
clean_corpu1 <- tm_map(clean_corpu1, removePunctuation)
# Remove @ ! / signs
clean_corpu2 <- tm_map(clean_corpu2, content_transformer(gsub), pattern = "@ | ! | /", replacement = "")
# Remove stopwords
clean_corpu1 <- tm_map(clean_corpu1, removeWords, words = c(stopwords("en"), "donaldtrump", "trump", "president","donald", "realdonaldtrump", "whitehouse", "white", "house"))
# Strip whitespace
clean_corpu1 <- tm_map(clean_corpu1, stripWhitespace)

# Transform to lower case
clean_corpu2 <- tm_map(tweet_corp2, content_transformer(tolower))
# Remove punctuation
clean_corpu2 <- tm_map(clean_corpu2, removePunctuation)
# Remove @ ! / signs
clean_corpu2 <- tm_map(clean_corpu2, content_transformer(gsub), pattern = "@ | ! | /", replacement = "")
# Remove stopwords
clean_corpu2 <- tm_map(clean_corpu2, removeWords, words = c(stopwords("en"), "donaldtrump", "trump", "president","donald", "realdonaldtrump", "whitehouse", "white", "house"))
# Strip whitespace
clean_corpu2 <- tm_map(clean_corpu2, stripWhitespace)


#create TDM
tweet_tdm1 <- TermDocumentMatrix(clean_corpu1)
tweet_tdm2 <- TermDocumentMatrix(clean_corpu2)

#convert to matrix
tweet_m1 <- as.matrix(tweet_tdm1)
tweet_m2 <- as.matrix(tweet_tdm2)
#have these functions ready because they are too big to keep in memory unless needed
rm(list = "tweet_m1")
rm(list = "tweet_m2")

dim(tweet_m1)
dim(tweet_m2)

#create term frequency
term_freq1 <- rowSums(tweet_m1)
term_freq2 <- rowSums(tweet_m2)
#sort
term_freq1 <- sort(term_freq1, decreasing = T)
term_freq2 <- sort(term_freq2, decreasing = T)

#bar plot
barplot(term_freq1[1:10], col="steelblue1", las=2)
barplot(term_freq2[1:10], col="steelblue1", las=2)


#convert term_freq into a df
wc_df1 <- data.frame(term = names(term_freq1), num = term_freq1)
wc_df2 <- data.frame(term = names(term_freq2), num = term_freq2)

#make word cloud
wordcloud2(wc_df1, size = 1,
           color = c("tomato","gold","black"), backgroundColor = "white")
wordcloud2(wc_df2, size = 1,
           color = c("tomato","gold","black"), backgroundColor = "white")

#https://news.stanford.edu/2019/08/22/the-power-of-language-how-words-shape-people-culture/


#collapse all tweets
all_tweets1 <- paste(Trump_Tweets_2010_2015$text, collapse = "")
all_tweets2 <- paste(Trump_Tweets_2016_2020$text, collapse = "")
all_tweets <- c(all_tweets1,all_tweets2)

all_vc <- VectorSource(all_tweets)
all_corpus <- VCorpus(all_vc)

# Transform to lower case
all_clean <- tm_map(all_corpus, content_transformer(tolower))
# Remove punctuation
all_clean <- tm_map(all_clean, removePunctuation)
# Remove @ ! / signs
all_clean <- tm_map(all_clean, content_transformer(gsub), pattern = "@ | ! | /", replacement = "")
# Remove stopwords
all_clean <- tm_map(all_clean, removeWords, words = c(stopwords("en"), "donaldtrump", "trump", "president","donald", "realdonaldtrump", "whitehouse", "white", "house"))
# Strip whitespace
all_clean <- tm_map(all_clean, stripWhitespace)


all_tdm <- TermDocumentMatrix(all_clean)
all_m <- as.matrix(all_tdm)

#commonality cloud

jpeg('rplot.jpg', width = 1000, height = 1000, units = "px", pointsize = 12,
     quality = 100)
commonality.cloud(all_m, colors = "steelblue1", max.words = 75)
dev.off()
?jpeg
#comparison cloud
colnames(all_tdm) <- c("before presidency", "after presidency")
comparison.cloud(all_m,
                 colors = c("orange", "blue"),
                 max.words = 100)
?comparison.cloud
#pyramid plot
common_words <- subset(all_m,
                       all_m[,1]>0 & all_m[,2]>0)
difference <- abs(common_words[,1] - common_words[,2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[,3],
                                   decreasing = T),]
top25_df <- data.frame(x = common_words[1:25,1],
                       y= common_words[1:25,2],
                       labels = rownames(common_words[1:25,]))


pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels,
             main = "words in common",
             gap = 300, laxlab = NULL,
             raxlab = NULL,
             space = 0.5,
             unit = NULL,
             labelcex=0.75,
             top.labels = c("before presidency" ,"words","after presidency"))

#making a dendogram friendly tdm
dim(tweet_tdm1)
rm_sparce_tdm1 <- removeSparseTerms(tweet_tdm1, sparse = 0.975)
dim(rm_sparce_tdm1)

dim(tweet_tdm2)
rm_sparce_tdm2 <- removeSparseTerms(tweet_tdm2, sparse = 0.975)
dim(rm_sparce_tdm2)

#create matrix
sparce_m1 <- as.matrix(rm_sparce_tdm1)
sparce_m2 <- as.matrix(rm_sparce_tdm2)

#create dist
tweet_dist1 <- dist(sparce_m1)
tweet_dist2 <- dist(sparce_m2)

#create hc
hc1 <- hclust(tweet_dist1)
hc2 <- hclust(tweet_dist2)

#plot
plot(hc1)
plot(hc2)
install.packages("ape")
library(ape)
#create hcd
hcd1 <- as.dendrogram(hc1)
hcd2 <- as.dendrogram(hc2)
labels(hcd1)
labels(hcd2)
?dendrogram
?hclust
order.dendrogram(hcd1)

plot(hcd1, type = "triangle", ylab = "Height")
plot(hcd2)

?ngrams()

?branches_attr_by_clusters
?order.dendrogram()


#N-gram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "),
         use.names = FALSE)
}

bigram_tdm1 <- TermDocumentMatrix(clean_corpu1,
                                  control=list(tokenize=bigramTokens))

bigram_tdm2 <- TermDocumentMatrix(clean_corpu2,
                                  control=list(tokenize=bigramTokens))

#sparce
bigram_sparce1 <- removeSparseTerms(bigram_tdm1, sparse = 0.999)
bigram_sparce2 <- removeSparseTerms(bigram_tdm2, sparse = 0.999)


#convert to matrix
bigram_m1 <- as.matrix(bigram_sparce1)
bigram_m2 <- as.matrix(bigram_sparce2)

#freq terms
bigram_freq1 <- rowSums(bigram_m1)
bigram_freq2 <- rowSums(bigram_m2)

#sort
sort_freq1 <- sort(bigram_freq1, decreasing = T)
sort_freq2 <- sort(bigram_freq2, decreasing = T)


#plot
par(mar=c(3, 9.5, 1, 2))
barplot(sort_freq1[1:10], col = "blue", las=2, horiz = T)
barplot(sort_freq2[1:10], col = "blue", las=2, horiz = T)

dev.off()

#creating a word cloud

#convert the term freq verctor into df
word_freq1 <- data.frame(term=names(sort_freq1), num=sort_freq1)
word_freq2 <- data.frame(term=names(sort_freq2), num=sort_freq2)

#make word cloud
wordcloud2(word_freq1, size = 1,
           color = c("tomato","gold","black"), backgroundColor = "white")

wordcloud2(word_freq2, size = 1,
           color = c("tomato","gold","steelblue"), backgroundColor = "white")




