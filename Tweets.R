setwd("~/Desktop/Courses/Coursera/Data Science Specialization/Capstone/DataSets/final/en_US")
options(scipen=999) # remove sci notation
library(ggplot2)
library(quanteda)
library(gridExtra)
library(grid)

# 
# file <- 'en_US.twitter.txt'
# twitter = readLines(file, file.info(file)$size)

# saveRDS(twitter, "twitter.rds")

TwitterRDS = readRDS("twitter.rds")
file.info("twitter.rds")$size/1024^2 # size on disk

size_T=format(object.size(TwitterRDS), units = "Mb")
lines_T=length(TwitterRDS)
n_wordsT=sum(ntoken(TwitterRDS))

# QUIZ ########
# max(nchar(TwitterRDS))
# love_count <- sum(grepl("love", TwitterRDS))
# hate_count <- sum(grepl("hate", TwitterRDS))
# love_count / hate_count
# biostats <- grep("biostats", TwitterRDS)
# TwitterRDS[biostats]


nlines=length(TwitterRDS)
TwitterRDS <- unlist(TwitterRDS)
Encoding(TwitterRDS)  <- "UTF-8"
length(unique(TwitterRDS))

TwitRDS_A <- sample(TwitterRDS, size=nlines*0.05, replace=FALSE)
sum(nchar(TwitRDS_A))
length(TwitRDS_A)

UNIgramT=dfm(TwitRDS_A, ngrams = 1,verbose = TRUE, toLower = TRUE,
            removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
            removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
            keptFeatures = NULL)

BIgramT=dfm(TwitRDS_A, ngrams = 2,verbose = TRUE, toLower = TRUE,
           removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
           removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
           keptFeatures = NULL)

TRIgramT=dfm(TwitRDS_A, ngrams = 3,verbose = TRUE, toLower = TRUE,
           removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
           removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
           keptFeatures = NULL)

UNItopT = topfeatures(UNIgramT, 30)#####
UNItopTDF=data.frame(UNItopT)
UNItopTDF$ngrams <- rownames(UNItopTDF) 


BItopT = topfeatures(BIgramT, 30)#####
BItopTDF=data.frame(BItopT)
BItopTDF$ngrams <- rownames(BItopTDF) 
BItopTDF$ngrams <- gsub('_', ' ', BItopTDF$ngrams)


TRItopT = topfeatures(TRIgramT, 30)#####
TRItopTDF=data.frame(TRItopT)
TRItopTDF$ngrams <- rownames(TRItopTDF) 
TRItopTDF$ngrams <- gsub('_', ' ', TRItopTDF$ngrams)

write.csv(UNItopTDF, "UniSampTweets.csv",row.names = FALSE)
write.csv(BItopTDF, "BiSampTweets.csv",row.names = FALSE)
write.csv(TRItopTDF, "TriSampTweets.csv",row.names = FALSE)

# break 1

UNItopSortTweet=read.csv("UniSampTweets.csv")
BItopSortTweet=read.csv("BiSampTweets.csv")
TRItopSortTweet=read.csv("TriSampTweets.csv")

UNItopSortTweet = transform(UNItopSortTweet,ngrams = reorder(ngrams, -UNItopT))
BItopSortTweet = transform(BItopSortTweet,ngrams = reorder(ngrams, -BItopT))
TRItopSortTweet = transform(TRItopSortTweet,ngrams = reorder(ngrams, -TRItopT))

UNItopSortTweet = UNItopSortTweet[1:20,]
BItopSortTweet = BItopSortTweet[1:20,]
TRItopSortTweet = TRItopSortTweet[1:20,]

# break 2

UNItwit = ggplot(UNItopSortTweet, aes(x=ngrams,y=UNItopT)) +xlab("") + ylab("frequency")
UNItwit = UNItwit + geom_bar(stat="Identity", fill="indianred1") +ggtitle("uni-gram")
UNItwit = UNItwit + theme_light() + theme(plot.margin=unit(c(.25,0,0.85,-0.05), "cm"))
UNItwit = UNItwit + theme(axis.text.x = element_text(angle = 45, hjust = 1))
UNItwit = UNItwit + scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,50000, by=10000),expand = c(0,0))
UNItwit

BItwit = ggplot(BItopSortTweet, aes(x=ngrams,y=BItopT)) +xlab("")+ ylab("")
BItwit = BItwit +  geom_bar(stat="Identity", fill="indianred2") +ggtitle("bi-gram")
BItwit = BItwit + theme_light()+ theme(plot.margin=unit(c(.25,0, .3,-0.05), "cm"))
BItwit = BItwit + theme(axis.text.x = element_text(angle = 45, hjust = 1))
BItwit = BItwit +scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,4000, by=500),expand = c(0,0))
BItwit

TRItwit = ggplot(TRItopSortTweet, aes(x=ngrams,y=TRItopT)) +xlab("")+ ylab("")
TRItwit = TRItwit +  geom_bar(stat="Identity", fill="indianred3") +ggtitle("tri-gram")
TRItwit = TRItwit + theme_light() + theme(plot.margin=unit(c(.25,.5,-.57,-0.2), "cm"))
TRItwit = TRItwit + theme(axis.text.x = element_text(angle = 45, hjust = 1))
TRItwit = TRItwit +scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,1200, by=250),expand = c(0,0))
TRItwit

barChartsTweets=arrangeGrob(UNItwit, BItwit, TRItwit,nrow = 1, ncol = 3,top = textGrob("Tweets"))

gridT=grid.draw(barChartsTweets)

#ggsave(grid,file="grid.png",size=1.2)
