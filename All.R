setwd("~/Desktop/Courses/Coursera/Data Science Specialization/Capstone/DataSets/final/en_US")
library(ggplot2)
library(quanteda)

NewsRDS = readRDS("News.rds")
NewsRDS_A <- unlist(NewsRDS)
Encoding(newsRDS_A)  <- "UTF-8"
BlogsRDS = readRDS("Blogs.rds")
BlogsRDS_A <- unlist(BlogsRDS)
Encoding(BlogsRDS_A)  <- "UTF-8"
TwitterRDS = readRDS("Twitter.rds")
TwitRDS_A <- unlist(TwitterRDS)
Encoding(TwitRDS_A)  <- "UTF-8"


ALL = c(newsRDS_A,blogsRDS_A,TwitRDS_A)
sum(nchar(ALL))
length(ALL)

ALLsamp <- sample(ALL, size=nlines*0.05, replace=FALSE)
sum(nchar(ALLsamp))
length(ALLsamp)

UNIgram=dfm(ALLsamp, ngrams = 1,verbose = TRUE, toLower = TRUE,
            removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
            removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
            keptFeatures = NULL)

BIgram=dfm(ALLsamp, ngrams = 2,verbose = TRUE, toLower = TRUE,
           removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
           removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
           keptFeatures = NULL)

TRIgram=dfm(ALLsamp, ngrams = 3,verbose = TRUE, toLower = TRUE,
            removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
            removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
            keptFeatures = NULL)

UNItop = topfeatures(UNIgram, 30)#####
UNItopDF=data.frame(UNItop)
UNItopDF$ngrams <- rownames(UNItopDF) 

BItop = topfeatures(BIgram, 30)#####
BItopDF=data.frame(BItop)
BItopDF$ngrams <- rownames(BItopDF) 
BItopDF$ngrams <- gsub('_', ' ', BItopDF$ngrams)

TRItop = topfeatures(TRIgram, 30)#####
TRItopDF=data.frame(TRItop)
TRItopDF$ngrams <- rownames(TRItopDF) 
TRItopDF$ngrams <- gsub('_', ' ', TRItopDF$ngrams)


write.csv(UNItopDF, "UniSampAll.csv",row.names = FALSE)
write.csv(BItopDF, "BiSampAll.csv",row.names = FALSE)
write.csv(TRItopDF, "TriSampAll.csv",row.names = FALSE)

UNItopSortAll=read.csv("UniSampAll.csv")
BItopSortAll=read.csv("BiSampAll.csv")
TRItopSortAll=read.csv("TriSampAll.csv")

UNItopSortAll = UNItopSortAll[1:20,]
BItopSortAll = BItopSortAll[1:20,]
TRItopSortAll = TRItopSortAll[1:20,]

UNItopSortAll = transform(UNItopSortAll,ngrams = reorder(ngrams, -UNItop))
BItopSortAll = transform(BItopSortAll,ngrams = reorder(ngrams, -BItop))
TRItopSortAll = transform(TRItopSortAll,ngrams = reorder(ngrams, -TRItop))


UNIall = ggplot(UNItopSortAll, aes(x=ngrams,y=UNItop)) +xlab("") + ylab("Frequency")
UNIall = UNIall + geom_bar(stat="Identity", fill="mediumorchid2") + ggtitle("Uni-gram")
UNIall = UNIall + theme_light() + theme(plot.margin=unit(c(.25,0,1.3,0), "cm"))
UNIall = UNIall + theme(axis.text.x = element_text(angle = 45, hjust = 1))
UNIall = UNIall + scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,60000, by = 10000),expand = c(0,0))
UNIall

BIall = ggplot(BItopSortAll, aes(x=ngrams,y=BItop)) +xlab("")+ ylab("")
BIall = BIall +  geom_bar(stat="Identity", fill="mediumorchid3") + ggtitle("Bi-gram")
BIall = BIall + theme_light() + theme(plot.margin=unit(c(.25,0,0.85,0), "cm"))
BIall = BIall + theme(axis.text.x = element_text(angle = 45, hjust = 1))
BIall = BIall +scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,5500, by=1000),expand = c(0,0))
BIall 

TRIall = ggplot(TRItopSortAll, aes(x=ngrams,y=TRItop)) +xlab("")+ ylab("")
TRIall = TRIall +  geom_bar(stat="Identity", fill="mediumorchid4") + ggtitle("Tri-gram")
TRIall = TRIall + theme_light() + theme(plot.margin=unit(c(.25,0.5,0,0), "cm"))
TRIall = TRIall + theme(axis.text.x = element_text(angle = 45, hjust = 1))
TRIall = TRIall +scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,450, by=100),expand = c(0,0))
TRIall

barCharts=arrangeGrob(UNIall, BIall,TRIall,nrow = 1, ncol = 3,
  left=textGrob("Combined",y = 0.6 ,gp = gpar(fontsize=18, face = "bold", col="mediumorchid4")))

grid=grid.draw(barCharts)

ggsave(filename="ALL.png",barCharts,width = 40,height = 10, units = "cm")
