setwd("~/Desktop/Courses/Coursera/Data Science Specialization/Capstone/DataSets/final/en_US")
library(ggplot2)
library(quanteda)

# file <- 'en_US.news.txt'
# news = readLines(file, file.info(file)$size)
# saveRDS(news, "news.rds")


newsRDS = readRDS("news.rds")
nlines=length(newsRDS)
newsRDS <- unlist(newsRDS)
Encoding(newsRDS)  <- "UTF-8"

newsRDS_A <- sample(newsRDS, size=nlines*0.05, replace=FALSE)
sum(nchar(newsRDS_A))
length(newsRDS_A)

UNIgramN=dfm(newsRDS_A, ngrams = 1,verbose = TRUE, toLower = TRUE,
            removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
            removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
            keptFeatures = NULL)

BIgramN=dfm(newsRDS_A, ngrams = 2,verbose = TRUE, toLower = TRUE,
           removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
           removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
           keptFeatures = NULL)

TRIgramN=dfm(newsRDS_A, ngrams = 3,verbose = TRUE, toLower = TRUE,
            removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
            removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
            keptFeatures = NULL)


UNItopN = topfeatures(UNIgramN, 30)#####
UNItopNDF=data.frame(UNItopN)
UNItopNDF$ngrams <- rownames(UNItopNDF) 

BItopN = topfeatures(BIgramN, 30)#####
BItopNDF=data.frame(BItopN)
BItopNDF$ngrams <- rownames(BItopNDF) 
BItopNDF$ngrams <- gsub('_', ' ', BItopNDF$ngrams)

TRItopN = topfeatures(TRIgramN, 30)#####
TRItopNDF=data.frame(TRItopN)
TRItopNDF$ngrams <- rownames(TRItopNDF) 
TRItopNDF$ngrams <- gsub('_', ' ', TRItopNDF$ngrams)

# write.csv(UNItopNDF, "UniSampNews.csv",row.names = FALSE)
# write.csv(BItopNDF, "BiSampNews.csv",row.names = FALSE)
# write.csv(TRItopNDF, "TriSampNews.csv",row.names = FALSE)

# break 1

UNItopSortNews=read.csv("UniSampNews.csv")
BItopSortNews=read.csv("BiSampNews.csv")
TRItopSortNews=read.csv("TriSampNews.csv")

UNItopSortNews = transform(UNItopSortNews,ngrams = reorder(ngrams, -UNItopN))
BItopSortNews = transform(BItopSortNews,ngrams = reorder(ngrams, -BItopN))
TRItopSortNews = transform(TRItopSortNews,ngrams = reorder(ngrams, -TRItopN))

UNItopSortNews = UNItopSortNews[1:20,]
BItopSortNews = BItopSortNews[1:20,]
TRItopSortNews = TRItopSortNews[1:20,]

# break 2

UNInews = ggplot(UNItopSortNews, aes(x=ngrams,y=UNItopN)) +xlab("") + ylab("frequency")
UNInews = UNInews + geom_bar(stat="Identity", fill="seagreen2") 
UNInews = UNInews + theme_light() + theme(plot.margin=unit(c(.25,0,1.3,0), "cm"))
UNInews = UNInews + theme(axis.text.x = element_text(angle = 45, hjust = 1))
UNInews = UNInews + scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,100000, by = 10000),expand = c(0,0))
UNInews

BInews = ggplot(BItopSortNews, aes(x=ngrams,y=BItopN)) +xlab("")+ ylab("")
BInews = BInews +  geom_bar(stat="Identity", fill="seagreen3") 
BInews = BInews + theme_light() + theme(plot.margin=unit(c(.25,0,0.85,0), "cm"))
BInews = BInews + theme(axis.text.x = element_text(angle = 45, hjust = 1))
BInews = BInews +scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,10000, by=1000),expand = c(0,0))
BInews

TRInews = ggplot(TRItopSortNews, aes(x=ngrams,y=TRItopN)) +xlab("")+ ylab("")
TRInews = TRInews +  geom_bar(stat="Identity", fill="seagreen4") 
TRInews = TRInews + theme_light() + theme(plot.margin=unit(c(.25,0.5,0,0), "cm"))
TRInews = TRInews + theme(axis.text.x = element_text(angle = 45, hjust = 1))
TRInews = TRInews +scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,800, by=100),expand = c(0,0))
TRInews

barChartsNews=arrangeGrob(UNInews, BInews, TRInews,nrow = 3)

gridN=grid.draw(barChartsNews)

# ggsave(file="gridN.png",gridN)
# 
# save(barChartsNews,file="grid1.png")
