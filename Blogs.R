setwd("~/Desktop/Courses/Coursera/Data Science Specialization/Capstone/DataSets/final/en_US")
library(ggplot2)
library(quanteda)

# file <- 'en_US.blogs.txt'
# blogs = readLines(file, file.info(file)$size)
# saveRDS(blogs, "blogs.rds")


blogsRDS = readRDS("blogs.rds")
nlines=length(blogsRDS)
blogsRDS <- unlist(blogsRDS)
Encoding(blogsRDS)  <- "UTF-8"

blogsRDS_A <- sample(blogsRDS, size=nlines*0.05, replace=FALSE)
sum(nchar(blogsRDS_A))
length(blogsRDS_A)



UNIgramB=dfm(blogsRDS_A, ngrams = 1,verbose = TRUE, toLower = TRUE,
            removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
            removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
            keptFeatures = NULL)

BIgramB=dfm(blogsRDS_A, ngrams = 2,verbose = TRUE, toLower = TRUE,
           removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
           removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
           keptFeatures = NULL)

TRIgramB=dfm(blogsRDS_A, ngrams = 3,verbose = TRUE, toLower = TRUE,
            removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
            removeTwitter = TRUE, stem = FALSE, ignoredFeatures = NULL,
            keptFeatures = NULL)

UNItopB = topfeatures(UNIgramB, 30)#####
UNItopBDF=data.frame(UNItopB)
UNItopBDF$ngrams <- rownames(UNItopBDF) 

BItopB = topfeatures(BIgramB, 30)#####
BItopBDF=data.frame(BItopB)
BItopBDF$ngrams <- rownames(BItopBDF) 
BItopBDF$ngrams <- gsub('_', ' ', BItopBDF$ngrams)

TRItopB = topfeatures(TRIgramB, 30)#####
TRItopBDF=data.frame(TRItopB)
TRItopBDF$ngrams <- rownames(TRItopBDF) 
TRItopBDF$ngrams <- gsub('_', ' ', TRItopBDF$ngrams)

# write.csv(UNItopBDF, "UniSampBlogs.csv",row.names = FALSE)
# write.csv(BItopBDF, "BiSampBlogs.csv",row.names = FALSE)
# write.csv(TRItopBDF, "TriSampBlogs.csv",row.names = FALSE)

# break 1

UNItopSortBlogs=read.csv("UniSampBlogs.csv")
BItopSortBlogs=read.csv("BiSampBlogs.csv")
TRItopSortBlogs=read.csv("TriSampBlogs.csv")

UNItopSortBlogs = transform(UNItopSortBlogs,ngrams = reorder(ngrams, -UNItopB))
BItopSortBlogs = transform(BItopSortBlogs,ngrams = reorder(ngrams, -BItopB))
TRItopSortBlogs = transform(TRItopSortBlogs,ngrams = reorder(ngrams, -TRItopB))

UNItopSortBlogs = UNItopSortBlogs[1:20,]
BItopSortBlogs = BItopSortBlogs[1:20,]
TRItopSortBlogs = TRItopSortBlogs[1:20,]

# break 2

UNIblog = ggplot(UNItopSortBlogs, aes(x=ngrams,y=UNItopB)) +xlab("") + ylab("frequency")
UNIblog = UNIblog + geom_bar(stat="Identity", fill="skyblue1") 
UNIblog = UNIblog + theme_light()  + theme(plot.margin=unit(c(.25,0,.8,0), "cm"))
UNIblog = UNIblog + theme(axis.text.x = element_text(angle = 45, hjust = 1))
UNIblog = UNIblog + scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,100000, by = 10000),expand = c(0,0))
UNIblog

BIblog = ggplot(BItopSortBlogs, aes(x=ngrams,y=BItopB)) +xlab("")+ ylab("")
BIblog = BIblog +  geom_bar(stat="Identity", fill="skyblue2") 
BIblog = BIblog + theme_light() + theme(plot.margin=unit(c(.25,0,0.45,0), "cm"))
BIblog = BIblog + theme(axis.text.x = element_text(angle = 45, hjust = 1))
BIblog = BIblog +scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,10000, by=1000),expand = c(0,0))
BIblog

TRIblog = ggplot(TRItopSortBlogs, aes(x=ngrams,y=TRItopB)) +xlab("")+ ylab("")
TRIblog = TRIblog +  geom_bar(stat="Identity", fill="skyblue3") 
TRIblog = TRIblog + theme_light() + theme(plot.margin=unit(c(.25,0.5,0,0), "cm"))
TRIblog = TRIblog + theme(axis.text.x = element_text(angle = 45, hjust = 1))
TRIblog = TRIblog +scale_x_discrete(expand = c(0,0))+scale_y_continuous(breaks = seq(0,800, by=100),expand = c(0,0))
TRIblog

barChartsBlogs=arrangeGrob(UNIblog, BIblog, TRIblog,nrow = 3, ncol = 1,top = textGrob("Blogs"))

gridB=grid.draw(barChartsBlogs)
