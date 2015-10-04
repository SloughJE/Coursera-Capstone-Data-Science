setwd("~/Desktop/Courses/Coursera/Data Science Specialization/Capstone/DataSets/final/en_US")

library(gridExtra)
library(grid)
library(ggplot2)


barCharts1=arrangeGrob(UNItwit, BItwit, TRItwit, UNIblog, BIblog, TRIblog, UNInews, BInews, TRInews,
nrow = 3, ncol = 3,left=textGrob(c("Tweets","Blogs","News"),y = c(.86,.54,.215) ,gp = gpar(fontsize=c(18,18,18), face = c("bold","bold","bold"),col=c("indianred3","skyblue3","seagreen4"))))

grid1=grid.draw(barCharts1)

ggsave(filename="Individual.png",barCharts1,width = 40,height = 25, units = "cm")
