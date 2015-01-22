##load MWT data
setwd("enter path")
N2 <- read.table("merged.file")

##split up column V1 into identifier and time
library(stringr)
df_V1 <- N2$V1
plate_tag  <- "/[0-9_]{1,}"
plate_names  <- str_extract(df_V1, plate_tag)
plate_names  <- sub("/", "", plate_names)
time_tag  <- ":[0-9.]{1,}"
time  <- str_extract(df_V1, time_tag)
time  <- sub(":", "", time)
df.N2 <- cbind(plate_names, time, N2[,c(2:7)])
N2 <- df.N2
rm(df.N2)

##clean up the workspace
rm(time, time_tag, plate_names, plate_tag, df_V1)

##rename columns  
colnames(N2) <- c("plate", "time", "ID", "speed", "bias", "morphwidth", "midline", "area")

##use bias to make speed go into negatives when going backwards (bias gives dominant direction =1 
##, and opposite direction =-1, no movement = 0)
N2$speed[which(N2$bias==-1)]  <- N2$speed[which(N2$bias==-1)]*-1

##remove NANs from data
N2.speed <- data.frame(N2$plate, N2$time, N2$ID, N2$speed)
N2.speed <- N2.speed[complete.cases(N2.speed), ]

##bin time into intervals
N2.speed$N2.time <- as.numeric(levels(N2.speed$N2.time))[N2.speed$N2.time]
c1 <- cut(N2.speed$N2.time, breaks = seq(0, 300, by = 0.08))
N2.speed$N2.time <- c1

##make data wide
library(reshape2)
N2.wide <- dcast(N2.speed, N2.plate + N2.ID ~ N2.time, fun.aggregate = mean, value.var="N2.speed")
##N2.subset <- N2.wide[,1523:3922]
##N2.subset <- N2.wide[,763:1763] ##works with breaks =  seq(0, 300, by = 0.1)
##N2.subset <- N2.wide[,1078:1578] ##90s to 130s
##N2.subset <- N2.wide[,1078:1702] ##90s to 140s
N2.subset <- N2.wide[,1078:1827] ##90s to 150s
N2.subset <- N2.subset[complete.cases(N2.subset), ]

##now plot heat maps
library(pheatmap)   
library(gplots)

##plot heatmap for each unique strain
N2.subset_matrix <- data.matrix(N2.subset)

#create the breaks
bk2 = unique(c(seq(-3, -0.011, length=8), seq(-0.01, 0.01, length=3), seq(0.011,6.5, length=12)))

#set different color vectors for each interval
col1 = colorRampPalette(c("blue","white"))(8) #set the order of greys
col2 = colorRampPalette(c("white"))(3)
col3 = colorRampPalette(c("lightgoldenrodyellow", "orangered",'red3'))(11)
colors2 <- c(col1, col2, col3)

#draw heatmap
N2_pheatmap <- pheatmap(N2.subset_matrix, color=colors2, breaks=bk2, main="wild-type", show_rownames=F, show_colnames=F, cluster_cols=F, cluster_rows = F, border_color=NA, cellheight=2, fontsize=12)

#save heatmap
N2_pheatmap <- pheatmap(N2.subset_matrix, color=colors2, breaks=bk2, main="wild-type", show_rownames=F, show_colnames=F, cluster_cols=F, cluster_rows = F, border_color=NA, cellheight=2, fontsize=12, width=7, height=5, filename="heatmap.pdf")

##rm(list=ls())







