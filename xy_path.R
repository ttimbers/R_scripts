##load MWT data
##set working directory to strain you want to analyze
strain.1 <- read.table("merged.file")

##split up column V1 into identifier and time
library(stringr)
df_V1 <- strain.1$V1
plate_tag  <- "/[0-9_]{1,}"
plate_names  <- str_extract(df_V1, plate_tag)
plate_names  <- sub("/", "", plate_names)
time_tag  <- ":[0-9.]{1,}"
time  <- str_extract(df_V1, time_tag)
time  <- sub(":", "", time)
df.strain.1 <- cbind(plate_names, time, strain.1[,c(2:6)])
strain.1 <- df.strain.1
rm(df.strain.1, df_V1, plate_names, plate_tag, time, time_tag)

##rename columns  
colnames(strain.1) <- c("plate", "time", "ID", "speed", "bias", "x", "y")

##remove NANs from data
strain.1.speed <- strain.1
strain.1.speed <- strain.1.speed[complete.cases(strain.1.speed), ]

##bin time into intervals
strain.1.speed$time <- as.numeric(levels(strain.1.speed$time))[strain.1.speed$time]
c1 <- cut(strain.1.speed$time, breaks = seq(0, 600, by = 0.08))
strain.1.speed$time <- c1

##make data wide
library(reshape2)
##the following command caused R to crash... oops... not sure why yet...
strain.1.wide <- dcast(strain.1.speed$speed, strain.1.speed$plate, strain.1.speed$x, strain.1.speed$y + strain.1.speed$ID ~ time, fun.aggregate = mean, value.var="strain.1.speed")
##strain.1.subset <- strain.1.wide[,1078:1827] ##90s to 150s
strain.1.subset <- strain.1.wide[,689:1078] ##60s to 90s
strain.1.subset <- strain.1.subset[complete.cases(strain.1.subset), ]

##now plot heat maps
library(pheatmap)   
##library(gplots)

strain.1.subset_matrix <- data.matrix(strain.1.subset)

#create the breaks
bk2 = unique(c(seq(-3, -0.011, length=8), seq(-0.01, 0.01, length=3), seq(0.011,6.5, length=12)))

#set different color vectors for each interval
col1 = colorRampPalette(c("blue","white"))(8) #set the order of greys
col2 = colorRampPalette(c("white"))(3)
col3 = colorRampPalette(c("lightgoldenrodyellow", "orangered",'red3'))(11)
colors2 <- c(col1, col2, col3)

##draw heatmap
strain.1_pheatmap <- pheatmap(strain.1.subset_matrix, color=colors2, breaks=bk2, main="VH2236", show_rownames=F, show_colnames=F, cluster_cols=F, cluster_rows = F, border_color=NA, cellheight=2, fontsize=12)

##save heatmap
strain.1_pheatmap <- pheatmap(strain.1.subset_matrix, color=colors2, breaks=bk2, main="VH2236", show_rownames=F, show_colnames=F, cluster_cols=F, cluster_rows = F, border_color=NA, cellheight=2, fontsize=12, width=7, height=5, filename="/Users/michelleroux/Documents/Jesse/2015-01-19/Individual_worm_analysis/VH2236_heatmap.pdf")

##clear work environment
rm(list=ls())







